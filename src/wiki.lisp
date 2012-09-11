(in-package #:cliki2)
(in-readtable cliki2)

(defstruct (wiki (:conc-name nil) (:constructor make-wiki-struct))
  ;; config
  home-directory
  wiki-name
  description
  password-reminder-email-address

  ;; locks
  (update-lock     (make-lock "update lock"))
  (data-lock       (make-lock "data lock"))
  (index-lock      (make-lock "index lock"))
  (session-lock    (make-lock "session lock"))

  ;; data
  (accounts        (make-hash-table :test 'equal))
  (articles        (make-hash-table :test 'equal))
  (blacklist       (make-hash-table :test 'equal))

  ;; cache
  (article-cache   (make-hash-table :test 'equal))
  (last-timestamp  0)

  ;; indexes
  (topic-index     (make-hash-table :test 'equal)) ;; article name
  (search-index    (make-hash-table :test 'equal)) ;; article name
  (author-index    (make-hash-table :test 'equal)) ;; revision obj
  (recent-changes  ())                             ;; revision obj

  ;; sessions
  (sessions        (make-hash-table :test 'equal)))

;;; access

(defun find-account (name)
  (with-lock-held ((data-lock *wiki*))
    (gethash (cut-whitespace name) (accounts *wiki*))))

(defun banned? (account-name)
  (with-lock-held ((data-lock *wiki*))
    (gethash account-name (blacklist *wiki*))))

(defun get-blacklist ()
  (with-lock-held ((data-lock *wiki*))
    (alexandria:hash-table-keys (blacklist *wiki*))))

(define-condition cannot-find-article-error (error)
  ((title   :initarg :title)
   (link    :initarg :link)
   (referer :initarg :referer))
  (:report (lambda (e stream)
             (format stream
                     "Cannot find article entitled '~A' (link ~A refered by ~A)"
                     (slot-value e 'title)
                     (slot-value e 'link)
                     (slot-value e 'referer)))))

(defun find-article (title &key error)
  (let ((article (with-lock-held ((data-lock *wiki*))
                   (gethash (canonicalize title) (articles *wiki*)))))
    (if (and error (not article))
        (error 'cannot-find-article-error
               :title   title
               :link    (request-uri*)
               :referer (referer))
        article)))

(defun get-all-articles (filter)
  (sort
   (mapcar #'article-title
           (remove-if-not
            filter
            (with-lock-held ((data-lock *wiki*))
              (alexandria:hash-table-values (articles *wiki*)))))
   #'string-lessp))

(defun edits-by-author (name)
  (with-lock-held ((index-lock *wiki*))
    (gethash name (author-index *wiki*))))

(defun cached-content (title)
  (with-lock-held ((data-lock *wiki*))
    (gethash (canonicalize title) (article-cache *wiki*))))

(defun get-recent-changes ()
  (with-lock-held ((index-lock *wiki*))
    (recent-changes *wiki*)))

;;; paths

(defun wiki-path (path)
  (merge-pathnames path (home-directory *wiki*)))

(defun file-path (type id)
  (let ((id (uri-encode (if (eq type 'article) (canonicalize id) id))))
    (wiki-path (ecase type
                 (account  #?"accounts/${ id }")
                 (article  #?"articles/${ id }/article")))))

;;; storage

(defun write-to-file (to-file obj
                      &optional (tmpdir (wiki-path "tmp/")))
  (let* ((content (if (stringp obj)
                      obj
                      (with-standard-io-syntax
                        (let ((*readtable*
                               (named-readtables:find-readtable :common-lisp)))
                          (prin1-to-string obj)))))
         (buffer (flexi-streams:string-to-octets content :external-format :utf-8))
         tmp-file)
    (loop while (probe-file
                 (setf tmp-file
                       (ensure-directories-exist
                        (merge-pathnames
                         (format nil "~A.~A"
                                 (file-namestring to-file)
                                 (random 10000))
                         tmpdir)))))
    (with-open-file (out tmp-file
                         :direction         :output
                         :if-exists         :error
                         :if-does-not-exist :create
                         :element-type      '(unsigned-byte 8))
      (write-sequence buffer out)
      (finish-output out))
    (unless (= (length buffer)
               (with-open-file (x tmp-file :element-type '(unsigned-byte 8))
                 (file-length x)))
      (error "Length mismatch in temporary file, aborting write"))
    ;; CL rename-file doesn't specify what to do if file exists
    (osicat-posix:rename (namestring tmp-file)
                         (namestring (ensure-directories-exist to-file)))))

;;; create and delete

(defun wiki-new (what obj)
  (with-lock-held ((update-lock *wiki*))
     ;; protocol: car is id
    (write-to-file (file-path what (car obj)) obj)
    (with-lock-held ((data-lock *wiki*))
      (ecase what
        (article (setf (gethash (canonicalize (article-title obj))
                                (articles *wiki*))
                       obj))
        (account (setf (gethash (account-name obj) (accounts *wiki*))
                       obj)))))
  obj)

(defun permadelete (title)
  (let ((title (canonicalize title)))
    (with-lock-held ((update-lock *wiki*))
      (let ((article-revisions (revisions (find-article title))))
        (flet ((remove-article-revisions (list)
                 (remove-if (lambda (x)
                              (member x article-revisions))
                            list)))
          (with-lock-held ((index-lock *wiki*))
            (setf (recent-changes *wiki*) (remove-article-revisions
                                           (recent-changes *wiki*)))
            ;; topic and search index should be clear,
            ;; but need to clear out author-index
            (loop for x being the hash-key of (author-index *wiki*)
                  using (hash-value old-rs) do
                  (setf (gethash x (author-index *wiki*))
                        (remove-article-revisions old-rs))))))
      (with-lock-held ((data-lock *wiki*))
        (remhash title (articles *wiki*)))
      (let* ((esc-title (uri-encode title))
             (deleted-path (wiki-path #?"deleted-articles/${ esc-title }/")))
        ;; rename-file sucks balls if file exists
        (cl-fad:delete-directory-and-files deleted-path :if-does-not-exist :ignore)
        (rename-file (wiki-path #?"articles/${ esc-title }/")
                     (ensure-directories-exist deleted-path))))))

;;; update

(defmacro update-account (account &rest updates)
  (let ((copy (gensym))
        (name (gensym)))
    `(with-lock-held ((update-lock *wiki*))
       (let* ((,name (account-name ,account))
              (,copy (copy-account (find-account ,name))))
         (setf ,@(loop for (accessor newval) on updates by #'cddr
                       collect `(,accessor ,copy) collect newval))
         (write-to-file (file-path 'account ,name) ,copy)
         (with-lock-held ((data-lock *wiki*))
           (setf (gethash ,name (accounts *wiki*)) ,copy))))))

(defun update-blacklist (account-name banned?)
  (with-lock-held ((update-lock *wiki*))
    (with-lock-held ((data-lock *wiki*))
      (if banned?
          (setf (gethash account-name (blacklist *wiki*)) t)
          (remhash account-name (blacklist *wiki*))))
    (write-to-file (wiki-path "blacklist") (get-blacklist))))

;;; revisions

(defun record-revision (revision content)
  (let ((title (canonicalize (parent-title revision))))
    (with-lock-held ((update-lock *wiki*))
      (when (<= (revision-date revision) (last-timestamp *wiki*))
        (warn "Clock seems to be running backwards")
        (setf (revision-date revision) (1+ (last-timestamp *wiki*))))
      (save-revision-content revision content)
      (setf (last-timestamp *wiki*) (revision-date revision))
      (let ((article (copy-article (find-article title)))
            old-content)
        (push revision (revisions article))
        (write-to-file (file-path 'article title) article)
        (with-lock-held ((data-lock *wiki*))
          (setf old-content (gethash title (article-cache *wiki*) "")
                (gethash title (articles *wiki*))      article
                (gethash title (article-cache *wiki*)) content))
        (with-lock-held ((index-lock *wiki*))
          (push revision (gethash (author-name revision) (author-index *wiki*)))
          (let ((recent-changes (cons revision (recent-changes *wiki*))))
            (setf (recent-changes *wiki*)
                  (subseq recent-changes 0 (min 100 (length recent-changes))))))
        (reindex-article (article-title article) content old-content))))
  revision)

(defun revision-path (revision)
  (wiki-path #?"articles/${ (uri-encode (canonicalize (parent-title revision))) }/revisions/${ (revision-date revision) }"))

(defun revision-content (revision)
  (let ((revision-path (revision-path revision)))
    (if (probe-file revision-path)
        (alexandria:read-file-into-string revision-path :external-format :utf-8)
        (error "Cannot find revision content file of revision dated ~A of article ~A (path ~A)"
               (revision-date revision) (parent-title revision) revision-path))))

(defun save-revision-content (revision content) ;; not locked
  (let ((path (revision-path revision)))
    (if (probe-file path)
        (error "Revision dated ~A of article ~A already exists, aborting write"
               (revision-date revision) (parent-title revision))
        (write-to-file path content))))

;;; indexing

(defun articles-by-search-word (word)
  (with-lock-held ((index-lock *wiki*))
    (gethash word (search-index *wiki*))))

(defun articles-by-topic (topic)
  (with-lock-held ((index-lock *wiki*))
    (gethash (canonicalize topic) (topic-index *wiki*))))

(defun reindex-article (title new-content old-content)
  (let ((title-words (words title)))
    (flet ((reindex (index new old)
             (with-lock-held ((index-lock *wiki*))
               (dolist (x (set-difference old new :test #'string=))
                 (setf (gethash x index)
                       (remove title (gethash x index) :test #'string=)))
               (dolist (x (set-difference new old :test #'string=))
                 (setf (gethash x index)
                       (merge 'list (list title) (gethash x index)
                              #'string-lessp)))))
           (words-for-search (content)
             (awhen (words content)
               (union title-words it :test #'string=))))
      (reindex (topic-index *wiki*)
               (topics new-content) (topics old-content))
      (reindex (search-index *wiki*)
               (words-for-search new-content) (words-for-search old-content)))))

(defun init-recent-changes ()
  (let ((recent-changes
         (reduce (lambda (r1s r2s)
                   (let ((merged
                          (merge 'list (copy-list r1s) (copy-list r2s)
                                 #'> :key #'revision-date)))
                     (subseq merged 0 (min 100 (length merged)))))
                 (with-lock-held ((data-lock *wiki*))
                   (alexandria:hash-table-values (articles *wiki*)))
                 :key #'revisions)))
    (with-lock-held ((index-lock *wiki*))
      (setf (recent-changes *wiki*) recent-changes))))

;;; lock file

(defun get-directory-lock (dir)
  (let ((pulse    1)
        (lockfile (merge-pathnames "lock" dir))
        nonce)
    (flet ((take-lock ()
             (bt:make-thread
              (lambda ()
                (loop
                   (let ((nonce1 (when (probe-file lockfile)
                                   (read-file lockfile))))
                     (if (and nonce (not (equal nonce nonce1)))
                         ;; someone else is writing to lock file
                         ;;(osicat-posix:exit 1)
                         (error "Nonces not equal ~A ~A" nonce nonce1)
                         (with-open-file (out lockfile
                                              :direction :output
                                              :if-exists :supersede)
                           (prin1 (setf nonce (random most-positive-fixnum))
                                  out))))
                   (sleep (+ pulse (random 1.0))))))))
     (if (probe-file lockfile)
         (let ((nonce (read-file lockfile)))
           (sleep (+ (* pulse 2) (random 3.0)))
           (if (equal nonce (read-file lockfile))
               (take-lock)
               (error "Lock file for ~A held by someone else, quitting" dir)))
         (take-lock)))))

;;; load

(defun read-file (file)
  (with-open-file (in file :direction         :input
                           :if-does-not-exist :error
                           :external-format   :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*readtable* (named-readtables:find-readtable :common-lisp)))
        (read in)))))

(defun load-wiki-article (path)
  (let* ((article (read-file (merge-pathnames "article" path)))
         (title   (canonicalize (article-title article)))
         (rev     (car (revisions article)))
         (content (revision-content rev)))
    (setf (gethash title (articles *wiki*))      article
          (gethash title (article-cache *wiki*)) content
          (last-timestamp *wiki*)                (max (last-timestamp *wiki*)
                                                      (revision-date rev)))
    (dolist (r (revisions article))
      (push r (gethash (author-name r) (author-index *wiki*))))
    (reindex-article (article-title article) content "")))

(defun load-wiki (*wiki*)
  (get-directory-lock (home-directory *wiki*))

  ;; set up empty file for diff
  (open (wiki-path "empty_file") :direction :probe :if-does-not-exist :create)

  (map nil #'load-wiki-article (cl-fad:list-directory (wiki-path "articles/")))

  (loop for author being the hash-key of (author-index *wiki*)
        using (hash-value author-revisions) do
        (setf (gethash author (author-index *wiki*))
              (sort author-revisions #'> :key #'revision-date)))

  (dolist (afile (cl-fad:list-directory (wiki-path "accounts/")))
    (let ((account (read-file afile)))
      (setf (gethash (account-name account) (accounts *wiki*)) account)))

  (dolist (banned (read-file (wiki-path "blacklist")))
    (setf (gethash banned (blacklist *wiki*)) t))

  (init-recent-changes))

;;; start

(defun make-wiki (name description homedir email)
  (let ((wiki (make-wiki-struct
               :home-directory                  homedir
               :wiki-name                       name
               :description                     description
               :password-reminder-email-address email)))
    (load-wiki wiki)
    wiki))
