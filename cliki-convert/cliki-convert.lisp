;;; WARNING!
;;; use "convmv -f latin1 -t utf8 --nosmart --replace --notest *"
;;; for prepare old cliki pages

(in-package #:cliki2.converter)

(defun convert-article-revision (content)
  (flet ((fixup-tag (regex str fixup)
           (ppcre:regex-replace-all regex str
                                    (lambda (match register)
                                      (declare (ignore match))
                                      (funcall fixup register))
                                    :simple-calls t))
         (fmt-category (category) (format nil "/(~A)" category))
         (fmt-package (url) (format nil "_P(~A)" url))
         (fmt-hyperspec (symbol) (format nil "_H(~A)" symbol)))
    (loop for regex in '(":\\(CLHS \"(.*?)\"\\)" ":\\(package \"(.*?)\"\\)" "/\\(\"(.*?)\".*?\\)")
          for action in (list #'fmt-hyperspec #'fmt-package #'fmt-category)
          do (setf content (fixup-tag regex content action)))
    content))

(defstruct (edit (:type list)) date article-name author summary)

(defun load-edits (old-article-dir)
  (with-open-file (s (merge-pathnames "admin/recent-changes.dat"
                                      old-article-dir)
                     :external-format :latin1)
    (loop for x = (read s nil) while x collect x)))

(defun edits-for-article (article-name edits)
  (remove-if (lambda (edit)
               (not (equal article-name (edit-article-name edit))))
             edits))

(defun unknown-edit (date article-name)
  (list date article-name "no edit summary available" "import from CLiki"))

(defun match-edits-to-files (article-name edits files)
  (let ((num-edits (length edits))
        (num-files (length files)))
    (cond ((> num-edits num-files)
           (subseq edits (- num-edits num-files)))
          ((= num-edits 0)
           (let ((timestamp-skew 0))
             (loop for file in files collecting
                  (unknown-edit (+ (incf timestamp-skew 60)
                                   (file-write-date file))
                                article-name))))
          (t (let ((edit-date (edit-date (car edits))))
               (append (reverse
                        (loop repeat (- num-files num-edits)
                           do (decf edit-date 60)
                           collect (unknown-edit edit-date article-name)))
                       edits))))))

(defun delete-article? (revision content all-revisions)
  (and (eq revision (car (last all-revisions)))
       (search "*(delete this page)" content :test #'char-equal)))

(defun make-revision (article revision-path authorship summary all-revisions)
  (let ((content (convert-article-revision
                  (alexandria:read-file-into-string
                   revision-path :external-format :latin1))))
    (or (when (delete-article? revision-path content all-revisions)
          (cliki2::latest-revision
           (cliki2::toggle-delete (bknr.datastore:store-object-id article)
                                  authorship)))
        (cliki2::add-revision article summary content authorship))))

(defun import-revisions (account article edits revision-paths)
  (loop for path in revision-paths
        for edit in edits do
       (let* ((date (edit-date edit))
              (revision
               (make-revision article
                              path
                              (list :author    account
                                    :author-ip "0.0.0.0"
                                    :date      date)
                              (format nil "~@[~A ~]~@[(~A)~]"
                                      (edit-summary edit) (edit-author edit))
                              revision-paths))
              (unix-date (local-time:timestamp-to-unix
                          (local-time:universal-to-timestamp date))))
         (sb-posix:utimes (cliki2::revision-path revision)
                          unix-date unix-date))))

(defun decode-article-name (file-name)
  (cliki2::cut-whitespace
   (hunchentoot:url-decode
    (substitute #\% #\= file-name)
    hunchentoot::+latin-1+)))

(defun load-old-articles (old-article-dir)
  "WARNING: This WILL blow away your old store."
  (close-store)

  (iter (for item in '("articles/" "store/"))
        (for path = (merge-pathnames item cliki2::*datadir*))
        (cl-fad:delete-directory-and-files path :if-does-not-exist :ignore)
        (ensure-directories-exist path))

  (open-store (merge-pathnames "store/" cliki2::*datadir*))

  (let ((old-articles (make-hash-table :test 'equalp)) ;; case insensitive
        (all-edits (load-edits old-article-dir)))
    (dolist (file (remove-if #'cl-fad:directory-pathname-p
                             (cl-fad:list-directory old-article-dir)))
      (let ((article-name (decode-article-name (pathname-name file))))
        (setf (gethash article-name old-articles)
              (merge 'list
                     (gethash article-name old-articles)
                     (list file)
                     #'<
                     :key (lambda (x)
                            (parse-integer (or (pathname-type x) "0")
                                           :junk-allowed t))))))

    ;; import into store
    (let ((cliki-import-user (make-instance 'cliki2::account
                                            :name "CLiki-importer"
                                            :email "noreply@cliki.net"
                                            :password-salt "000000"
                                            :password-digest "nohash")))
      (loop for article-title being the hash-key of old-articles
            for revision-paths being the hash-value of old-articles do
           (import-revisions
            cliki-import-user
            (make-instance 'cliki2::article :title article-title)
            (match-edits-to-files article-title
                                  (edits-for-article article-title all-edits)
                                  revision-paths)
            revision-paths))))

  (cliki2::init-recent-revisions)
  (snapshot))

;; (load-old-articles #P"/home/viper/tmp/cliki-virgin/")
