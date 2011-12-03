;;; WARNING!
;;; use "convmv -f latin1 -t utf8 --nosmart --replace --notest *"
;;; for prepare old cliki pages

(in-package #:cliki2.converter)

(defun read-file (path)
  (alexandria:read-file-into-string path :external-format :latin1))

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

(defun delete-article (article authorship)
  (cliki2::latest-revision
   (cliki2::toggle-delete (bknr.datastore:store-object-id article) authorship)))

(defun import-revision (article content authorship)
  (cliki2::add-revision article "import from CLiki" content authorship))

(defun import-revisions (account article revision-paths)
  (let ((timestamp-skew 0))
    (dolist (path revision-paths)
      (let* ((date       (+ (incf timestamp-skew) (file-write-date path)))
             (unix-date  (local-time:timestamp-to-unix
                          (local-time:universal-to-timestamp date)))
             (content    (convert-article-revision (read-file path)))
             (authorship (list :author    account
                               :author-ip "0.0.0.0"
                               :date      date))
             (revision   (or (when (and (eq path (car (last revision-paths)))
                                        (search "*(delete this page)"
                                                content :test #'char-equal))
                               (delete-article article authorship))
                             (import-revision article content authorship))))
        (sb-posix:utimes (cliki2::revision-path revision)
                         unix-date unix-date)))))

(defun load-old-articles (old-article-dir)
  "WARNING: This WILL blow away your old store."
  (close-store)

  (iter (for item in '("articles/" "store/"))
        (for path = (merge-pathnames item cliki2::*datadir*))
        (cl-fad:delete-directory-and-files path :if-does-not-exist :ignore)
        (ensure-directories-exist path))

  (open-store (merge-pathnames "store/" cliki2::*datadir*))

  (let ((old-articles (make-hash-table :test 'equalp))) ;; case insensitive
    (dolist (file (cl-fad:list-directory old-article-dir))
      (let ((file-name (cliki2::cut-whitespace
                        (hunchentoot:url-decode
                         (substitute #\% #\= (pathname-name file))
                         hunchentoot::+latin-1+))))
        (setf (gethash file-name old-articles)
              (merge 'list
                     (list file)
                     (gethash file-name old-articles)
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
            revision-paths))))

  (cliki2::init-recent-revisions)
  (snapshot))

;; (load-old-articles "/home/viper/tmp/cliki/")
