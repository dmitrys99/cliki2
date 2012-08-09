;;; WARNING!
;;; use "convmv -f latin1 -t utf8 --nosmart --replace --notest *"
;;; for prepare old cliki pages

(in-package #:cliki2.converter)

(defun convert-content (content)
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
        (num-files (length files))
        (edits (let ((timestamp-skew 0))
                 (mapcar (lambda (edit)
                           (cons (+ (edit-date edit) (incf timestamp-skew))
                                 (cdr edit) ))
                         edits))))
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

(defun copy-revision-content (revision old-file)
  (let ((new-file (ensure-directories-exist (cliki2::revision-path revision))))
    (alexandria:write-string-into-file
     (convert-content
      (alexandria:read-file-into-string old-file :external-format :latin1))
     new-file
     :external-format :utf8)
    (let ((unix-date (local-time:timestamp-to-unix
                      (local-time:universal-to-timestamp
                       (cliki2::revision-date revision)))))
      (sb-posix:utimes new-file unix-date unix-date)))
  revision)

(defun import-revisions (article-title edits old-files)
  (loop for old-file in old-files
        for edit     in edits
        collect (copy-revision-content
                 (cliki2::make-revision
                  :parent-title  article-title
                  :revision-date (edit-date edit)
                  :summary       (format
                                  nil "~@[~A ~]~@[(~A)~]"
                                  (hunchentoot:escape-for-html (edit-summary edit))
                                  (edit-author edit))
                  :author-name   "cliki-import"
                  :author-ip     "0.0.0.0")
                 old-file)))

(defun write-article (name revisions)
  (cliki2::write-to-file (cliki2::file-path 'cliki2::article name)
                         (list name (reverse revisions))))

(defun decode-article-name (file-name)
  (cliki2::cut-whitespace
   (hunchentoot:url-decode
    (substitute #\% #\= file-name)
    hunchentoot::+latin-1+)))

(defun load-old-articles (wiki-home old-article-dir)
  (let ((cliki2::*wiki* (cliki2::make-wiki-struct :home-directory wiki-home))
        (old-articles (make-hash-table :test 'equalp)) ;; case insensitive
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

    (loop for article-title being the hash-key of old-articles
          for revision-paths being the hash-value of old-articles do
         (write-article
          article-title
          (import-revisions
           article-title
           (match-edits-to-files article-title
                                 (edits-for-article article-title all-edits)
                                 revision-paths)
           revision-paths)))))

;; (cliki2.converter::load-old-articles "/home/viper/tmp/cliki-converted/" "/home/viper/tmp/cliki-august/")
