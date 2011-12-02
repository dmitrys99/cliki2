(in-package #:cliki2)
(in-readtable cliki2)

(defclass deleted-article (store-object)
  ((title            :initarg       :title
                     :reader        title)
   (canonical-title  :initarg       :canonical-title
                     :reader        canonical-title
                     :index-type    string-unique-index
                     :index-reader  deleted-article-with-title)
   (revisions        :initarg       :revisions
                     :reader        revisions))
  (:metaclass persistent-class))

(defun find-deleted-article (title)
  (deleted-article-with-title (canonicalize title)))

(defun find-article-any (title)
  (or (find-article title) (find-deleted-article title)))

(defmethod pprint-article-summary-li ((article deleted-article) separator)
  #H[<li>] (pprint-article-link (title article)) #H[ ${separator} </li>])

(defun %move-revisions (old-article new-article)
  (dolist (r (revisions old-article))
    (setf (slot-value r 'article) new-article)))

(defclass revision-undelete (revision) ()
  (:metaclass persistent-class))

(deftransaction tx-delete-article (article)
  (let ((deleted (make-instance 'deleted-article
                                :title           (title article)
                                :canonical-title (canonical-title article)
                                :revisions       (revisions article))))
    (%move-revisions article deleted)))

(defun delete-article (article &rest args)
  (prog1 (apply #'add-revision article "Deleted article" "" args)
    (tx-delete-article article)
    (delete-object article)))

(deftransaction tx-undelete-article (deleted)
  (let ((article (make-instance 'article :title (title deleted))))
    (setf (slot-value article 'revisions) (revisions deleted))
    (%move-revisions deleted article)
    article))

(defun undelete-article (deleted)
  (let ((article (tx-undelete-article deleted)))
    (add-revision article "Undeleted article"
                  (revision-content (second (revisions article)))
                  :revision-type 'revision-undelete)
    (delete-object deleted)
    article))

(defhandler /site/delete (title)
  (awhen (and (not (youre-banned?))
              (not (find-deleted-article title))
              (find-article title))
    (delete-article it))
  (link-to title))

(deftransaction tx-permadelete (deleted-article)
  (dolist (r (revisions deleted-article))
    (setf *recent-revisions* (remove r *recent-revisions*))
    (delete-object r)))

(defhandler /site/permadelete (title)
  (awhen (and (not (youre-banned?))
               (account-is? *account* :moderator :administrator)
               (find-deleted-article title))
    (tx-permadelete it)
    (delete-object it))
  #/)
