(in-package #:cliki2)
(in-readtable cliki2)

(defclass deleted-article (proto-article)
  ((canonical-title :index-type   string-unique-index
                    :index-reader deleted-article-with-title))
  (:metaclass persistent-class))

(defun find-deleted-article (title)
  (deleted-article-with-title (canonicalize title)))

(defun find-article-any (title)
  (or (find-article title) (find-deleted-article title)))

(defmethod pprint-article-summary-li ((article deleted-article) separator)
  #H[<li>] (pprint-article-link (title article)) #H[ ${separator} </li>])

(defclass revision-undelete (revision) ()
  (:metaclass persistent-class))

(defun %move-revisions (old-article new-article)
  (dolist (r (revisions old-article))
    (setf (slot-value r 'article) new-article)))

(deftransaction toggle-delete (old-article-id authorship)
  (let* ((old-article (store-object-with-id old-article-id))
         (new-article (make-instance (if (typep old-article 'article)
                                         'deleted-article
                                         'article)
                                     :title     (title old-article)
                                     :revisions (revisions old-article))))
    ;; use add-revision with empty content to de-index deleted article
    (if (typep old-article 'article)
        (push (add-revision old-article "Deleted article" "" authorship)
              (revisions new-article))
        (add-revision new-article "Undeleted article"
                      (revision-content (second (revisions old-article)))
                      authorship 'revision-undelete))
    (dolist (r (revisions old-article))
      (setf (slot-value r 'article) new-article))
    (delete-object old-article)
    new-article))

(defhandler /site/delete (title)
  (awhen (and (not (youre-banned?))
              (not (find-deleted-article title))
              (find-article title))
    (toggle-delete (store-object-id it) (connection-authorship-info)))
  (link-to title))

(deftransaction permadelete (deleted-article-id)
  (let ((deleted-article (store-object-with-id deleted-article-id)))
    (dolist (r (revisions deleted-article))
      (setf *recent-revisions* (remove r *recent-revisions*))
      (delete-object r))
    (delete-object deleted-article)))

(defhandler /site/permadelete (title)
  (awhen (and (not (youre-banned?))
               (account-is? *account* :moderator :administrator)
               (find-deleted-article title))
    (permadelete (store-object-id it)))
  #/)
