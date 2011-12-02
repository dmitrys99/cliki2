(in-package #:cliki2)
(in-readtable cliki2)

(defun cut-whitespace (str)
  (string-trim #(#\Space #\Tab #\Newline #\Return)
               (ppcre:regex-replace-all "\\s+" str " ")))

;;; article categories

(defun canonicalize (title)
  (string-downcase (cut-whitespace title)))

(defun category-keyword (category-title)
  (intern (canonicalize category-title) '#:cliki2.categories))

(defun content-categories (content)
  (let (categories)
    (ppcre:do-register-groups (category) (#?/\*\(([^\)]*)\)/ content)
      (pushnew category categories :test #'string-equal))
    categories))

;;; article

(defclass article (store-object)
  ((title            :initarg       :title
                     :reader        title)
   (canonical-title  :reader        canonical-title
                     :index-type    string-unique-index
                     :index-reader  article-with-canonical-title)
   (revisions        :initform      ()
                     :accessor      revisions)
   (category-list    :initform      ()
                     :accessor      category-list
                     :index-type    hash-list-index
                     :index-reader  articles-with-category)
   (cached-content   :initform      ""
                     :accessor      cached-content))
  (:metaclass persistent-class))

(defmethod shared-initialize :after ((article article) slot-names &key &allow-other-keys)
  (with-slots (title canonical-title) article
    (setf title (cut-whitespace title)
          canonical-title (string-downcase title))))

(defun find-article (title)
  (article-with-canonical-title (canonicalize title)))

(defun latest-revision (article)
  (car (revisions article)))

(defun article-description (article)
  (let ((c (cached-content article)))
    (subseq c 0 (ppcre:scan "\\.(?:\\s|$)|\\n|$" c))))

(defmethod link-to ((article store-object))
  (link-to (canonical-title article)))

(defmethod link-to ((article-titled string))
  #?[/${(uri-encode (cut-whitespace article-titled))}])

(defun %print-article-link (title class)
  #H[<a href="${(link-to title)}" class="${class}">${title}</a>])

(defun pprint-article-link (title)
  (%print-article-link title (if (find-article title) "internal" "new")))

(defun pprint-category-link (title)
  (%print-article-link title "category"))

;;; revisions

(defclass revision (store-object)
  ((article    :initarg      :article
               :reader       article)
   (author     :initarg      :author
               :reader       author
               :index-type   hash-index
               :index-reader revisions-by-author)
   (author-ip  :initarg      :author-ip
               :reader       author-ip)
   (date       :initarg      :date
               :reader       date)
   (summary    :initarg      :summary
               :reader       summary))
  (:metaclass persistent-class))

(defun revision-path (revision)
  #?"${*datadir*}articles/${(uri-encode (canonical-title (article revision)))}/${(date revision)}")

(defun revision-content (revision)
  (alexandria:read-file-into-string (revision-path revision)))

(defun add-revision (article summary content &key
                     (author (or *account*
                                 (get-anonymous-account (real-remote-addr))))
                     (author-ip (real-remote-addr))
                     (date (get-universal-time))
                     (revision-type 'revision))
  (let ((new-revision (make-instance revision-type
                                     :article    article
                                     :author     author
                                     :author-ip  author-ip
                                     :date       date
                                     :summary    summary))
        (content (remove #\Return content)))
    (alexandria:write-string-into-file
     content
     (ensure-directories-exist (revision-path new-revision))
     :if-exists :supersede
     :if-does-not-exist :create)
    (%add-revision article new-revision (content-categories content) content)
    (index-article article)
    new-revision))

(deftransaction %add-revision (article revision categories content)
  (push revision (revisions article))
  (push revision *recent-revisions*)
  (setf (category-list article) (mapcar #'category-keyword categories)
        (cached-content article) content))

(defun link-to-edit (revision text)
  #?[<a href="$(#/site/edit-article?title={(title (article revision))}&from-revision={(store-object-id revision)})">${text}</a>])

(defun current-and-history-buttons (revision)
  (let ((article (article revision)))
    #H[<li><a href="${(link-to article)}">Current version</a></li>
    <li><a href="$(#/site/history?title={(title article)})">History</a></li>]))

(defun render-revision (revision &optional (content (revision-content revision)))
  (generate-html-from-markup content)
  (awhen (content-categories content)
    #H[<div id="categories"><hr />Categories: ]
    (loop for category in it for divider = nil then t do
      (when divider #H" | ") (pprint-category-link category))
    #H[</div>])
  (setf
   *footer*
   (let ((title (title (article revision))))
     (with-output-to-string (*html-stream*)
       (current-and-history-buttons revision)
       (unless (youre-banned?)
         #H[<li>${(link-to-edit revision "Edit")}</li>]
         #H[<li><a href="$(#/site/edit-article?create=t)">Create</a></li>]
         (when *account*
           #H[<li><form method="post" action="$(#/site/delete?title={title})">
           <input class="del" type="submit" value="Delete" /></form></li>]))))))

(defun find-revision (string-id)
  (let ((revision (store-object-with-id (parse-integer string-id))))
    (assert (typep revision 'revision))
    revision))

(defpage /site/view-revision () (id)
  (let ((revision (find-revision id)))
    #H[<div class="centered">Revision ${(rfc-1123-date (date revision))}</div>]
    (render-revision revision)))

(defmethod link-to ((revision revision))
  #/site/view-revision?id={(store-object-id revision)})

(defun pprint-revision-link (revision)
  #H[<a class="internal" href="${(link-to revision)}">${(rfc-1123-date (date revision))}</a>])

;;; edit article

(defun render-edit-article-common (content summary)
  #H[<textarea rows="18" cols="80" name="content">${content}</textarea>
<div>Edit summary:
<input type="text" name="summary" size="50" value="${summary}" />
</div>
<input type="submit" value="Save" name="save" />
<input type="submit" value="Preview" name="preview" />]
  (when content
    #H[<h1>Article preview:</h1>]
    (generate-html-from-markup content)))

(defpage /site/edit-article () (title content summary from-revision save create)
  (let ((maybe-article (find-article title)))
    #H[<form method="post" action="$(#/site/edit-article)">]
    (cond ((check-banned))
          ((find-deleted-article title) (redirect (link-to title)))
          (save (let ((article (or maybe-article
                                   (make-instance 'article :title title))))
                  (add-revision article summary content)
                  (redirect (link-to article))))
          (create (setf *title* "Create new article")
                  #H[<span>Title:</span>
                     <input type="text" name="title" size="50" />]
                  (render-edit-article-common "" "created page"))
          (t (setf *title* #?"Editing ${title}")
             #H[<h1>Editing '${title}'</h1>]
             #H[<input type="hidden" name="title" value="${title}" />]
             (render-edit-article-common
              (cond (content content)
                    (from-revision (revision-content (find-revision from-revision)))
                    (maybe-article (cached-content maybe-article))
                    (t ""))
              (cond (summary summary)
                    ((not maybe-article) "created page")
                    (t "")))))
    #H[</form>]))
