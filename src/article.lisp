(in-package #:cliki2)
(in-readtable cliki2)

;;; delete article (only logged-in users can delete, view, and undelete deleted articles)

(defun cut-whitespace (str)
  (string-trim #(#\Space #\Tab #\Newline #\Return)
               (ppcre:regex-replace-all "\\s+" str " ")))

;;; article categories

(defun category-keyword (category-title)
  (intern (string-upcase (cut-whitespace category-title)) '#:cliki2.categories))

(defun content-categories (content)
  (mapcar
   (lambda (x)
     (subseq x 2 (1- (length x))))
   (cl-ppcre:all-matches-as-strings
    '(:sequence #\* #\( (:greedy-repetition 0 nil (:inverted-char-class #\))) #\))
    content)))

;;; article

(defclass article (store-object)
  ((title            :initarg       :title
                     :reader        title)
   (canonical-title  :index-type    string-unique-index
                     :index-reader  article-with-canonical-title
                     :reader        canonical-title)
   (revisions        :initarg :revisions
                     :initform ()
                     :accessor revisions)
   (category-list    :initarg       :category-list
                     :initform      ()
                     :accessor      category-list
                     :index-type    hash-list-index
                     :index-reader  articles-with-category))
  (:metaclass persistent-class))

(defmethod shared-initialize :after ((article article) slot-names &key &allow-other-keys)
  (with-slots (title canonical-title) article
    (setf title (cut-whitespace title)
          canonical-title (string-downcase title))))

(defun content-directory (article)
  (list :relative "articles" (uri-encode (canonical-title article))))

(defun find-article (title)
  (article-with-canonical-title (string-downcase (cut-whitespace title))))

(defun latest-revision (article)
  (car (revisions article)))

(defvar *latest-revision-cache* (make-hash-table))
(defvar *latest-revision-cache-lock* (bt:make-lock))

(defun latest-content (article)
  (let ((latest-revision (latest-revision article))
        (cached (bt:with-lock-held (*latest-revision-cache-lock*)
                 (gethash article *latest-revision-cache*))))
    (if (eq (car cached) latest-revision)
        (cdr cached)
        (let ((content (revision-content latest-revision)))
          (bt:with-lock-held (*latest-revision-cache-lock*)
            (setf (gethash article *latest-revision-cache*)
                  (cons latest-revision content)))
          content))))

(defun article-description (article)
  (let ((content (latest-content article)))
    (subseq content 0 (1- (nth-value 1 (cl-ppcre:scan ".*\\.[\\s]" content))))))

(defmethod link-to ((article article))
  (link-to (canonical-title article)))

(defmethod link-to ((article-titled string))
  #?[/${(uri-encode (string-downcase (cut-whitespace article-titled)))}])

;;; revisions

(defclass revision (store-object)
  ((article    :initarg      :article
               :reader       revision-article)
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
  (merge-pathnames
   (make-pathname :directory (content-directory (revision-article revision))
                  :name (write-to-string (date revision)))
   *datadir*))

(defun revision-content (revision)
  (alexandria:read-file-into-string (revision-path revision)))

(defun add-revision (article summary content &key
                     author
                     (author-ip (hunchentoot:real-remote-addr))
                     (date (get-universal-time)))
  (alexandria:write-string-into-file
   content
   (ensure-directories-exist (content-directory article))
   :if-exists :supersede
   :if-does-not-exist :create)
  (let ((new-revision (make-instance 'revision
                                     :article    article
                                     :author     author
                                     :author-ip  author-ip
                                     :date       date
                                     :summary    summary)))
    (%add-revision article new-revision (content-categories content))
    new-revision))

(deftransaction %add-revision (article revision categories)
  (push revision (revisions article))
  (push revision *recent-revisions*)
  (index-document (store-object-id article) (revision-content revision))
  (setf (category-list article) (mapcar #'category-keyword categories)))

(defun render-revision (revision)
  (let ((title (title (revision-article revision))))
    (princ (generate-html-from-markup revision) *html-stream*)
    #H[<div id="footer">
         <a href="/${title}">Current version</a>
         <a href="$(#/site/edit-article?title={title}&from-revision={(store-object-id revision)})">Edit</a>
         <a href="$(#/site/history?title={title})">History</a>
</div>]))

(defun find-revision (string-id)
  (let ((revision (store-object-with-id (parse-integer string-id))))
    (assert (typep revision 'revision))
    revision))

(defpage /site/view-revision () (id)
  (render-revision (find-revision id)))

(defmethod link-to ((revision revision))
  #/site/view-revision?id=${(store-object-id revision)})

;;; article history

(defpage /site/history () (title)
  (let ((article (find-article title)))
    (if article
        (progn
          (setf *title* (format nil "History of article: \"~A\"" title))
          #H[<h1>${title}</h1>
          <form method="post">
          <input type="submit" value="Compare selected versions" />
          <ul id="pagehistory">]

          (dolist (revision (revisions article))
            (flet ((radio (x) #H[<input type="radio" name="${x}" value="${(store-object-id revision)}" />]))
              (let ((author (author revision)))
                #H[<li>] (radio "old") (radio "diff")
                #H[<a href="${(link-to revision)}">${(hunchentoot:rfc-1123-date (date revision))}</a>
                ${(summary revision)}
                <a href="${(link-to author)}">${(account-name author)}</a>
                </li>])))

          #H[</ul>
          <input type="submit" value="Compare selected versions" />
          </form>
          <div id="footer"><a href="">Current version</a></div>])
        (progn (setf *title* "Article not found"
                     (return-code*) 404)
               #H[No article named "$(title)" found.]))))

;;; diff

(defpage /site/compare-revisions () (old diff)
  (let ((old-revision (find-revision old))
        (diff-revision (find-revision diff)))
  (setf *title* (title (revision-article old-revision)))
  #H[<h1>${(title (revision-article old-revision))}</h1>
  <table class="diff">
  <colgroup>
    <col class="diff-marker"> <col class="diff-content">
    <col class="diff-marker"> <col class="diff-content">
  </colgroup>
  <tbody>
    <tr>
      <th colspan="2">
      <a href="${(link-to old-revision)}">Version ${(date old-revision)}</a>
      </th>
      <th colspan="2">
      <a href="${(link-to diff-revision)}">Version ${(date diff-revision)}</a>
      </th>
    </tr>
  ${(diff:format-diff-string 'wiki-diff (revision-path old-revision) (revision-path diff-revision))}
  </tbody>
  </table>]))

;;; edit article

(defpage /site/edit-article () (title content summary from-revision)
  (let ((maybe-article (find-article title)))
    (setf *title* #?"Editing ${title}")
    #H[<form method="post">
         <form  method="post">
           <div class="textarea">
             <textarea rows="30" cols="80" name="content">${(cond (from-revision (revision-content (find-revision from-revision)))
                                                                  (maybe-article (latest-content maybe-article))
                                                                  (t ""))}</textarea>
           </div>

           <div class="edit-buttons">
             <input type="submit" value="Save" name="save" />
             <input type="submit" value="Preview" name="preview" />

             <span>Summary of changes</span>
             <input type="text" name="summary" size="50" value="${(if summary summary "")}" />
           </div>
         </form>
         ${(if content (generate-html-from-markup content) "")}
       </form>]))

(defhandler /site/save-article (title summary content)
  (add-revision (or (find-article title)
                    (make-instance 'article :title title))
                summary content)
  (link-to title))

;;; article

;; (defresource / (uri-template:uri-decode id) ;; fixme
;;   (let ((title (uri-template:uri-decode id)))
;;     (if (find-article title)
;;         (render-revision (latest-content article))
;;         (progn (setf (return-code*) 404)
;;                #H[<h1>Cliki2 does not have an article with this exact name</h1>
;;                <a href="$(#/site/edit-article?title=${})">Create</a>]))))
