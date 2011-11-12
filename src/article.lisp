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
  (article-with-canonical-title (string-downcase (cut-whitespace title))))

(defun latest-revision (article)
  (car (revisions article)))

(defun article-description (article)
  (let ((content (cached-content article)))
    (subseq content 0
            (1- (max 1 (or (nth-value 1 (cl-ppcre:scan ".*\\.[\\s]" content))
                           (position #\Newline content)
                           (max 30 (length content))))))))

(defmethod link-to ((article article))
  (link-to (canonical-title article)))

(defmethod link-to ((article-titled string))
  #?[/${(uri-encode (string-downcase (cut-whitespace article-titled)))}])

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
                     author
                     (author-ip (hunchentoot:real-remote-addr))
                     (date (get-universal-time)))
  (let ((new-revision (make-instance 'revision
                                     :article    article
                                     :author     author
                                     :author-ip  author-ip
                                     :date       date
                                     :summary    summary)))
    (alexandria:write-string-into-file
     content
     (ensure-directories-exist (revision-path new-revision))
     :if-exists :supersede
     :if-does-not-exist :create)
    (%add-revision article new-revision (content-categories content) content)
    (index-document (store-object-id article) content)
    new-revision))

(deftransaction %add-revision (article revision categories content)
  (push revision (revisions article))
  (push revision *recent-revisions*)
  (setf (category-list article) (mapcar #'category-keyword categories)
        (cached-content article) content))

(defun render-revision (revision &optional (content (revision-content revision)))
  (let ((title (title (article revision))))
    (princ (generate-html-from-markup content) *html-stream*)
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
  #/site/view-revision?id={(store-object-id revision)})

;;; article history

(defpage /site/history () (title)
  (awhen (find-article title)
    (setf *title* #?'History of article: "${title}"')
    #H[<h1>${title}</h1>
    <form method="post" action="$(#/site/do-compare-revisions)">
    <input type="submit" value="Compare selected versions" />
    <ul id="pagehistory">]

    (dolist (revision (revisions it))
      (flet ((radio (x) #H[<input type="radio" name="${x}" value="${(store-object-id revision)}" />]))
        (let ((author (author revision)))
          #H[<li>] (radio "old") (radio "diff")
          #H[<a href="${(link-to revision)}">${(hunchentoot:rfc-1123-date (date revision))}</a>
          ${(summary revision)}
          <a href="${(link-to author)}">${(name author)}</a>
          </li>])))

    #H[</ul>
    <input type="submit" value="Compare selected versions" />
    </form>
    <div id="footer"><a href="">Current version</a></div>]))

;;; diff

(defhandler /site/do-compare-revisions (old diff)
  #/site/compare-revisions?old={old}&diff={diff})

(defpage /site/compare-revisions () (old diff)
  (let ((old-revision (find-revision old))
        (diff-revision (find-revision diff)))
    (setf *title* (title (article old-revision)))
    #H[<h1>${(title (article old-revision))}</h1>
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
                                                                  (maybe-article (cached-content maybe-article))
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

;;; article dispatcher

(defun guess-article-name ()
  (uri-decode (subseq (script-name*) 1)))

(defun render-article (article)
  (render-page (title article)
    (render-revision (latest-revision article) (cached-content article))))

(defun article-dispatcher (request)
  (declare (ignore request))
  (awhen (find-article (guess-article-name))
    (lambda () (render-article it))))

(define-easy-handler (root :uri "/") ()
  (render-article (find-article "index")))