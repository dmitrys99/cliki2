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

(defun pprint-article-link (title)
  #H[<a href="${(link-to title)}" class="${(if (find-article title) "internal" "new")}">${title}</a>])

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
                     (author-ip (real-remote-addr))
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
  (generate-html-from-markup content)
  (setf *footer*
        (let ((title (title (article revision))))
            #?[
<li><a href="${(link-to (article revision))}">Current version</a></li>
<li><a href="$(#/site/history?title={title})">History</a></li>
<li><a href="$(#/site/edit-article?title={title}&from-revision={(store-object-id revision)})">Edit</a></li>])))

(defun find-revision (string-id)
  (let ((revision (store-object-with-id (parse-integer string-id))))
    (assert (typep revision 'revision))
    revision))

(defpage /site/view-revision () (id)
  (render-revision (find-revision id)))

(defmethod link-to ((revision revision))
  #/site/view-revision?id={(store-object-id revision)})

(defun pprint-revision-link (revision)
  #H[<a href="${(link-to revision)}">${(rfc-1123-date (date revision))}</a>])

;;; article history

(defpage /site/history () (title)
  (awhen (find-article title)
    (setf *title* #?'History of article: "${title}"')
    #H[<h1>History of article '${title}'</h1>
    <form method="post" action="$(#/site/do-compare-revisions)">
    <input type="submit" value="Compare selected versions" />
    <table id="pagehistory">]

    (loop for rhead on (revisions it)
          for revision = (car rhead)
          for author = (author revision) do
         (flet ((radio (x)
                  #H[<td><input type="radio" name="${x}" value="${(store-object-id revision)}" /></td>]))
           #H[<tr><td>]
           (awhen (cadr rhead)
             #H[(<a href="$(#/site/compare-revisions?old={(store-object-id it)}&diff={(store-object-id revision)})">prev</a>)])
           #H[</td>]
           (radio "old") (radio "diff")
           #H[<td>] (pprint-revision-link revision)
           #H[ ${(format-account-link author)} (<em>${(summary revision)}</em>)</td>
           </tr>]))

    #H[</table>
    <input type="submit" value="Compare selected versions" />
    </form>]

    (setf *footer* #?[<li><a href="${(link-to it)}">Current version</a></li>])))

;;; diff

(defhandler /site/do-compare-revisions (old diff)
  #/site/compare-revisions?old={old}&diff={diff})

(defpage /site/compare-revisions () (old diff)
  (let ((oldr (find-revision old))
        (diffr (find-revision diff)))
    (when (> (date oldr) (date diffr))
      (rotatef oldr diffr))
    (setf *title* (title (article oldr)))
    #H[<h1>${(title (article oldr))}</h1>
  <table class="diff">
  <colgroup>
    <col class="diff-marker"> <col class="diff-content">
    <col class="diff-marker"> <col class="diff-content">
  </colgroup>
  <tbody>
    <tr>
      <th colspan="2"> Version ] (pprint-revision-link oldr) #H[</th>
      <th colspan="2"> Version ] (pprint-revision-link diffr) #H[</th>
    </tr>
  ${(diff:format-diff-string 'wiki-diff (revision-path oldr) (revision-path diffr))}
  </tbody>
  </table>]))

;;; edit article

(defpage /site/edit-article () (title content summary from-revision save)
  (let ((maybe-article (find-article title)))
    (if save
        (progn
          (add-revision maybe-article summary content
                        :author (or *account*
                                    (get-anonymous-account (real-remote-addr))))
          (redirect (link-to maybe-article)))
        (progn
          (setf *title* #?"Editing ${title}")
          #H[<h1>Editing '${title}'</h1>
          <form method="post">
            <form method="post">
              <div class="textarea">
                <textarea rows="18" cols="80" name="content">${
                (cond (content content)
                      (from-revision (revision-content (find-revision from-revision)))
                      (maybe-article (cached-content maybe-article))
                      (t ""))}</textarea>
              </div>
              <div class="edit-buttons">
                <span>Edit summary:</span>
                <input type="text" name="summary" size="50"
                       value="${(cond (summary summary)
                                      ((not maybe-article) "created page")
                                      (t ""))}" />
                <br />
                <input type="submit" value="Save" name="save" />
                <input type="submit" value="Preview" name="preview" />
              </div>
            </form>]
            (when content
              #H[<h1>Article preview:</h1>]
              (generate-html-from-markup content))
            #H[</form>]))))

(defhandler /site/save-article (title summary content)
  (add-revision (or (find-article title)
                    (make-instance 'article :title title))
                summary content)
  (link-to title))

;;; article dispatcher

(defun guess-article-name ()
  (uri-decode (subseq (script-name*) 1)))

(defun render-article (article)
  (let ((*header* #?[<link rel="alternate" type="application/rss+xml" title="edits" href="$(#/site/article-feed/rss.xml?title={(title article)})">]))
    (render-page (title article)
      (render-revision (latest-revision article) (cached-content article)))))

(defun article-dispatcher (request)
  (declare (ignore request))
  (awhen (find-article (guess-article-name))
    (lambda () (render-article it))))

(define-easy-handler (root :uri "/") ()
  (render-article (find-article "index")))