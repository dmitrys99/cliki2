(in-package #:cliki2)
(in-readtable cliki2)

(defun cut-whitespace (str)
  (string-trim #(#\Space #\Tab #\Newline #\Return)
               (ppcre:regex-replace-all "\\s+" str " ")))

;;; article

(defstruct (article (:type list) (:conc-name nil))
  article-title
  revisions)

(defun deleted? (article)
  (equal "" (cached-content (article-title article))))

(defun article-link (x)
  (format nil "/~A" (uri-encode (cut-whitespace x))))

(defun latest-revision (article)
  (car (revisions article)))

(defun %print-article-link (title class)
  (let* ((article (find-article title))
         (class   (if (and article (not (deleted? article))) class "new")))
    #H[<a href="${ (article-link title) }" class="${ class }">${ (escape-for-html title) }</a>]))

(defun pprint-article-link (title)
  (%print-article-link title "internal"))

(defun pprint-topic-link (title)
  (%print-article-link title "category"))

;;; revisions

(defstruct (revision (:type list) (:conc-name nil))
  parent-title
  revision-date
  summary
  UNUSED-revision-delete ;; here only to work w/old data format
  author-name
  author-ip)

(defun add-revision (article content summary)
  (record-revision (make-revision :parent-title     (article-title article)
                                  :revision-date    (get-universal-time)
                                  :summary          summary
                                  :author-name      (if *account*
                                                        (account-name *account*)
                                                        (real-remote-addr))
                                  :author-ip        (real-remote-addr))
                   (string-trim #(#\Space #\Newline #\Tab)
                                (remove #\Return content))))

(defun edit-link (revision text)
  #?[<a href="$(#/site/edit-article?title={ (parent-title revision) }&amp;from-revision={ (revision-date revision) })">${ text }</a>])

(defun article-footer (revision)
  (with-output-to-string (*html-stream*)
    (let ((title (parent-title revision)))
      #H[<li><a href="${ (article-link title) }">Current version</a></li>
      <li><a href="$(#/site/history?article={ title })">History</a></li>
      <li><a href="$(#/site/backlinks?article={ title })">Backlinks</a></li>]
      (unless (youre-banned?)
        #H[<li>${ (edit-link revision "Edit") }</li>]
        #H[<li><a href="$(#/site/edit-article?create=t)">Create</a></li>]))))

(defun render-revision (revision &optional (content (revision-content revision)))
  (generate-html-from-markup content)
  (setf *footer* (article-footer revision)))

(defun find-revision (article-title date-string)
  (find (parse-integer date-string)
        (revisions (find-article article-title :error t))
        :key #'revision-date))

(defpage /site/view-revision () (article date)
  (let* ((revision      (find-revision article date))
         (revision-name #?"Revision ${ (rfc-1123-date (revision-date revision)) }"))
    (setf *title* #?"${ (escape-for-html article) } ${ revision-name }")
    #H[<div class="centered">${ revision-name }</div>]
    (render-revision revision)))

(defun revision-link (revision)
  #/site/view-revision?article={ (parent-title revision) }&date={ (revision-date revision) })

(defun pprint-revision-link (revision)
  #H[<a class="internal" href="${ (revision-link revision) }">${ (rfc-1123-date (revision-date revision)) }</a>])

;;; edit article

(defun render-edit-article-common (title content summary &key edit-title error)
  (if edit-title
      #H[<span>Title: </span>
      <input type="text" name="title" size="50" value="${title}"/>]
      (progn
        (setf *title* #?"Editing ${title}")
        #H[<h1>Editing '${title}'</h1>]
        #H[<input type="hidden" name="title" value="${title}" />]))

  #H[<textarea rows="18" cols="80" name="content">${(escape-for-html content)}</textarea>
<dl class="prefs">
<dt><label for="summary">Edit summary:</label></dt>
<dd><input type="text" name="summary" size="50" value="${summary}" /></dd>]

  (unless *account*
    (maybe-show-form-error error t "Wrong captcha answer")
    (let ((captcha (make-captcha)))
      #H[<dt><label for="captcha">${captcha} is:</label></dt><dd>]
      (emit-captcha-inputs captcha "" 50)
      #H[</dd>]))

  #H[</dl>
<input type="submit" value="Save" name="save" />
<input type="submit" value="Preview" name="preview" />]
  (when content
    #H[<h1>Article preview:</h1>]
    (generate-html-from-markup (remove #\Return content))))

(defpage /site/edit-article () (title content summary from-revision save create)
  (let ((maybe-article (find-article title)))
    #H[<form method="post" action="$(#/site/edit-article)">]
    (cond ((youre-banned?)
           (redirect #/))
          (save
           (if (or *account* (check-captcha))
               (progn
                 (add-revision (or maybe-article
                                   (wiki-new
                                    'article
                                    (make-article
                                     :article-title (cut-whitespace title))))
                               content
                               summary)
                 (redirect (article-link title)))
               (render-edit-article-common
                title content summary :edit-title (not maybe-article) :error t)))
          (create
           (setf *title* "Create new article")
           (render-edit-article-common "" "" "created page" :edit-title t))
          (t
           (render-edit-article-common
            title
            (cond (content        content)
                  (from-revision  (revision-content
                                   (find-revision title from-revision)))
                  (maybe-article  (cached-content title))
                  (t              ""))
            (if (and (not maybe-article) (not summary))
                "created page"
                (or summary ""))
            :edit-title (not maybe-article))))
    #H[</form>]))
