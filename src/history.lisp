(in-package #:cliki2)
(in-readtable cliki2)

(defun output-undo-button (revision)
  (unless (youre-banned?)
    #H[<input type="hidden" name="undo-revision" value="${ (revision-date revision) }" />
    (<input type="submit" name="undo" value="undo" class="undo" />)]))

(defun output-compare-link (old new text)
  #H[(<a class="internal" href="$(#/site/compare-revisions?article={ (parent-title new) }&old={ (revision-date old) }&diff={ (revision-date new) })">${text}</a>)])

(defpage /site/history () (article)
  (let ((article-obj (find-article article :error t)))
    (setf *title* #?'History of article: "${article}"')

    #H[<h1>History of article ] (pprint-article-link article) #H[</h1>
    <form method="post" action="$(#/site/history-with-undo)">
    <input type="hidden" name="article" value="${ article }" />
    <input type="submit" value="Compare selected versions" />
    <table id="pagehistory">]

    (loop for rhead on (revisions article-obj)
          for revision = (car rhead)
          for author   = (author-name revision)
          for first    = t then nil do
         (flet ((radio (x)
                  #H[<td><input type="radio" name="${x}" value="${ (revision-date revision) }" /></td>]))
           #H[<tr><td>]
           (awhen (cadr rhead)
             (output-compare-link it revision "prev"))
           #H[</td>]
           (radio "old") (radio "diff")
           #H[<td>] (pprint-revision-link revision)
           #H[ ${ (account-link author) } (<em>${ (escape-for-html (summary revision)) }</em>) ]
           (when first
             (output-undo-button revision))
           #H[</td></tr>]))

    #H[</table>
    <input type="submit" value="Compare selected versions" />
    </form>]

    (setf *footer* #?[<li><a href="${ (article-link article) }">Current version</a></li>])))

;;; undo

(defpage /site/not-latest "Revision not the latest" (article)
  #H[Can't undo this revision because it is not the latest.
  <a href="$(#/site/history?article={article})">Go back to history page</a>.])

(defun undo-latest-revision (article)
  (let ((revision          (first  (revisions article)))
        (restored-revision (second (revisions article))))
    (when restored-revision
      (add-revision
       article (revision-content restored-revision)
       #?"undid last revision by ${ (author-name revision) }"
       :revision-delete (revision-delete restored-revision)))))

(defun undo-revision (article-title revision-date)
  (let ((revision       (find-revision article-title revision-date))
        (article-object (find-article  article-title)))
    (if (eq revision (latest-revision article-object))
        (progn (unless (youre-banned?)
                 (undo-latest-revision article-object))
               (article-link article-title))
        #/site/not-latest?article={article-title})))

(defhandler /site/history-with-undo (article old diff undo undo-revision)
  (if undo
      (undo-revision article undo-revision)
      #/site/compare-revisions?article={article}&old={old}&diff={diff}))

(defhandler /site/undo (article undo-revision)
  (undo-revision article undo-revision))
