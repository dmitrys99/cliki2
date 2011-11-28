(in-package #:cliki2)
(in-readtable cliki2)

(defun output-undo-link (revision)
  (unless (youre-banned?)
    #H[<form method="post" action="$(#/site/history-special)">(<input type= "hidden" name="r" value="${(store-object-id revision)}"/><input class="undo" type="submit" name="undo" value="undo" />)</form>]))

(defun output-compare-link (old new text)
  #H[(<a class="internal" href="$(#/site/compare-revisions?old={(store-object-id old)}&diff={(store-object-id new)})">${text}</a>)])

(defpage /site/history () (title)
  (awhen (find-article-any title)
    (setf *title* #?'History of article: "${title}"')

    #H[<h1>History of article ] (pprint-article-link title) #H[</h1>
    <form method="post" action="$(#/site/history-special)">
    <input type="submit" value="Compare selected versions" />
    <table id="pagehistory">]

    (loop for rhead on (revisions it)
          for revision = (car rhead)
          for author = (author revision)
          for first = t then nil do
         (flet ((radio (x)
                  #H[<td><input type="radio" name="${x}" value="${(store-object-id revision)}" /></td>]))
           #H[<tr><td>]
           (awhen (cadr rhead)
             (output-compare-link it revision "prev"))
           #H[</td>]
           (radio "old") (radio "diff")
           #H[<td>] (pprint-revision-link revision)
           #H[ ${(format-account-link author)} (<em>${(summary revision)}</em>) ]
           (when first
             (output-undo-link revision))
           #H[</td></tr>]))

    #H[</table>
    <input type="submit" value="Compare selected versions" />
    </form>]

    (setf *footer* #?[<li><a href="${(link-to it)}">Current version</a></li>])))

(defpage /site/compare-revisions () (old diff)
  (let* ((oldr (find-revision old))
         (diffr (find-revision diff))
         (title (title (article oldr))))
    (when (> (date oldr) (date diffr))
      (rotatef oldr diffr))
    (setf *title* title
          *footer* (with-output-to-string (*html-stream*)
                     (current-and-history-buttons oldr)))
    #H[<h1><a class="internal" href="${(link-to title)}">${title}</a></h1>
  <table class="diff">
  <colgroup>
    <col class="diff-marker"> <col class="diff-content">
    <col class="diff-marker"> <col class="diff-content">
  </colgroup>
  <tbody>
    <tr>
      <th colspan="2"> Version ] (pprint-revision-link oldr)
      #H[ (${(link-to-edit oldr "edit")})</th>
      <th colspan="2"> Version ] (pprint-revision-link diffr)
      #H[ (${(link-to-edit diffr "edit")}) ]
      (when (eq diffr (latest-revision (article diffr)))
        (output-undo-link diffr))
      #H[</th>
    </tr>
    ${(diff:format-diff-string 'wiki-diff (revision-path oldr) (revision-path diffr))}
  </tbody>
  </table>]))

;;; undo

(defun check-banned ()
  (when (youre-banned?) #H[Your account/IP is banned from editing]))

(defpage /site/not-latest "Revision not the latest" (title)
  #H[Can't undo this revision because it is not the latest.
  <a href="$(#/site/history?title={title})">Go back to history page</a>.])

(defun undo (r)
  (let* ((revision (find-revision r))
         (article (article revision))
         (latest-revision (latest-revision article)))
    (cond ((check-banned))
          ((typep article 'deleted-article)
           (link-to (undelete-article article)))
          ((eq revision latest-revision)
           (prog1 (link-to article)
             (if (typep latest-revision 'revision-undelete)
                 (delete-article article)
                 (add-revision
                  article
                  #?"undid last revision by ${(name (author revision))}"
                  (revision-content (second (revisions article)))))))
          (t #/site/not-latest?title={(title article)}))))

(defhandler /site/history-special (old diff undo r)
  (if undo
      (undo r)
      #/site/compare-revisions?old={old}&diff={diff}))