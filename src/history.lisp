(in-package #:cliki2)
(in-readtable cliki2)

(defun output-undo-button (revision)
  (unless (youre-banned?)
    #H[<input type="hidden" name="undo-revision" value="${ (revision-date revision) }" />
    (<input type="submit" name="undo" value="отмена" class="undo" />)]))

(defun output-compare-link (old new text)
  #H[(<a class="internal" href="$(#/site/compare-revisions?article={ (parent-title new) }&old={ (revision-date old) }&diff={ (revision-date new) })">${text}</a>)])

(defpage /site/history () (article)
  (let ((article-obj (find-article article :error t)))
    (setf *title*  #?'Изменения статьи: "${ (article-title article-obj) }"'
          *header* #?[<link rel="alternate" type="application/atom+xml" title="article changes" href="$(#/site/feed/article.atom?title={ (article-title article-obj) })">]
          *footer* (article-footer (latest-revision article-obj)))

    #H[<h1>Изменения статьи ] (pprint-article-link article) #H[</h1>
    <a class="internal" href="$(#/site/feed/article.atom?title={ (article-title article-obj) })">ATOM</a>
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
    <input type="submit" value="Сравнить выбранные версии" />
    </form>]))

;;; undo

(defpage /site/not-latest "Не последняя ревизия" (article)
  #H[Не могу отменить эту ревизию, потому как она не последняя.
  <a href="$(#/site/history?article={article})">На страницу изменений</a>.])

(defun undo-latest-revision (article)
  (let ((revision          (first  (revisions article)))
        (restored-revision (second (revisions article))))
    (when restored-revision
      (add-revision
       article (revision-content restored-revision)
       #?"отменить последнюю ревизию автора ${ (author-name revision) }"))))

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
