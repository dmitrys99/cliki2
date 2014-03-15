(in-package #:cliki2)
(in-readtable cliki2)

(defun find-previous-revision (revision)
  (cadr (member revision (revisions
                          (find-article (parent-title revision) :error t)))))

(defun %render-revision-summary (revision)
  (pprint-revision-link revision)
  #H[ <a class="internal" href="${ (article-link (parent-title revision)) }">${ (escape-for-html (parent-title revision)) }</a>
  - ${ (escape-for-html (summary revision)) } ${ (account-link (author-name revision)) } ]
  (awhen (find-previous-revision revision)
    (output-compare-link it revision "diff")))

(defun render-revision-summary (revision)
  #H[<li>] (%render-revision-summary revision) #H[</li>])

(defpage /site/recent-changes "Последние изменения" ()
  (setf *header* #?[<link rel="alternate" type="application/atom+xml" title="последние изменения" href="$(#/site/feed/recent-changes.atom)">])
  #H[<h1>Последние изменения</h1>
  <a class="internal" href="$(#/site/feed/recent-changes.atom)">ATOM</a>
  <ul>] (map nil #'render-revision-summary (get-recent-changes)) #H[</ul>])

;;; feed

(defun iso8601-time (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour minute second)))

(defun feed-doc (title link updated entries-body)
  (setf (content-type*) "application/atom+xml")
  (with-output-to-string (*html-stream*)
    #H[<?xml version="1.0" encoding="utf-8"?>
    <feed xmlns="http://www.w3.org/2005/Atom">
      <title>${title}</title>
      <link href="${ (escape-for-html link) }" />
      <updated>${ (iso8601-time updated) }</updated>]
      (funcall entries-body)
    #H[</feed>]))

(defun feed-format-content (revision)
  (escape-for-html
   (with-output-to-string (*html-stream*)
     (%render-revision-summary revision)
     (render-diff-table (find-previous-revision revision) revision nil))))

(defun feed-present-revision (revision)
  #H[<entry>
  <title>${ (parent-title revision) } - ${ (escape-for-html (summary revision)) } ${ (author-name revision) }</title>
  <link href="${ (escape-for-html (revision-link revision)) }" type="text/html" />
  <updated>${ (iso8601-time (revision-date revision)) }</updated>
  <content type="html">${ (feed-format-content revision) }</content>
</entry>])

(%defpage /site/feed/recent-changes.atom :get ()
  (feed-doc
   #?"${(wiki-name *wiki*)} Последние изменения" #/site/feed/recent-changes.atom
   (revision-date (car (get-recent-changes)))
   (lambda ()
     (map nil #'feed-present-revision (get-recent-changes)))))

(%defpage /site/feed/article.atom :get (title)
  (let ((article (find-article title :error t)))
    (feed-doc
     #?"${(wiki-name *wiki*)} Статья ${ (escape-for-html title) } Правки"
     #/site/feed/article.atom?title={title}
     (revision-date (latest-revision article))
     (lambda ()
       (loop repeat 20 for revision in (revisions article)
             do (feed-present-revision revision))))))
