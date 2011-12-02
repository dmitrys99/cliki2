(in-package #:cliki2)
(in-readtable cliki2)

(defvar *recent-revisions* ())

(defun init-recent-revisions ()
  (setf *recent-revisions*
        (sort (copy-list (store-objects-with-class 'revision))
              #'>
              :key #'date)))

(defun do-recent-revisions (f)
  (loop for i from 0 below 100
        for x on *recent-revisions*
        do (funcall f (car x))
        finally (when x (setf (cdr x) nil))))

(defun find-previous-revision (revision)
  (cadr (member revision (revisions (article revision)))))

(defun render-revision-summary (revision)
  #H[<li>] (pprint-revision-link revision)
  #H[ <a class="internal" href="${(link-to (article revision))}">${(title (article revision))}</a>
  - ${(summary revision)} ${(format-account-link (author revision))} ]
  (awhen (find-previous-revision revision)
    (output-compare-link it revision "diff"))
  #H[</li>])

(defpage /site/recent-changes "Recent Changes" ()
  (setf *header* #?[<link rel="alternate" type="application/rss+xml" title="recent changes" href="$(#/site/feed/rss.xml)">])
  #H[<h1>Recent Changes</h1>
  <a class="internal" href="$(#/site/feed/rss.xml)">RSS feed</a>
  <ul>] (do-recent-revisions #'render-revision-summary) #H[</ul>])

;;; RSS feed

(defun rss-doc (title link description items-body)
  (setf (content-type*) "application/rss+xml")
  (with-output-to-string (*html-stream*)
    #H[<?xml version="1.0" encoding="utf-8"?>
    <rss version="2.0">
    <channel>
    <title>${title}</title>
    <link>${link}</link>
    <description>${description}</description>]
    (funcall items-body)
    #H[</channel>
    </rss>]))

(defun rss-present-revision (revision)
  #H[<item>
  <title>${(name (author revision))}: ${(title (article revision))}</title>
  <link>${(link-to revision)}</link>
  <description>${(summary revision)}
Diff:
${(awhen (cadr (member revision (revisions (article revision))))
    (escape-for-html
     (diff:format-diff-string
      'diff:unified-diff
      (revision-path it)
      (revision-path revision))))}
  </description>
  <pubDate>${(rfc-1123-date (date revision))}</pubDate>
  </item>])

(%defpage /site/feed/rss.xml :get ()
  (rss-doc
   "CLiki Recent Changes" #/site/feed/rss.xml "CLiki Recent Changes"
   (lambda ()
     (do-recent-revisions #'rss-present-revision))))

(%defpage /site/article-feed/rss.xml :get (title)
  (awhen (find-article-any title)
    (let ((description #?"CLiki Article ${title} Edits"))
      (rss-doc
       description #/site/article-feed/rss.xml?title={title} description
       (lambda ()
         (map nil #'rss-present-revision (revisions it)))))))
