(in-package #:cliki2)
(in-readtable cliki2)

(defvar *recent-revisions* ())

(defun init-recent-revisions ()
  (let ((sorted (sort (copy-list (store-objects-with-class 'revision))
                      #'> :key #'date)))
    (setf *recent-revisions* (subseq sorted 0 (min 100 (length sorted))))))

(defun do-recent-revisions (f)
  (loop for i from 0 below 100
        for x on *recent-revisions*
        do (funcall f (car x))
        finally (when x (setf (cdr x) nil))))

(defun find-previous-revision (revision)
  (cadr (member revision (revisions (article revision)))))

(defun %render-revision-summary (revision)
  (pprint-revision-link revision)
  #H[ <a class="internal" href="${(link-to (article revision))}">${(title (article revision))}</a>
  - ${(summary revision)} ${(format-account-link (author revision))} ]
  (awhen (find-previous-revision revision)
    (output-compare-link it revision "diff")))

(defun render-revision-summary (revision)
  #H[<li>] (%render-revision-summary revision) #H[</li>])

(defpage /site/recent-changes "Recent Changes" ()
  (setf *header* #?[<link rel="alternate" type="application/atom+xml" title="recent changes" href="$(#/site/feed/recent-changes.atom)">])
  #H[<h1>Recent Changes</h1>
  <a class="internal" href="$(#/site/feed/recent-changes.atom)">ATOM feed</a>
  <ul>] (do-recent-revisions #'render-revision-summary) #H[</ul>])

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
      <link href="${link}" />
      <updated>${(iso8601-time updated)}</updated>]
      (funcall entries-body)
    #H[</feed>]))

(defun feed-format-content (revision)
  (escape-for-html
   (with-output-to-string (*html-stream*)
     (%render-revision-summary revision)
     (render-diff-table (find-previous-revision revision) revision nil))))

(defun feed-present-revision (revision)
  #H[<entry>
  <title>${(title (article revision))} - ${(summary revision)} ${(name (author revision))}</title>
  <link href="${(link-to revision)}" type="text/html" />
  <updated>${(iso8601-time (date revision))}</updated>
  <content type="html">${(feed-format-content revision)}</content>
</entry>])

(%defpage /site/feed/recent-changes.atom :get ()
  (feed-doc
   "CLiki Recent Changes" #/site/feed/recent-changes.atom
   (date (car *recent-revisions*))
   (lambda ()
     (do-recent-revisions #'feed-present-revision))))

(%defpage /site/feed/article.atom :get (title)
  (awhen (find-article-any title)
    (feed-doc
     #?"CLiki Article ${title} Edits"
     #/site/feed/article.atom?title={title}
     (date (latest-revision it))
     (lambda ()
       (loop repeat 20 for revision in (revisions it)
             do (feed-present-revision revision))))))
