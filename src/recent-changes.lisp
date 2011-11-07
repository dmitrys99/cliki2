(in-package #:cliki2)
(in-readtable cliki2)

(defvar *recent-revisions* ())

(defun init-recent-revisions ()
  (setf *recent-revisions*
        (sort (store-objects-with-class 'revision)
              #'>
              :key #'date)))

(defun do-recent-revisions (f)
  (loop for i from 0 below 100
        for x on *recent-revisions*
        do (funcall f (car x))
        finally (when x (setf (cdr x) nil))))

(defpage /site/recent-changes "CLiki: Recent Changes" ()
  #H[<h1>Recent Changes</h1>
  <a href="$(#/site/feed/rss.xml)">RSS feed</a>
  <ul>] (do-recent-revisions
          (lambda (revision)
            #H[<li><a href="${(link-to revision)}">${(date revision)}</a>
            ${(title (revision-article revision))} - ${(summary revision)}
            <a href="${(link-to (author revision))}">${(name (author revision))}</a>
            </li>]))
  #H[</ul>])

;;; RSS feed

(%defpage /site/feed/rss.xml :get ()
  (setf (content-type*) "application/rss+xml")
  (with-output-to-string (*html-stream*)
    #H[<?xml version="1.0" encoding="utf-8"?>
    <rss version="2.0">
    <channel>
    <title>CLiki Recent Changes</title>
    <link>$(#/site/feed/rss.xml)</link>
    <description>CLiki Recent Changes</description>]

    (do-recent-revisions
      (lambda (revision)
        #H[<item>
        <title>${(name (author revision))}: ${(title (revision-article revision))}</title>
        <link>${(link-to (revision-article revision))}</link>
        <description>${(summary revision)}</description>
        <pubDate>${(date revision)}</pubDate>
        </item>]))

    #H[</channel>
    </rss>]))
