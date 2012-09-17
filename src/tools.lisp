(in-package #:cliki2)
(in-readtable cliki2)

(defpage /site/tools "Tools" ()
  #H[<h2>Tools</h2>
  <ul>
  <li><a href="$(#/site/all-articles)">All articles</a></li>
  <li><a href="$(#/site/all-topics)">All topics</a></li>
  <li><a href="$(#/site/uncategorized)">Uncategorized articles</a></li>
  <li><a href="$(#/site/deleted-articles)">Deleted articles</a></li>
  <li><a href="$(#/site/blacklist)">Blacklist of users/IPs</a></li>
  </ul>])

(defpage /site/blacklist "Blacklist" ()
  #H[<h3>Banned accounts/IPs</h3>
  <ul>]
  (dolist (banned (get-blacklist))
    #H[<li>${ (account-link banned) }</li>])
  #H[</ul>])

(defpage /site/all-articles "All articles" (start)
  (paginate-article-summaries
   start
   (get-all-articles (complement #'deleted?))))

(defpage /site/deleted-articles "Deleted articles" (start)
  (paginate-article-summaries
   start
   (get-all-articles #'deleted?)))

(defpage /site/uncategorized "Uncategorized articles" (start)
  (paginate-article-summaries
   start
   (get-all-articles (lambda (article)
                       (not (or (deleted? article)
                                (topics (cached-content
                                         (article-title article)))))))))

(defpage /site/all-topics "All topic markers" ()
  #H[<ul>]
  (dolist (topic (sort (all-topics) #'string-lessp))
    #H[<li>] (pprint-topic-link topic) #H[</li>])
  #H[</ul>])
