(in-package #:cliki2)
(in-readtable cliki2)

(defpage /site/tools "Tools" ()
  #H[<h2>CLiki tools</h2>
  <ul>
  <li><a href="$(#/site/all-articles)">All CLiki articles</a></li>
  <li><a href="$(#/site/blacklist)">Blacklist of users/IPs</a></li>
  <li><a href="$(#/site/uncategorized)">Uncategorized articles</a></li>
  <li><a href="$(#/site/deleted-articles)">Deleted articles</a></li>
  </ul>])

(defpage /site/blacklist "Blacklist" ()
  #H[<h3>Banned accounts/IPs</h3>
  <ul>]
  (dolist (account (banned-accounts t))
    #H[<li>${(format-account-link account)}</li>])
  #H[</ul>])

(defpage /site/all-articles "All articles" (start)
  (paginate-article-summaries start (store-objects-with-class 'article)))

(defpage /site/uncategorized "Uncategorized articles" (start)
  (paginate-article-summaries
   start
   (remove-if #'category-list (store-objects-with-class 'article))))

(defpage /site/deleted-articles "Deleted articles" (start)
  (paginate-article-summaries start (store-objects-with-class 'deleted-article)))