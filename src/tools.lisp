(in-package #:cliki2)
(in-readtable cliki2)

(defpage /site/tools "Tools" ()
  #H[<h2>CLiki tools</h2>
  <ul>
  <li><a href="$(#/site/all-articles)">All CLiki articles</a></li>
  <li><A href="$(#/site/blacklist)">Blacklist of users/IPs</a></li>
  </ul>])

(defpage /site/blacklist "Blacklist" ()
  #H[<h3>Banned accounts/IPs</h3>
  <ul>]
  (dolist (account (banned-accounts t))
    #H[<li>${(format-account-link account)}</li>])
  #H[</ul>])

(defpage /site/all-articles "All articles" (start)
  (paginate-article-summaries start (store-objects-with-class 'article) #U?))