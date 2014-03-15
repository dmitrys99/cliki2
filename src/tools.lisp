(in-package #:cliki2)
(in-readtable cliki2)

(defpage /site/tools "Tools" ()
  #H[<h2>Инструменты</h2>
  <ul>
  <li><a href="$(#/site/all-articles)">Все статьи</a></li>
  <li><a href="$(#/site/all-topics)">Все разделы</a></li>
  <li><a href="$(#/site/uncategorized)">Статьи без категории</a></li>
  <li><a href="$(#/site/deleted-articles)">Удаленные статьи</a></li>
  <li><a href="$(#/site/blacklist)">Черный список аккаунтов/IP</a></li>
  </ul>])

(defpage /site/blacklist "Чёрный список" ()
  #H[<h3>Блокированные аккаунты/IP</h3>
  <ul>]
  (dolist (banned (get-blacklist))
    #H[<li>${ (account-link banned) }</li>])
  #H[</ul>])

(defpage /site/all-articles "Все статьи" (start)
  (paginate-article-summaries
   start
   (get-all-articles (complement #'deleted?))))

(defpage /site/deleted-articles "Удаленные статьи" (start)
  (paginate-article-summaries
   start
   (get-all-articles #'deleted?)))

(defpage /site/uncategorized "Статьи без категории" (start)
  (paginate-article-summaries
   start
   (get-all-articles (lambda (article)
                       (not (or (deleted? article)
                                (topics (cached-content
                                         (article-title article)))))))))

(defpage /site/all-topics "Все маркеры разделов" ()
  #H[<ul>]
  (dolist (topic (sort (all-topics) #'string-lessp))
    #H[<li>] (pprint-topic-link topic) #H[</li>])
  #H[</ul>])
