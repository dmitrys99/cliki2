(in-package #:cliki2)
(in-readtable cliki2)

(defun print-link-list (links printer)
  #H[<ul>]
  (dolist (link links)
    #H[<li>] (funcall printer link))
  #H[</ul>])

(defpage /site/backlinks () (article)
  (let* ((article-obj (find-article article :error t))
         (content     (cached-content article)))
    (setf *title*  #?'Link information for "${ (article-title article-obj) }"'
          *footer* (article-footer (latest-revision article-obj)))

    #H[<h1>Link information for ] (pprint-article-link article) #H[</h1>
    Topics:] (print-link-list (topics content) #'pprint-topic-link)
    #H[Links to other articles:]
    (print-link-list (page-links content) #'pprint-article-link)
    #H[Links from other articles:]
    (print-link-list (article-backlinks article) #'pprint-article-link)
    #H[Articles in '${ article }' topic:]
    (print-link-list (articles-by-topic article) #'pprint-article-link)))
