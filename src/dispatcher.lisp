(in-package #:cliki2)
(in-readtable cliki2)

(defun guess-article-name ()
  (subseq (url-decode (script-name*)) 1))

(defun show-deleted-article-page (article)
  (setf (return-code*)
        404
        *footer*
        (with-output-to-string (*html-stream*)
          #H[<li><a href="$(#/site/history?article={ (article-title article) })">History</a></li>]
          (when (and (not (youre-banned?)) (account-admin *account*))
            #H[<li>
                 <form method="post"
                       action="$(#/site/permadelete?title={ (article-title article) })">
                     <input class="del" type="submit" value="Delete permanently" />
                   </form>
               </li>])))
  #H[Article was deleted])

(defun render-article (article)
  (let ((*header* #?[<link rel="alternate" type="application/atom+xml" title="ATOM feed of edits to current article"
                  href="$(#/site/feed/article.atom?title={ (article-title article) })">]))
    (render-page (article-title article)
      (if (deleted? article)
          (show-deleted-article-page article)
          (render-revision (latest-revision article)
                           (cached-content (article-title article)))))))

(defun article-dispatcher (request)
  (lambda ()
    (let ((article (find-article (guess-article-name))))
      (cond
        ((not article)
         (setf (return-code*) 404)
         (with-account
           (render-page "Article not found"
             #H[<h1>Article not found</h1>
             <a href="$(#/site/edit-article?title={(guess-article-name)})">Create</a>])))
        ((get-parameter "download" request)
         (redirect (elt
                    (nth-value
                     1
                     (ppcre:scan-to-strings
                      #?/_P\((.*?)\)/ (cached-content (article-title article))))
                    0)))
        (t (render-article article))))))

(defmethod acceptor-status-message :around ((acceptor cliki2-acceptor)
                                            status-code &key &allow-other-keys)
  (unless (and (equal status-code 404) (not (boundp '*wiki*)))
    (call-next-method)))

(define-easy-handler (root :uri "/") ()
  (render-article (find-article "index")))

(%defpage /robots.txt :both ()
  "User-agent: *
Disallow: /site/")

(defun wiki-static-dispatcher ()
  (create-prefix-dispatcher
   "/static/"
   (lambda ()
     (let ((request-path (request-pathname *request* "/static/")))
       (setf (header-out :cache-control) "max-age=31536000")
       (handle-static-file
        (merge-pathnames request-path (wiki-path "static/")))))))
