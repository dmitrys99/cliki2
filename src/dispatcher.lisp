(in-package #:cliki2)
(in-readtable cliki2)

(defun guess-article-name ()
  (uri-decode (subseq (script-name*) 1)))

(defun show-deleted-article-page (article)
  (setf (return-code*)
        404
        *footer*
        (with-output-to-string (*html-stream*)
          #H[<li><a href="$(#/site/history?title={(title article)})">History</a></li>]
          (when (and (not (youre-banned?))
                     (account-is? *account* :moderator :administrator))
            #H[<li><form method="post"
                         action="$(#/site/permadelete?title={(title article)})">
                     <input class="del" type="submit" value="Delete permanently" />
                   </form>
               </li>])))
  #H[Article was deleted.])

(defun render-article (article)
  (let ((*header* #?[<link rel="alternate" type="application/atom+xml" title="edits"
                  href="$(#/site/feed/article.atom?title={(title article)})">]))
    (render-page (title article)
      (if (typep article 'deleted-article)
          (show-deleted-article-page article)
          (render-revision (latest-revision article) (cached-content article))))))

(defun article-dispatcher (request)
  (lambda ()
    (let ((article (find-article-any (guess-article-name))))
      (cond
        ((not article)
         (setf (return-code*) 404)
         (with-account
           (render-page "Article not found"
             #H[<h1>Cliki does not have an article with this exact name</h1>
             <a href="$(#/site/edit-article?title={(guess-article-name)})">Create</a>])))
        ((get-parameter "download" request)
         (redirect (elt
                    (nth-value
                     1
                     (ppcre:scan-to-strings
                      #?/_P\((.*?)\)/ (cached-content article)))
                    0)))
        (t (render-article article))))))

(define-easy-handler (root :uri "/") ()
  (render-article (find-article "index")))

(%defpage /robots.txt :both ()
  "User-agent: *
Disallow: /site/")

(defmethod acceptor-status-message :around ((acceptor easy-acceptor)
                                            status-code &key &allow-other-keys)
  (unless (equal status-code 404)
    (call-next-method)))

(defvar %static-handler
  (create-folder-dispatcher-and-handler
   "/static/"
   (merge-pathnames #p"static/"
                    (asdf:component-pathname (asdf:find-system :cliki2)))))

(setf *dispatch-table*
      (list
       (lambda (request)
         (awhen (funcall %static-handler request)
           (lambda ()
             (setf (header-out :cache-control) "max-age=31536000")
             (funcall it))))
       'dispatch-easy-handlers
       'article-dispatcher))
