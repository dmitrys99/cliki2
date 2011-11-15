(in-package #:cliki2)

(close-store)
(open-store (merge-pathnames "store/" *datadir*))

(setf *dispatch-table*
      (list
       (hunchentoot:create-folder-dispatcher-and-handler
        "/static/"
        (merge-pathnames #p"static/"
                         (asdf:component-pathname (asdf:find-system :cliki2))))
       'dispatch-easy-handlers
       'article-dispatcher))

(defmethod acceptor-status-message :around ((acceptor easy-acceptor) status-code &key &allow-other-keys) ;; blah, hunchentoot 1.2 is annoying
  (if (equal status-code 404)
      (not-found-page)
      (call-next-method)))

(defmethod acceptor-dispatch-request :around ((acceptor easy-acceptor) request)
  (let ((*account* (session-value 'account)))
    (call-next-method)))

(open-search-index)

(init-recent-revisions)

(dolist (unreferenced-uri (set-difference %referenced-uris %defined-uris
                                          :key #'car :test #'string-equal))
  (warn "Reference warning: referencing unknown URI resource ~a in file ~a"
        (car unreferenced-uri) (cdr unreferenced-uri)))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))
