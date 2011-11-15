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

(open-search-index)

(init-recent-revisions)

(dolist (unreferenced-uri (set-difference %referenced-uris %defined-uris
                                          :key #'car :test #'string-equal))
  (warn "Reference warning: referencing unknown URI resource ~a in file ~a"
        (car unreferenced-uri) (cdr unreferenced-uri)))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))
