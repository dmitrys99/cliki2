(in-package #:cliki2)

(defparameter *datadir* #P"/var/cliki2/")

(setf clhs-lookup::*hyperspec-pathname*
      (merge-pathnames "HyperSpec/" *datadir*)
      clhs-lookup::*hyperspec-map-file*
      (merge-pathnames "HyperSpec/Data/Symbol-Table.text" *datadir*))

(close-store)
(open-store (merge-pathnames "store/" *datadir*))

(init-recent-revisions)

(dolist (unreferenced-uri (set-difference %referenced-uris %defined-uris
                                          :key #'car :test #'string-equal))
  (warn "Reference warning: referencing unknown URI resource ~a in file ~a"
        (car unreferenced-uri) (cdr unreferenced-uri)))

;; BKNR's policy of storing *random-state*, besides being annoying in
;; SBCL, is also a security hole
(defmethod bknr.datastore::ensure-store-random-state :around ((store store))
  (bknr.datastore::initialize-store-random-state store))

(defparameter *blank-file*
  (let ((pathname (merge-pathnames "cliki2blankfile" *datadir*)))
    (open pathname :direction :probe :if-does-not-exist :create)
    pathname))

(defparameter *error-log*
  (merge-pathnames "error-log" *datadir*))

(defvar %snapshot-thread
  (bt:make-thread
   (lambda ()
     (loop (snapshot) (sleep (* 24 60 60))))))

(defvar %acceptor
  (let ((acceptor (make-instance
                   'hunchentoot:easy-acceptor
                   :port (or (with-open-file (s (merge-pathnames
                                                 "port" *datadir*)
                                                :if-does-not-exist nil)
                               (read s))
                             8080)
                   :access-log-destination nil
                   :message-log-destination *error-log*)))
    (hunchentoot:start acceptor)
    acceptor))

(push (create-static-file-dispatcher-and-handler
       "/site/error-log" *error-log* "text/plain")
      *dispatch-table*)
