(in-package #:cliki2)

(defparameter *datadir* #P"/var/cliki2/")

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

(defvar %snapshot-thread
  (bt:make-thread
   (lambda ()
     (loop (snapshot) (sleep (* 24 60 60))))))

(defvar %acceptor
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port 8080)))
    (hunchentoot:start acceptor)
    acceptor))


