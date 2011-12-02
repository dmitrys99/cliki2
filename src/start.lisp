(in-package #:cliki2)

(defparameter *datadir* #P"/var/cliki2/")

(setf clhs-lookup::*hyperspec-pathname* #P"/var/cliki2/HyperSpec/"
      clhs-lookup::*hyperspec-map-file* #P"/var/cliki2/HyperSpec/Data/Symbol-Table.text")

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
  (let ((acceptor (make-instance
                   'hunchentoot:easy-acceptor
                   :port 8080
                   :access-log-destination nil
                   :message-log-destination #p"/var/cliki2/error-log")))
    (hunchentoot:start acceptor)
    acceptor))
