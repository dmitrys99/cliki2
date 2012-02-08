(in-package #:cliki2)

;;; unreferenced uri checking
(dolist (unreferenced-uri (set-difference %referenced-uris %defined-uris
                                          :key #'car :test #'string-equal))
  (warn "Reference warning: referencing unknown URI resource ~a in file ~a"
        (car unreferenced-uri) (cdr unreferenced-uri)))

;; BKNR's policy of storing *random-state*, besides being annoying in
;; SBCL, is also a security hole
(defmethod bknr.datastore::ensure-store-random-state :around ((store store))
  (bknr.datastore::initialize-store-random-state store))

;; SBCL and CCL use same random on each startup
(setf *random-state* (make-random-state t))

(defun load-cliki-store ()
  (close-store)
  (open-store (merge-pathnames "store/" *datadir*))

  (init-recent-revisions)

  (bt:make-thread
   (lambda ()
     (loop (sleep (* 7 24 60 60)) (snapshot)))))

(defun start-cliki-server ()
  (let ((acceptor (make-instance
                   'easy-acceptor
                   :port (read-config-file "port")
                   :access-log-destination nil
                   :message-log-destination *error-log*)))
    (start acceptor)
    acceptor))
