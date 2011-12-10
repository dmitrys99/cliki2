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

(defun load-cliki-store ()
  (close-store)
  (open-store (merge-pathnames "store/" *datadir*))

  (init-recent-revisions)

  (bt:make-thread
   (lambda ()
     (loop (snapshot) (sleep (* 24 60 60))))))

(defun start-cliki-server ()
  (let ((acceptor (make-instance
                   'easy-acceptor
                   :port (or (with-open-file (s (merge-pathnames
                                                 "port" *datadir*)
                                                :if-does-not-exist nil)
                               (read s))
                             8080)
                   :access-log-destination nil
                   :message-log-destination *error-log*)))
    (start acceptor)
    acceptor))
