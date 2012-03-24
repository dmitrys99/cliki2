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

(defun load-cliki-store (wiki-home)
  (close-store)
  (setf *article-directory* wiki-home)
  (open-store (merge-pathnames "store/" wiki-home))

  (init-recent-revisions)

  (bt:make-thread
   (lambda ()
     (loop (sleep (* 7 24 60 60)) (snapshot)))))

(defun start-cliki-server (&rest config-opts
                           &key port wiki-home wiki-name wiki-description
                           password-reminder-email-address)
  (declare (ignorable port wiki-name wiki-description
                      password-reminder-email-address))
  ;; SBCL, CCL and possibly others always start w/same pseudo-random seed
  (setf *random-state* (make-random-state t))
  ;; set up empty file for diff
  (open (merge-pathnames "empty_file" wiki-home)
        :direction :probe :if-does-not-exist :create)
  ;; set up HyperSpec paths
  (setf clhs-lookup::*hyperspec-pathname*
        (merge-pathnames "HyperSpec/" wiki-home)
        clhs-lookup::*hyperspec-map-file*
        (merge-pathnames "HyperSpec/Data/Symbol-Table.text" wiki-home)
        clhs-lookup::*hyperspec-root* "/site/HyperSpec/")
  (let ((error-log (merge-pathnames "error-log" wiki-home)))
    (open error-log :direction :probe :if-does-not-exist :create)
    (let ((acceptor
           (apply #'make-instance
                  'cliki2-acceptor
                  :input-chunking-p         nil
                  :persistent-connections-p nil
                  :access-log-destination   nil
                  :message-log-destination  error-log
                  :dispatch-table
                  (list
                   (caching-dispatcher
                    (create-folder-dispatcher-and-handler
                     "/static/" (merge-pathnames #p"static/" wiki-home)))
                   (caching-dispatcher
                    (create-folder-dispatcher-and-handler
                     "/site/HyperSpec/" (merge-pathnames #p"HyperSpec/" wiki-home)))
                   (create-static-file-dispatcher-and-handler
                    "/site/error-log" error-log "text/plain")
                   'dispatch-easy-handlers
                   'article-dispatcher)
                  config-opts)))
      (start acceptor)
      acceptor)))
