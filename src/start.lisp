(in-package #:cliki2)

(defparameter *datadir* #P"/var/cliki2/")

(close-store)
(open-store (merge-pathnames "store/" *datadir*))

(init-recent-revisions)

(dolist (unreferenced-uri (set-difference %referenced-uris %defined-uris
                                          :key #'car :test #'string-equal))
  (warn "Reference warning: referencing unknown URI resource ~a in file ~a"
        (car unreferenced-uri) (cdr unreferenced-uri)))

(defvar %acceptor
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port 8080)))
    ;(setf (hunchentoot:acceptor-error-template-directory acceptor) "/home/viper/opt/hunchentoot/www/errors/")
    (hunchentoot:start acceptor)
    acceptor))


