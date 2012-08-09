(in-package #:cliki2)

(defvar *wiki*)
(defvar *account* nil)

(defclass cliki2-acceptor (easy-acceptor)
  ((dispatch-table :reader dispatch-table :initarg :dispatch-table)
   (wikis          :reader wikis          :initarg :wikis)))

(defmethod acceptor-dispatch-request ((acceptor cliki2-acceptor) request)
  (let* ((host      (subseq (host) 0 (or (position #\: (host)) (length (host)))))
         (*wiki*    (cadr (assoc host (wikis acceptor) :test #'string=)))
         (*account* (account-auth)))
    (if *wiki*
        (loop for dispatcher in (dispatch-table acceptor)
              for action = (funcall dispatcher request)
              when action return (funcall action)
              finally (call-next-method))
        (call-next-method))))
