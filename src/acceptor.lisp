(in-package #:cliki2)

(defclass cliki2-acceptor (easy-acceptor)
  ((dispatch-table   :reader dispatch-table    :initarg :dispatch-table)
   ;; config slots
   (wiki-home        :reader wiki-home         :initarg :wiki-home)
   (wiki-name        :reader wiki-name         :initarg :wiki-name)
   (wiki-description :reader wiki-description  :initarg :wiki-description)
   (reminder-email   :reader password-reminder-email-address
                     :initarg :password-reminder-email-address)))

(defmethod acceptor-dispatch-request ((acceptor cliki2-acceptor) request)
  (loop for dispatcher in (dispatch-table acceptor)
     for action = (funcall dispatcher request)
     when action return (funcall action)
     finally (call-next-method)))
