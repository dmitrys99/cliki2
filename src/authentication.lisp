(in-package #:cliki2)
(in-readtable cliki2)

(defvar *session-secrets* (make-hash-table :test 'equal))

(defun logout ()
  (remhash (cookie-in "cliki2auth") *session-secrets*)
  (set-cookie "cliki2auth" :value "" :path "/"))

(defun hash^2-account-password (account)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    'ironclad:sha256
    (babel:string-to-octets (account-password-digest account)))))

(defun login (account)
  (let ((secret #?"${(make-random-string 20)}.${(hash^2-account-password account)}"))
    (setf (gethash secret *session-secrets*) account)
    (set-cookie "cliki2auth" :value secret :path "/")))

(defun account-auth ()
  (let* ((cookie (cookie-in "cliki2auth")))
    (awhen (gethash cookie *session-secrets*)
      (if (string= (subseq cookie (1+ (position #\. cookie)))
                   (hash^2-account-password it))
          it
          (logout)))))

(defvar *account* nil)

(defmacro with-account (&body body)
  `(let ((*account* (account-auth)))
     ,@body))

(defmethod acceptor-dispatch-request :around ((acceptor easy-acceptor) request)
  (with-account (call-next-method)))
