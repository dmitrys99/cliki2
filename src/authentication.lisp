(in-package #:cliki2)
(in-readtable cliki2)

(defvar *session-secrets*      (make-hash-table :test 'equal))
(defvar *session-secrets-lock* (make-lock))

(defun logout ()
  (with-lock-held (*session-secrets-lock*)
    (remhash (cookie-in "cliki2auth") *session-secrets*))
  (set-cookie "cliki2auth" :value "" :path "/")
  nil)

(defun hash^2-account-password (salt account)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    'ironclad:sha256
    (flexi-streams:string-to-octets
     (concatenate 'string (account-password-digest account) salt)
     :external-format :utf8))))

(defun login (account)
  (let* ((salt   (make-random-string 20))
         (secret #?"${salt}.${(hash^2-account-password salt account)}"))
    (with-lock-held (*session-secrets-lock*)
      (setf (gethash secret *session-secrets*) account))
    (set-cookie "cliki2auth" :value secret :path "/"
                :expires (+ (get-universal-time) (* 60 60 24 180))))) ;;6 months

(defun account-auth ()
  (let* ((cookie (cookie-in "cliki2auth"))
         (dot    (position #\. cookie)))
    (awhen (with-lock-held (*session-secrets-lock*)
             (gethash cookie *session-secrets*))
      (if (string= (subseq cookie (1+ dot))
                   (hash^2-account-password (subseq cookie 0 dot) it))
          it
          (logout)))))

(defvar *account* nil)

(defmacro with-account (&body body)
  `(let ((*account* (account-auth)))
     ,@body))

(defmethod acceptor-dispatch-request :around ((acceptor cliki2-acceptor) request)
  (declare (ignorable request))
  (with-account (call-next-method)))
