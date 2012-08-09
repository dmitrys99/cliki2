(in-package #:cliki2)
(in-readtable cliki2)

(defstruct session
  account
  expires-at
  password-digest)

(defun logout ()
  (with-lock-held ((session-lock *wiki*))
    (remhash (cookie-in "cliki2auth") (sessions *wiki*)))
  (set-cookie "cliki2auth" :value "" :path "/")
  nil)

(defun expire-old-sessions (wiki)
  (with-lock-held ((session-lock wiki))
    (loop for x being the hash-key of (sessions wiki)
          using (hash-value session) do
          (when (< (session-expires-at session) (get-universal-time))
            (remhash x (sessions wiki))))))

(defun next-expiry-time ()
  (+ (get-universal-time) (* 60 60 24 180)))

(defun login (account)
  (let (secret)
    (with-lock-held ((session-lock *wiki*))
      (loop while (gethash (setf secret (make-random-string 60))
                           (sessions *wiki*)))
      (setf (gethash secret (sessions *wiki*))
            (make-session :account         account
                          :expires-at      (next-expiry-time)
                          :password-digest (account-password-digest account))))
    (set-cookie "cliki2auth" :value secret :path "/" :expires (next-expiry-time))))

(defun account-auth ()
  (let* ((secret (cookie-in "cliki2auth")))
    (awhen (with-lock-held ((session-lock *wiki*))
             (gethash secret (sessions *wiki*)))
      (if (and (< (get-universal-time) (session-expires-at it))
               (string= (account-password-digest (session-account it))
                        (session-password-digest it)))
          (progn (setf (session-expires-at it) (next-expiry-time))
                 (session-account it))
          (logout)))))

;;; captcha

(defvar captcha-ops '(floor ceiling truncate round))

(defun make-captcha ()
  (list (elt captcha-ops (random (length captcha-ops)))
        (- (random 40) 20)
        (1+ (random 10))))

(defun emit-captcha-inputs (captcha class size)
  #H[<input class="${class}" name="captcha-answer" size="${size}" />
     <input type="hidden" name="captcha-op" value="${(elt captcha 0)}" />
     <input type="hidden" name="captcha-x"  value="${(elt captcha 1)}" />
     <input type="hidden" name="captcha-y"  value="${(elt captcha 2)}" />])

(defun check-captcha ()
  (let ((x      (parse-integer (parameter "captcha-x")      :junk-allowed t))
        (y      (parse-integer (parameter "captcha-y")      :junk-allowed t))
        (answer (parse-integer (parameter "captcha-answer") :junk-allowed t))
        (op     (find (parameter "captcha-op") captcha-ops
                      :key #'symbol-name :test #'string-equal)))
    (when (and op x y answer)
      (= (funcall op x y) answer))))
