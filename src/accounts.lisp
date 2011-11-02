(in-package #:cliki2)
(in-readtable cliki2)

(defclass account (store-object)
  ((name            :initarg       :name
                    :index-type    string-unique-index
                    :index-reader  account-with-name
                    :index-values  all-accounts
                    :reader        name)
   (email           :initarg       :email
                    :index-type    string-unique-index
                    :index-reader  account-with-email
                    :reader        email)
   (role            :initarg       :role
                    :accessor      account-role
                    :type          (member nil :pending :administrator :moderator))
   (password-salt   :initarg       :password-salt
                    :accessor      account-password-salt)
   (password-digest :initarg       :password-digest
                    :accessor      account-password-digest))
  (:metaclass persistent-class))

(defmethod link-to ((account account))
  #/site/account?name=${(account-name account)})

;;; passwords

(let ((kdf (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)))
  (defun password-digest (password salt)
    (ironclad:byte-array-to-hex-string
     (ironclad:derive-key kdf
                          (babel:string-to-octets password :encoding :utf-8)
                          (babel:string-to-octets salt)
                          1000 128))))

(let ((elements "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
  (defun make-random-string (length)
    (let ((string (make-string length)))
      (dotimes (i length)
        (setf (aref string i) (aref elements (random 62))))
      string)))

;;; registration

(defclass invite (store-object)
  ((account  :initarg :account
             :reader invite-account)
   (date     :initform (get-universal-time)
             :reader date)
   (token    :reader invite-token
             :index-type string-unique-index
             :index-reader invite-with-token
             :index-values all-invites))
  (:metaclass persistent-class))

(defmethod shared-initialize :after ((invite invite) slot-names &key &allow-other-keys)
  (let ((account (invite-account invite)))
    (setf (slot-value invite 'token)
          (ironclad:byte-array-to-hex-string
           (ironclad:digest-sequence :SHA1
                                     (babel:string-to-octets
                                      (format nil
                                              "~A~A"
                                              (account-name account)
                                              (account-email account))))))))

(deftransaction confirm-registration (invite)
  (with-transaction ()
    (setf (account-role (invite-account invite)) nil)
    (delete-object invite)))

;; (defpage /site/register "Register" (name email badname bademail badpassword badcaptcha)
;;   (if *session*
;;       (redirect #/)
;;       (progn
;; #H[
;; <div>
;;   <h3>Create account</h3>
;;   <form method="post" action="$(#/site/do-register)">
;;   <table>
;;     <tbody>
;;       <tr>
;;         <td>Name:</td>
;;         <td>]

;;         (when badname
;;                 #H[<div class="error-info">]
;;                 (when (equal badname "empty") #H[Name required])
;;                 (when (equal badname "exists") #H[An account with this name already exists])
;;                 #H[</div>])

;;         #H[<input name="name" size="30" value="${(if name name ""}" />
;;         </td>
;;       </tr>
;;       <tr>
;;         <td>Email:</td>
;;         <td>] (when bademail
;;                 #H[<div class="error-info">]
;;                 (when (equal bademail "bad") #H[Invalid email address])
;;                 (when (equal bademail "exists") #H[Email address already used for another account])
;;                 #H[</div>])
;;         #H[<input name="email" size="30" value="${(if email email "")}" />
;;         </td>
;;       </tr>
;;       <tr>
;;         <td>Password:</td>
;;         <td>]

;;         (when badpassword
;;           #H[<div class="error-info">Password too short</div>])

;;         #H[<input name="password" type="password" size="30" />
;;         <div class="info">Minimum length - 8 characters</div>
;;         </td>
;;         </tr>
;;         </tbody>
;;         </table>

;;         <br />

;;         <div>]
;;           (when badcaptcha
;;             #H[<div class="error-info"> reCaptcha failed </div>])
;;           #H[<script>
;;                 var RecaptchaOptions = { theme : 'clean', lang: 'en' };
;;           </script>

;;           <script type="text/javascript"
;;                     src="http://api.recaptcha.net/challenge?k=${*reCAPTCHA.public-key*}">
;;           </script>

;;           <noscript>
;;                 <iframe src="http://api.recaptcha.net/noscript?k=${*reCAPTCHA.public-key*}"
;;                         height="300" width="500" frameborder="0"></iframe><br />
;;                 <textarea name="recaptcha_challenge_field" rows="3" cols="40">
;;                 </textarea>
;;                 <input type="hidden" name="recaptcha_response_field"
;;                        value="manual_challenge" />
;;           </noscript>
;;         </div>

;;         <br />
;;         <input type="submit" value="Create account" />
;;     </form>
;; </div>]
;; )
;;       )
;;   )

(defun check-register-form (name email password recaptcha-challenge recaptcha-response ip)
  (let ((errors ()))
    (flet ((err (field message)
             (push message errors)
             (push field errors))
           (empty? (x) (or (not x) (string= x ""))))
      (cond ((empty? name)
             (err :name "empty"))
            ((account-with-name name)
             (err :name "exists")))
      (cond ((not (ppcre:scan "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$"
                              (string-downcase email)))
             (err :email "bad"))
            ((account-with-email email)
             (err :email "exists")))
      (cond ((< (length password) 8)
             (err :password "short")))
      (unless (cl-recaptcha:verify-captcha recaptcha-challenge
                                           recaptcha-response
                                           ip
                                           :private-key *reCAPTCHA.private-key*)
        (err :captcha "Bad")))
    errors))

(defhandler /site/do-register (name email password recaptcha_challenge_field recaptcha_response_field)
  (let ((fails (check-register-form name email password recaptcha_challenge_field
                                    recaptcha_response_field (real-remote-addr))))
    (if fails
        #/site/register?name={name}&email={email}&badname={(getf fails :name)}&bademail={(getf fails :email)}&badpassword={(getf fails :password)}&badcaptcha={(getf fails :captcha)}
        (let* ((salt (make-random-string 50))
               (digest (password-digest password salt))
               (invite (make-instance 'invite
                                      :account (make-instance
                                                'account
                                                :name            name
                                                :email           email
                                                :password-salt   salt
                                                :password-digest digest
                                                :role            :pending))))
          (cl-smtp:send-email "cliki.net" "noreply@cliki.net" email
                              "[Do not reply] CLiki2 registration"
                              (format nil
"Someone (hopefully you) has registered for an account with your email address on
CLiki2. To confirm registration, visit this link:
http://cliki.net/site/confirm-registration?token=~A

If you think this message is erroneous, please disregard it."
                                      (invite-token invite)))
          #/site/registration-sent))))

(defpage /site/registration-sent "Registration info sent" ()
  #H[A confirmation email has been sent. Please check your inbox.])

(defpage /site/confirm-registration "Registration confirmed" (token)
         ;; confirm registration
         ;; login and redirect
         (redirect #/))

;;; password recovery

(deftransaction set-password (account new-salt new-digest)
  (setf (account-password-digest account)  new-digest
        (account-password-salt account)    new-salt
        (account-role account)             (if (eq (account-role account) :pending)
                                               nil
                                               (account-role account))))

(defun reset-password (account)
  (let ((salt (make-random-string 50))
        (password (make-random-string 8)))
    (set-password account salt (password-digest password salt))
    (cl-smtp:send-email "cliki.net" "noreply@cliki.net" (email account)
      "[Do not reply] Your new CLiki2 password"
      (format nil
"Someone (hopefully you) requested a password reset for a lost password on CLiki2.
Your new password is: '~A'

If you think this message is erroneous, please contact admin@cliki.net"
              password))))

;;; login

(defhandler /site/signin (name password)
  (if (session-value 'account)
      (referer)
      (let ((account (account-with-name name)))
        (if (and account password
                 (equal (account-password-digest account)
                        (password-digest password (account-password-salt account))))
            (progn (start-session)
                   (setf (session-value 'account) account)
                   (referer))
            #/site/invalid-login))))

(defpage /site/invalid-login "Invalid Login" ()
  #H[Account name and/or password is incorrect])

(defpage /site/signout () ()
  (remove-session *session*)
  (redirect #/))

(defpage /site/account (format nil "Account: ~A" name) (name)
  ;list edits done by this account
         )
