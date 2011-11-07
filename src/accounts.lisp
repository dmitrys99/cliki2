(in-package #:cliki2)
(in-readtable cliki2)

(defclass account (store-object)
  ((name            :initarg       :name
                    :reader        name
                    :index-type    string-unique-index
                    :index-reader  account-with-name
                    :index-values  all-accounts)
   (email           :initarg       :email
                    :reader        email
                    :index-type    string-unique-index
                    :index-reader  account-with-email)
   (password-salt   :initarg       :password-salt
                    :accessor      account-password-salt)
   (password-digest :initarg       :password-digest
                    :accessor      account-password-digest)
   (role            :type          (member nil :administrator :moderator)
                    :accessor      account-role
                    :index-type    hash-index
                    :index-reader  accounts-by-role))
  (:metaclass persistent-class))

(defmethod link-to ((account account))
  #/site/account?name=${(name account)})

;;; passwords

(let ((kdf (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)))
  (defun password-digest (password salt)
    (ironclad:byte-array-to-hex-string
     (ironclad:derive-key kdf
                          (babel:string-to-octets password :encoding :utf-8)
                          (babel:string-to-octets salt)
                          1000 128))))

(let ((an "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
  (defun make-random-string (length)
    (map-into (make-string length) (lambda () (aref an (random 62))))))

;;; registration

(defpage /site/register "Register" (name email badname bademail badpassword)
  (if *session*
      (redirect #/)
      (progn
#H[
<div>
  <h3>Create account</h3>
  <form method="post" action="$(#/site/do-register)">
  <table>
    <tbody>
      <tr>
        <td>Name:</td>
        <td>]

        (when badname
                #H[<div class="error-info">]
                (when (equal badname "empty") #H[Name required])
                (when (equal badname "exists") #H[An account with this name already exists])
                #H[</div>])

        #H[<input name="name" size="30" value="${(if name name "")}" />
        </td>
      </tr>
      <tr>
        <td>Email:</td>
        <td>] (when bademail
                #H[<div class="error-info">]
                (when (equal bademail "bad") #H[Invalid email address])
                (when (equal bademail "exists") #H[Email address already used for another account])
                #H[</div>])
        #H[<input name="email" size="30" value="${(if email email "")}" />
        </td>
      </tr>
      <tr>
        <td>Password:</td>
        <td>]

          (when badpassword
            #H[<div class="error-info">Password too short</div>])

          #H[<input name="password" type="password" size="30" />
          <div class="info">Minimum length - 8 characters</div>
        </td>
      </tr>
    </tbody>
  </table>

  <br />
  <input type="submit" value="Create account" />
  </form>
</div>])))

(defun check-register-form (name email password)
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
             (err :password "short"))))
    errors))

(defhandler /site/do-register (name email password)
  (aif (check-register-form name email password)
       #/site/register?name={name}&email={email}&badname={(getf it :name)}&bademail={(getf it :email)}&badpassword={(getf it :password)}
       (let* ((salt (make-random-string 50))
              (account (make-instance
                        'account
                        :name            name
                        :email           email
                        :password-salt   salt
                        :password-digest (password-digest password salt))))
         ;; login
         #/)))

;;; password recovery

(deftransaction set-password (account new-salt new-digest)
  (setf (account-password-digest account) new-digest
        (account-password-salt account)   new-salt))

(defun reset-password (account)
  (let ((salt (make-random-string 50))
        (password (make-random-string 8)))
    (set-password account salt (password-digest password salt))
    (cl-smtp:send-email "cliki.net" "admin@cliki.net" (email account)
      "Your new CLiki password"
      (format nil
"Someone (hopefully you) requested a password reset for a lost password on CLiki.
Your new password is: '~A'

If you think this message is erroneous, please contact admin@cliki.net"
              password))))

(defhandler /site/reset-password (name)
  (aif (or (account-with-name name) (account-with-email name))
       (progn (reset-password it) #/site/reset-ok)
       #/site/cantfind?name={name}))

(defpage /site/cantfind "Account does not exist" (name)
  #H[Account with name or email '${name}' doesn't exist])

(defpage /site/reset-ok "Password reset successfully" ()
  #H[Password reset successfully. Check your inbox.])

;;; login

(defun account-login (account)
  (start-session)
  (setf (session-value 'account) account))

(defhandler /site/signin (name password)
  (if (session-value 'account)
      (referer)
      (let ((account (account-with-name name)))
        (if (and account password
                 (equal (account-password-digest account)
                        (password-digest password (account-password-salt account))))
            (progn (account-login account) (referer))
            #/site/invalid-login))))

(defpage /site/invalid-login "Invalid Login" ()
  #H[Account name and/or password is incorrect])

(defpage /site/signout () ()
  (remove-session *session*)
  (redirect #/))

(defpage /site/account (format nil "Account: ~A" name) (name)
  ;list edits done by this account
         )
