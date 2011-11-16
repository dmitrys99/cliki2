(in-package #:cliki2)
(in-readtable cliki2)

(defclass account (store-object)
  ((name            :initarg       :name
                    :reader        name
                    :index-type    string-unique-index
                    :index-reader  find-account
                    :index-values  all-accounts)
   (email           :initarg       :email
                    :accessor      email)
   (password-salt   :initarg       :password-salt
                    :accessor      account-password-salt)
   (password-digest :initarg       :password-digest
                    :accessor      account-password-digest)
   (role            :initarg       :role
                    :initform      nil
                    :type          (member nil :administrator :moderator :anonymous :banned)
                    :accessor      role
                    :index-type    hash-index
                    :index-reader  accounts-by-role))
  (:metaclass persistent-class))

(defmethod link-to ((account account))
  #/site/account?name={(name account)})

;;; passwords

(let ((kdf (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)))
  (defun password-digest (password salt)
    (ironclad:byte-array-to-hex-string
     (ironclad:derive-key kdf
                          (babel:string-to-octets password :encoding :utf-8)
                          (babel:string-to-octets salt)
                          1000 128))))

(let ((AN "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
  (defun make-random-string (length)
    (map-into (make-string length) (lambda () (aref AN (random 62))))))

;;; registration

(defun maybe-show-form-error (error expected-error message)
  (when (equal error expected-error)
    #H[<div class="error-info">${message}</div>]))

(defpage /site/register "Register" (name email error)
  (if *account*
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
        (maybe-show-form-error error "name" "Name required")
        (maybe-show-form-error error "nametaken"
                               "An account with this name already exists")
        #H[<input name="name" size="30" value="${(if name name "")}" />
        </td>
      </tr>
      <tr>
        <td>Email:</td>
        <td>]
        (maybe-show-form-error error "email" "Invalid email address")
        #H[<input name="email" size="30" value="${(if email email "")}" />
        </td>
      </tr>
      <tr>
        <td>Password:</td>
        <td>]
          (maybe-show-form-error error "password" "Password too short")
          #H[<input name="password" type="password" size="30" />
          <div class="info">Minimum length - 6 characters</div>
        </td>
      </tr>
    </tbody>
  </table>

  <br />
  <input type="submit" value="Create account" />
  </form>
</div>])))

(defun email-address? (str)
  (ppcre:scan "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$"
              (string-downcase str)))

(defhandler /site/do-register (name email password)
  (aif (cond ((or (not name) (string= name "")) "name")
             ((find-account name) "nametaken")
             ((not (email-address? email)) "email")
             ((< (length password) 6) "password"))
       #/site/register?name={name}&email={email}&error={it}
       (let* ((salt (make-random-string 50))
              (account (make-instance
                        'account
                        :name            name
                        :email           email
                        :password-salt   salt
                        :password-digest (password-digest password salt))))
         (login account)
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
#?"Someone (hopefully you) requested a password reset for a lost password on CLiki.
Your new password is: ${password}

If you think this message is erroneous, please contact admin@cliki.net")))

(defpage /site/reset-ok "Password reset successfully" ()
  #H[Password reset successfully. Check your inbox.])

;;; login

(defun check-password (password account)
  (and password (not (equal "" password))
       (equal (account-password-digest account)
              (password-digest password (account-password-salt account)))))

(defhandler /site/login (name password reset-pw)
  (let ((account (find-account name)))
    (cond (*account* (referer))
          ((not account) #/site/cantfind?name={name})
          (reset-pw (reset-password account) #/site/reset-ok)
          ((check-password password account) (login account) (referer))
          (t #/site/invalid-login))))

(defpage /site/invalid-login "Invalid Login" ()
  #H[Account name and/or password is incorrect])

(defpage /site/cantfind "Account does not exist" (name)
  #H[Account with name '${name}' doesn't exist])

(defpage /site/logout () ()
  (logout)
  (redirect #/))

;;; user page

(defpage /site/account #?"Account: ${name}" (name)
  (aif (find-account name)
       (progn
         #H[<h1>${name} account info page</h1>]
         (flet ((ban-by (who)
                  (when (and *account* (find (role *account*) who))
                    #H[<form method="post" action="$(#/site/ban?name={name})">
                    <input type="submit" value="Ban" /></form>])))
           (if (and *account* (equal name (name *account*)))
               #H[<a href="$(#/site/preferences)">Edit preferences</a>]
               (case (role it)
                 (:administrator #H[<em>Administrator</em>])
                 (:banned #H[<em>banned user</em>])
                 (:moderator #H[<em>Moderator</em>] (ban-by '(:administrator)))
                 (:anonymous (ban-by '(:administrator :moderator)))
                 ((nil) (ban-by '(:administrator :moderator))
                  (when (eq (role *account*) :administrator)
                    #H[<br /><form method="post" action="$(#/site/make-moderator?name={name})">
                    <input type="submit" value="Make moderator" /></form>])))))
         #H[<br />User page: ] (pprint-article-link name)
         #H[<br />Edits by ${name}: <ul>]
         (dolist (r (revisions-by-author it))
           #H[<li>]
           (pprint-article-link (title (article r))) #H[ ]
           (pprint-revision-link r)
           #H[ (<em>${(summary r)}</em>)</li>])
         #H[</ul>])
       (redirect #/site/cantfind?name={name})))

;;; user preferences

(defpage /site/preferences-ok "Preferences updated" ()
  #H[Email updated successfully])

(defhandler /site/change-email (email password)
  (flet ((err (e) #/site/preferences?email={email}&error={e}))
    (cond ((not *account*) #/)
          ((not (email-address? email)) (err "email"))
          ((check-password password *account*) (with-transaction ("change email")
                                                 (setf (email *account*) email))
           #/site/preferences-ok)
          (t (err "pw")))))

(defpage /site/preferences "Account preferences" (email error)
  (if *account*
      (progn
        #H[<form method="post" action="$(#/site/change-email)">
        New email: <input type="text" name="email" title="new email"
                          value="${(if email email "")}" />]
        (maybe-show-form-error error "email" "Bad email address")
        #H[<br />Confirm password: <input type="password" name="password" />]
        (maybe-show-form-error error "pw" "Bad password")
        #H[<br /><input type="submit" value="change email" />
        </form>])
      (redirect #/)))

;;; anonymous

(defun get-anonymous-account (ip)
  (or (find-account ip) (make-instance 'account
                                       :role            :anonymous
                                       :name            ip
                                       :email           ""
                                       :password-salt   "xxxx"
                                       :password-digest "yyyy")))

;;; moderation

(deftransaction change-role (account role)
  (setf (role account) role))

(defmacro moderator-handler (uri admin-roles role)
  `(defhandler ,uri ()
     (let ((name (get-parameter "name")))
       (acond ((not (and *account* (find (role *account*) ',admin-roles))) (referer))
              ((find-account name) (change-role it ,role) (referer))
              (t #/site/cantfind?name={name})))))

(moderator-handler /site/make-moderator (:administrator) :moderator)

(moderator-handler /site/ban (:moderator :administrator) :banned)
