(in-package #:cliki2)
(in-readtable cliki2)

(defclass anonymous (store-object)
  ((name            :initarg       :name
                    :reader        name
                    :index-type    string-unique-index
                    :index-reader  find-account)
   (banned?         :initform      nil
                    :accessor      banned?
                    :index-type    hash-index
                    :index-reader  banned-accounts))
  (:metaclass persistent-class))

(deftransaction set-ban (account status)
  (setf (banned? account) status))

(defclass account (anonymous)
  ((email           :initarg       :email
                    :accessor      email)
   (password-salt   :initarg       :password-salt
                    :accessor      account-password-salt)
   (password-digest :initarg       :password-digest
                    :accessor      account-password-digest)
   (role            :initform      nil
                    :type          (member nil :administrator :moderator)
                    :accessor      role))
  (:metaclass persistent-class))

(deftransaction set-role (account role)
  (setf (role account) role))

(defmethod link-to ((account anonymous))
  #/site/account?name={(name account)})

(defun format-account-link (account)
  #?[<a class="internal" href="${(link-to account)}">${(name account)}</a>])

(defun account-is? (account &rest roles)
  (and account (find (role account) roles)))

;;; passwords

(let ((kdf (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)))
  (defun password-digest (password salt)
    (ironclad:byte-array-to-hex-string
     (ironclad:derive-key kdf
                          (flexi-streams:string-to-octets
                           password :external-format :utf-8)
                          (flexi-streams:string-to-octets
                           salt :external-format :utf-8)
                          1000 128))))

(let ((AN "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
  (defun make-random-string (length)
    (map-into (make-string length) (lambda () (aref AN (random 62))))))

;;; registration

(defun maybe-show-form-error (error expected-error message)
  (when (equal error expected-error)
    #H[<div class="error-info">${message}</div>]))

(defpage /site/register "Register" (name email error)
  (if (or *account* (youre-banned?))
      (redirect #/)
      (progn
#H[
<div>
  <h3>Create account</h3>
  <form id="registration" class="prefs" method="post" action="$(#/site/do-register)">
  <dl>]
  (maybe-show-form-error error "name" "Name required")
  (maybe-show-form-error error "nametaken"
                         "An account with this name already exists")
    #H[<dt><label for="name">Name:</label></dt>
    <dd><input class="regin" name="name" size="30" value="${(if name name "")}" /></dd>]
    (maybe-show-form-error error "email" "Invalid email address")
    #H[<dt><label for="email">Email:</label></dt>
    <dd><input class="regin" name="email" size="30" value="${(if email email "")}" /></dd>]
    (maybe-show-form-error error "password" "Password too short")
    #H[<dt><label for="password">Password:</label></dt>
    <dd><input class="regin" name="password" type="password" size="30" /></dd>

    <dt /><dd><input type="submit" value="Create account" /></dd>
  </dl>
  </form>
</div>])))

(defun email-address? (str)
  (ppcre:scan "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$"
              (string-downcase str)))

(defhandler /site/do-register (name email password)
  (let ((name (if name (escape-for-html name) "")))
    (acond
      ((youre-banned?) #/)
      ((cond ((or (not name) (string= name "")) "name")
             ((find-account name) "nametaken")
             ((not (email-address? email)) "email")
             ((< (length password) 6) "password"))
       #/site/register?name={name}&email={email}&error={it})
      (t (let* ((salt (make-random-string 50))
                (account (make-instance
                          'account
                          :name            name
                          :email           email
                          :password-salt   salt
                          :password-digest (password-digest password salt))))
           (login account)
           #/)))))

;;; password recovery

(deftransaction set-password (account new-salt new-digest)
  (setf (account-password-digest account) new-digest
        (account-password-salt account)   new-salt))

(defun reset-password (account)
  (let ((salt (make-random-string 50))
        (password (make-random-string 8)))
    (set-password account salt (password-digest password salt))
    (cl-smtp:send-email "localhost"
                        (password-reminder-email-address *acceptor*)
                        (email account)
      #?"Your new ${(wiki-name *acceptor*)} wiki password"
#?"Someone (hopefully you) requested a password reset for a lost password on the ${(wiki-name *acceptor*)} wiki.
Your new password is: ${password}")))

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

(defun youre-banned? ()
  (or (aand (find-account (real-remote-addr)) (banned? it))
      (aand *account* (banned? it))))

(defpage /site/account #?"Account: ${name}" (name)
  (aif (find-account name)
       (progn
         #H[<h1>${name} account info page</h1>]
         (when *account*
           (flet ((ban (&key (by-who '(:administrator :moderator)) (un ""))
                    (when (and (not (youre-banned?))
                               (apply #'account-is? *account* by-who))
                      #H[<form method="post" action="$(#/site/{un}ban?name={name})">
                      <input type="submit" value="${un}ban" /></form>])))
             (cond ((equal name (name *account*))
                    #H[<a href="$(#/site/preferences)">Edit preferences</a>])
                   ((banned? it) #H[<em>banned user</em>] (ban :un "un"))
                   ((eql (type-of it) 'anonymous) (ban))
                   (t (case (role it)
                        (:administrator #H[<em>Administrator</em>])
                        (:moderator #H[<em>Moderator</em>]
                                    (ban :by-who '(:administrator)))
                        (t
                         (ban)
                         (when (eq (role *account*) :administrator)
                           #H[<br /><form method="post" action="$(#/site/make-moderator?name={name})">
                           <input type="submit" value="Make moderator" /></form>])))))))
         #H[<br />User page: ] (pprint-article-link name)
         #H[<br />Edits by ${name}: <ul>]
         (map nil #'render-revision-summary (revisions-by-author it))
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
        #H[<h3>Change account preferences</h3>
        <form id="changemail" class="prefs" method="post"
                 action="$(#/site/change-email)">
        <dl>]
          (maybe-show-form-error error "email" "Bad email address")
          #H[<dt><label for="email">New email:</label></dt>
          <dd><input class="regin" type="text" name="email" title="new email"
                     value="${(if email email "")}" /></dd>]
          (maybe-show-form-error error "pw" "Wrong password")
          #H[<dt><label for="password">Confirm password:</label></dt>
          <dd><input class="regin" type="password" name="password" /></dd>
          <dt /><dd><input type="submit" value="change email" /></dd>
        </dl>
      </form>])
      (redirect #/)))

;;; anonymous

(defun get-anonymous-account (ip)
  (or (find-account ip) (make-instance 'anonymous :name ip)))

;;; moderation

(defmacro moderator-handler (uri action &rest authorized-roles)
  `(defhandler ,uri ()
     (let ((name (get-parameter "name")))
       (acond ((youre-banned?) #/)
              ((not (account-is? *account* ,@authorized-roles)) (referer))
              ((find-account name) ,action (referer))
              (t #/site/cantfind?name={name})))))

(moderator-handler /site/make-moderator (set-role it :moderator) :administrator)

(moderator-handler /site/ban (set-ban it t) :moderator :administrator)

(moderator-handler /site/unban (set-ban it nil) :moderator :administrator)
