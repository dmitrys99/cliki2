(in-package #:cliki2)
(in-readtable cliki2)

(defstruct (account (:type list))
  name
  email
  password-salt
  password-digest
  (admin nil         :type (member nil :administrator :moderator)))

(defun account-link (account-name)
  #?[<a class="internal" href="${ #/site/account?name={account-name} }">${ account-name }</a>])

;;; passwords

(let ((kdf (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)))
  (defun password-digest (password salt)
    (ironclad:byte-array-to-hex-string
     (ironclad:derive-key
      kdf
      (flexi-streams:string-to-octets password :external-format :utf-8)
      (flexi-streams:string-to-octets salt     :external-format :utf-8)
      1000 128)
     :element-type 'character)))

(let ((AN "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
  (defun make-random-string (length)
    (map-into (make-string length) (lambda () (aref AN (random 62))))))

;;; registration

(defun maybe-show-form-error (error expected-error message)
  (when (equal error expected-error)
    #H[<div class="error-info">${ message }</div>]))

(defpage /site/register "Регистрация" (name email error)
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
                         "Такой аккаунт уже есть в системе")
    #H[<dt><label for="name">Name:</label></dt>
    <dd><input class="regin" name="name" size="30" value="${(if name name "")}" /></dd>]
    (maybe-show-form-error error "email" "Неправильный адрес почты")
    #H[<dt><label for="email">Email:</label></dt>
    <dd><input class="regin" name="email" size="30" value="${(if email email "")}" /></dd>]
    (maybe-show-form-error error "password" "Слишком короткий пароль")
    #H[<dt><label for="password">Password:</label></dt>
       <dd><input class="regin" name="password" type="password" size="30" /></dd>]

    (maybe-show-form-error error "captcha" "Неправильная капча")
    (let ((captcha (make-captcha)))
      #H[<dt><label for="captcha">${captcha} is:</label></dt><dd>]
      (emit-captcha-inputs captcha "regin" 30)
      #H[</dd>])

    #H[<dt /><dd><input type="submit" value="Создать аккаунт" /></dd>
  </dl>
  </form>
</div>])))

(defun email-address? (str)
  (and str (not (string= str ""))))

(defun new-account (name email password)
  (let* ((salt     (make-random-string 50))
         (account  (make-account
                    :name            name
                    :email           email
                    :password-salt   salt
                    :password-digest (password-digest password salt))))
    (wiki-new 'account account)))

(defhandler /site/do-register (name email password)
  (let ((name (if name (escape-for-html name) "")))
    (acond
      ((youre-banned?) #/)
      ((cond ((or (not name) (string= name "")) "name")
             ((find-account name)               "nametaken")
             ((not (email-address? email))      "email")
             ((< (length password) 8)           "password")
             ((not (check-captcha))             "captcha"))
       #/site/register?name={name}&email={email}&error={it})
      (t (login (new-account (cut-whitespace name) email password))
         #/))))

;;; password recovery

(defun reset-password (account)
  (let ((salt     (make-random-string 50))
        (password (make-random-string 14)))
    (update-account account
                    account-password-digest (password-digest password salt)
                    account-password-salt   salt)
    (cl-smtp:send-email "localhost"
                        (password-reminder-email-address *wiki*)
                        (account-email account)
      #?"Your new ${ (wiki-name *wiki*) } wiki password"
#?"Someone (hopefully you) requested a password reset for a lost password on the ${ (wiki-name *wiki*) } wiki.
Your new password is: ${ password }")))

(defpage /site/reset-ok "Password reset successfully" ()
  #H[Пароль успешно сброшен. Проверьте почту.])

;;; login

(defun check-password (password account)
  (and password (not (equal "" password))
       (equal (account-password-digest  account)
              (password-digest password (account-password-salt account)))))

(defhandler /site/login (name password reset-pw)
  (let ((account (find-account name)))
    (cond (*account*                          (referer))
          ((not account)                      #/site/cantfind?name={name})
          (reset-pw (reset-password account)  #/site/reset-ok)
          ((check-password password account)  (login account) (referer))
          (t                                  #/site/invalid-login))))

(defpage /site/invalid-login "Неверный логин" ()
  #H[Неправильный пароль или имя аккаунта])

(defpage /site/cantfind "Аккаунта нет" (name)
  #H[Аккаунт '${name}' не существует])

(defpage /site/logout () ()
  (logout)
  (redirect #/))

;;; user page

(defun youre-banned? ()
  (or (and *account* (banned? (account-name *account*)))
      (banned? (real-remote-addr))))

(defpage /site/account #?"Аккаунт: ${name}" (name)
  (let ((account (find-account name))
        (edits   (edits-by-author name)))
    (if (or account edits)
        (progn
         #H[<h1>Страница аккаунта ${name}</h1>]
         (when *account*
           (flet ((ban (&key (un ""))
                    (when (and (not (youre-banned?)) (account-admin *account*))
                      #H[<form method="post" action="$(#/site/{un}ban?name={name})">
                           <input type="submit" value="${un}ban" />
                         </form>])))
             (cond
               ((equal name (account-name *account*))
                #H[<a href="$(#/site/preferences)">Изменить настройки</a>])
               ((banned? name)
                #H[<em>забаненный</em>] (ban :un "un"))
               (account
                (case (account-admin account)
                  (:administrator #H[<em>Administrator</em>])
                  (:moderator     #H[<em>Moderator</em>]
                                  (ban))
                  (t
                   (ban)
                   (when (account-admin *account*)
                     #H[<br />
                        <form method="post" action="$(#/site/make-moderator?name={name})">
                          <input type="submit" value="Make moderator" />
                        </form>]))))
               (t (ban)))))
         #H[<br />Страница пользователя: ] (pprint-article-link name)
         #H[<br />Автор ${name}: <ul>]
         (map nil #'render-revision-summary (edits-by-author name))
         #H[</ul>])
        (redirect #/site/cantfind?name={name}))))

;;; user preferences

(defpage /site/preferences-ok "Настройки обновлены" ()
  #H[Email успешно обновлен])

(defhandler /site/change-email (email password)
  (flet ((err (e) #/site/preferences?email={email}&error={e}))
    (cond ((not *account*)                          #/)
          ((not (email-address? email))             (err "email"))
          ((check-password password *account*)
           (update-account *account*
                           account-email email)     #/site/preferences-ok)
          (t                                        (err "pw")))))

(defpage /site/preferences "Настройки аккаунта" (email error)
  (if *account*
      (progn
        #H[<h3>Изменить настройки аккаунта</h3>
        <form id="changemail" class="prefs" method="post"
              action="$(#/site/change-email)">
        <dl>]
          (maybe-show-form-error error "email" "Неверный адрес почты")
          #H[<dt><label for="email">Новая почта:</label></dt>
          <dd><input class="regin" type="text" name="email" title="new email"
                     value="${(if email email "")}" /></dd>]
          (maybe-show-form-error error "pw" "Неправильный пароль")
          #H[<dt><label for="password">Подтвердить пароль:</label></dt>
          <dd><input class="regin" type="password" name="password" /></dd>
          <dt /><dd><input type="submit" value="change email" /></dd>
        </dl>
      </form>])
      (redirect #/)))

;;; moderation

(defmacro moderator-handler (uri &body actions)
  `(defhandler ,uri ()
     (let ((name (cut-whitespace (get-parameter "name"))))
       (cond ((youre-banned?)                 #/)
             ((not (account-admin *account*)) (referer))
             (t ,@actions)))))

(moderator-handler /site/make-moderator
  (aif (find-account name)
       (progn (update-account it account-admin :moderator)
              (referer))
       #/site/cantfind?name={name}))

(moderator-handler /site/ban
  (unless (aand (find-account name) (eq (account-admin it) :administrator))
    (update-blacklist name t))
  (referer))

(moderator-handler /site/unban
  (update-blacklist name nil)
  (referer))
