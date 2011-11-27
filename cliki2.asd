;;;; cliki2.asd

(asdf:defsystem :cliki2
    :depends-on (#:alexandria
                 #:iterate
                 #:hunchentoot
                 #:bknr.datastore
                 #:ironclad
                 #:colorize
                 #:cl-recaptcha
                 #:sanitize
                 #:diff
                 #:cl-interpol
                 #:uri-template
                 #:babel
                 #:cl-ppcre
                 #:cl-smtp
                 #:anaphora
                 #:stem)
    :components
    ((:module "src"
              :serial t
              :components
              ((:file "package")
               (:file "readtable")
               (:file "authentication")
               (:file "html-rendering")
               (:file "http-resource")
               (:file "accounts")
               (:file "article")
               (:file "markup")
               (:file "diff")
               (:file "search")
               (:file "recent-changes")
               (:file "deleted-articles")
               (:file "history")
               (:file "tools")
               (:file "dispatcher")
               (:file "start")))))
