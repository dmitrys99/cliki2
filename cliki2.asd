(asdf:defsystem :cliki2
  :description "The Common Lisp wiki."
  :depends-on (#:alexandria
               #:iterate
               #:hunchentoot
               #:bordeaux-threads
               #:bknr.datastore
               #:ironclad
               #:colorize
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
