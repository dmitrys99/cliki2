(asdf:defsystem :cliki-convert
  :components
  ((:module "cliki-convert"
            :components ((:file "package")
                         (:file "cliki-convert" :depends-on ("package")))))
  :depends-on (#:cliki2 #:iterate #:cl-fad #:external-program #:local-time))
