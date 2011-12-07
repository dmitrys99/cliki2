(asdf:defsystem :cliki-daemon
  :components ((:file "start-daemon"))
  :depends-on (#:cliki2 #:swank #:cl-daemonize))
