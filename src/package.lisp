(in-package #:cl)

(defpackage #:cliki2
  (:use #:cl #:named-readtables #:anaphora #:uri-template #:hunchentoot
        #:bordeaux-threads)
  (:shadow #:session)
  (:export #:start-cliki-server))
