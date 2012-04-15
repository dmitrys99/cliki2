(in-package #:cl)

(defpackage #:cliki2
  (:use #:cl #:named-readtables #:anaphora #:uri-template #:hunchentoot
        #:bordeaux-threads)
  (:export #:start-cliki-server))
