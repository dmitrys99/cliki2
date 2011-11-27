(in-package #:cl)

(defpackage #:cliki2
  (:use #:cl #:named-readtables #:iterate #:anaphora #:uri-template
        #:hunchentoot #:bknr.datastore #:bknr.indices))

(defpackage #:cliki2.categories)
