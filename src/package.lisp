(in-package #:cl)

(defpackage #:cliki2
  (:use #:cl #:named-readtables #:iterate #:anaphora #:uri-template
        #:hunchentoot #:bknr.datastore #:bknr.indices
        #:3bmd #:3bmd-ext #:esrap))

(defpackage #:cliki2.categories)
