(in-package #:cliki2)

(defmacro %defpage (page method parameters &body body)
  (let ((page-uri (symbol-name (if (listp page) (car page) page))))
    `(progn
       (pushnew (list ,page-uri) %defined-uris :test #'equal)
       (define-easy-handler (,(if (listp page) (cadr page) page) :uri ,(string-downcase page-uri) :default-request-type ,method) ,parameters
         ,@body))))

(defmacro defpage (page title parameters &body body)
  `(%defpage ,page :get ,parameters
     (render-page ,title ,@body)))

(defmacro defhandler (page parameters &body body)
  `(%defpage ,page :post ,parameters (redirect (progn ,@body) :code 303)))
