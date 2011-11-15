(in-package #:cliki2)

(defvar *account* nil)

(defmacro %defpage (page method parameters &body body)
  (let ((page-uri (symbol-name (if (listp page) (car page) page))))
    `(progn
       (pushnew (list ,page-uri) %defined-uris :test #'equal)
       (define-easy-handler (,(if (listp page) (cadr page) page) :uri ,(string-downcase page-uri) :default-request-type ,method) ,parameters
         (let ((*account* (session-value 'account)))
           ,@body)))))

(defmacro defpage (page title parameters &body body)
  `(%defpage ,page :both ,parameters
     (render-page ,title ,@body)))

(defmacro defhandler (page parameters &body body)
  `(%defpage ,page :post ,parameters (redirect (progn ,@body) :code 303)))
