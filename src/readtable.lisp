(in-package #:cliki2)

(defparameter %defined-uris ())
(defparameter %referenced-uris ())

(defvar *html-stream* *standard-output*)

(defreadtable cliki2
  (:merge :standard)
  (:case :invert)
  (:dispatch-macro-char
   #\# #\?
   #'cl-interpol::interpol-reader)
  (:dispatch-macro-char
   #\# #\H
   (lambda (&rest args)
     `(princ ,(apply #'cl-interpol::interpol-reader args) *html-stream*)))
  (:dispatch-macro-char
   #\# #\/
   (lambda (stream subchar arg)
     (declare (ignore subchar arg))
     (let ((uri-path
            (with-output-to-string (x)
              (princ #\/ x)
              (loop until (member (peek-char nil stream nil #\Space t)
                                  '(#\Space #\Newline #\Tab #\? #\) #\{))
                    do (princ (read-char stream) x)))))
       (pushnew (cons uri-path (or *compile-file-pathname* *load-pathname*))
                %referenced-uris :key #'car :test #'equal)
       `(concatenate 'string ,uri-path
                     ,@(uri-template:read-uri-template stream))))))
