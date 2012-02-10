(in-package #:cliki2)

(defparameter *datadir*
  (or (cl-fad:directory-exists-p
       (or (osicat:environment-variable "CLIKI_HOME")
           (error "Environment variable CLIKI_HOME not set")))
      (error "CLIKI_HOME does not point to a valid directory (~A)"
             (osicat:environment-variable "CLIKI_HOME"))))

(defparameter *blank-file*
  (let ((pathname (merge-pathnames "cliki2blankfile" *datadir*)))
    (open pathname :direction :probe :if-does-not-exist :create)
    pathname))

(defparameter *error-log*
  (merge-pathnames "error-log" *datadir*))

(setf clhs-lookup::*hyperspec-pathname*
      (merge-pathnames "HyperSpec/" *datadir*)
      clhs-lookup::*hyperspec-map-file*
      (merge-pathnames "HyperSpec/Data/Symbol-Table.text" *datadir*)
      clhs-lookup::*hyperspec-root* "/site/HyperSpec/")

(defun read-config-file (file)
  (with-open-file (s (merge-pathnames (merge-pathnames "config/" file)
                                      *datadir*))
    (read s)))

(defparameter *wiki-name* (read-config-file "name"))
(defparameter *wiki-description* (read-config-file "description"))
(defparameter *reminder-email* (read-config-file "reminder-email"))
