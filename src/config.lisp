(in-package #:cliki2)

(defparameter *datadir* #P"/home/cliki2/")

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