#|

    Utils-kt core

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :utils-kt)

(defmacro eval-now! (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro export! (&rest symbols)
    `(eval-when ( :compile-toplevel :load-toplevel :execute)
       (export ',symbols))))

(eval-now!
  (defmacro define-constant (name value &optional docstring)
   "Define a constant properly.  If NAME is unbound, DEFCONSTANT
it to VALUE.  If it is already bound, and it is EQUAL to VALUE,
reuse the SYMBOL-VALUE of NAME.  Otherwise, DEFCONSTANT it again,
resulting in implementation-specific behavior."
   `(defconstant ,name
      (if (not (boundp ',name))
	  ,value
	  (let ((value ,value))
	    (if (equal value (symbol-value ',name))
		(symbol-value ',name)
		value)))
      ,@(when docstring (list docstring)))))


(export! exe-path exe-dll font-path)

#-iamnotkenny
(defun exe-path ()
  #+its-alive!
  (excl:current-directory)
  #-its-alive!
  (excl:path-pathname (ide.base::project-file ide.base:*current-project*)))

#-iamnotkenny 
(defun font-path ()
  (merge-pathnames
   (make-pathname
    :directory #+its-alive! (list :relative "font")
    #-its-alive! (append (butlast (pathname-directory 
       (exe-path)
  ))
   (list "TY Extender" "font")))
   (exe-path)))

#+test
(list (exe-path)(font-path))

(defmacro exe-dll (&optional filename)
  (assert filename)
  (concatenate 'string filename ".dll"))

#+chya
(defun exe-dll (&optional filename)
  (merge-pathnames
   (make-pathname :name filename :type "DLL"
     :directory (append (butlast (pathname-directory (exe-path)))
                  (list "dll")))
   (exe-path)))

#+test
(probe-file (exe-dll "openal32"))
