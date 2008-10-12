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



(defmacro with-gensyms ((&rest symbols) &body body)
  `(let ,(loop for sym in symbols
             collecting `(,sym (gensym ,(string sym))))
     ,@body))

(defmacro eval-now! (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro export! (&rest symbols)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
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

(defun test-setup (&optional drib)
  #+(and allegro ide (or (not its-alive!) debugging-alive!))
  (ide.base::find-new-prompt-command
   (cg.base::find-window :listener-frame))
  (when drib
    (dribble (merge-pathnames 
              (make-pathname :name drib :type "TXT")
              (project-path)))))

(export! test-setup test-prep test-init)
(export! project-path)
(defun project-path ()
  #+(and allegro ide (not its-alive!))
  (excl:path-pathname (ide.base::project-file ide.base:*current-project*))
  )

#+test
(test-setup)

(defun test-prep (&optional drib)
  (test-setup drib))

(defun test-init (&optional drib)
  (test-setup drib))