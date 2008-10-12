#|

    Utils-kt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#


(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *features* (remove :its-alive! *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *features* (pushnew :gimme-a-break *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *features* (remove :debugging-alive! *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;;  #+(and its-alive! (not debugging-alive!))
  ;;;  (proclaim '(optimize (speed 3) (safety 1) (space 1) (debug 0)))
  ;;;  #-(and its-alive! (not debugging-alive!))
  (proclaim '(optimize (speed 2) (safety 1) (space 1) (debug 3))))

(defpackage :utils-kt
  (:nicknames #:ukt)
  (:use #:common-lisp #:excl
    #+(or allegro lispworks clisp) #:clos
    #+cmu  #:mop
    #+sbcl #:sb-mop
    #+openmcl-partial-mop #:openmcl-mop
    #+(and mcl (not openmcl-partial-mop))  #:ccl)
  (:export 
    #:export!
    #:utils-kt-reset
    #:count-it #:count-of #:with-counts
    #:wdbg #:maptimes #:bwhen #:bif #:xor
    #:with-dynamic-fn #:last1 #:packed-flat! #:with-metrics 
    #:shortc
    #:intern$
    #:define-constant #:*count* #:*stop*
    #:*dbg*
   #:with-gensyms
    #:make-fifo-queue #:fifo-queue #:fifo-add #:fifo-delete
    #:fifo-empty #:fifo-pop #:fifo-clear
    #:fifo-map #:fifo-peek #:fifo-data #:with-fifo-map #:fifo-length

    #-(or lispworks mcl) #:true
    #+(and mcl (not openmcl-partial-mop)) #:class-slots
    ))
