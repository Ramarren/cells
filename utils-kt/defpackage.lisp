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
  (:use #:common-lisp
    #+(or allegro lispworks clisp ecl) #:clos
    #+cmu  #:mop
    #+sbcl #:sb-mop
    #+openmcl-partial-mop #:openmcl-mop
    #+abcl #:mop
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
   ;; also exports
   #:brk #:right$ #:conc-list$ #:empty$
   #:subseq-contiguous-p #:test-prep
   #:dd-mmm-yy #:mm/dd #:find-after
   #:list-flatten! #:conc$
   #:tree-includes #:-1?1 #:cc-defstruct
   #:week-time #:test-setup #:b1
   #:b-if #:maphash* #:min$
   #:ymdhmsh #:cc$ #:+lf$+
   #:class-proto #:trim$ #:remove$
   #:hashtable-assoc #:month-abbreviation
   #:timex #:space$ #:up$
   #:pair-off #:down$ #:mdyy-yymd
   #:upper$ #:clock-0 #:assoc$
   #:eval-now! #:abbrev$
   #:tree-intersect #:hour-min-of-day
   #:collect #:assocd
   #:weekday-abbreviation #:numeric$
   #:mid$ #:find$ #:yyyy-mm-dd
   #:trunc$ #:m/d/y #:char$
   #:clock-off #:list-insertf #:prime?
   #:b-when #:member$ #:clock
   #:instance-slots #:num$
   #:tree-traverse #:tree-flatten
   #:rassoca #:project-path
   #:find-before #:count-it! #:starts$
   #:u-date #:u-time #:alpha$
   #:push-end #:normalize$ #:now
   #:list-insert-after #:without-repeating
   #:mmm-dd-yyyy #:time-in-zone
   #:split-sequence #:case-string-equal
   #:seg$ #:lower$ #:shuffle
   #:insert$ #:left$ multiple-value-bind
   #:equal$ #:case$ #:make$
   #:collect-if #:+return$+ #:u-day
   #:test-init #:time-of-day
   #:os-tickcount #:strloc$
   ))
