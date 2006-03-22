#+eval-this-if-you-do-not-autoload-asdf
(load (make-pathname :device "c"
        :directory '(:absolute "0dev" "cells")
        :name "asdf"
        :type "lisp"))

(push (make-pathname :device "c" :directory '(:absolute "0dev" "cells"))
    asdf:*central-registry*)

(push (make-pathname :device "c" :directory '(:absolute "0dev" "Celtk"))
    asdf:*central-registry*)

#-runtestsuite
(ASDF:OOS 'ASDF:LOAD-OP :CELLS)

#+runtestsuite
(ASDF:OOS 'ASDF:LOAD-OP :CELLS-TEST)

#+checkoutceltk
(ASDF:OOS 'ASDF:LOAD-OP :CELTK)

#+testceltk
(ctk::tk-test)

