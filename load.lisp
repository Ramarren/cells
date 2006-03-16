(load (make-pathname :device "c"
        :directory '(:absolute "0dev" "cells")
        :name "asdf"
        :type "lisp"))

(progn
  (push (make-pathname :device "c" :directory '(:absolute "0dev" "cells"))
    asdf:*central-registry*)

  (ASDF:OOS 'ASDF:LOAD-OP :CELLS :force t))

;;;(push (make-pathname :device "c"
;;;        :directory '(:absolute "0dev" "cells" "cells-test"))
;;;  asdf:*central-registry*)

(ASDF:OOS 'ASDF:LOAD-OP :CELLS-TEST :force t)