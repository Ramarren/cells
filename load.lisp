(PUSH (MAKE-PATHNAME :DEVICE "C" :DIRECTORY '(:ABSOLUTE "0dev" "cells_2.0"))
                                               ASDF:*CENTRAL-REGISTRY*)
(ASDF:OOS 'ASDF:LOAD-OP :CELLS :force t)