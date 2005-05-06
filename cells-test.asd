;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#+(or allegro lispworks cmu mcl clisp cormanlispx sbcl scl)
(progn

(asdf:defsystem :cells-test
  :name "cells-test"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT Style"
  :description "Cells Regression Test/Documentation"
  :long-description "Informatively-commented regression tests for Cells"
  :serial t
  :depends-on (:cells)
  :components ((:module "cells-test"
			:components ((:file "test")
				     (:file "hello-world")
				     (:file "internal-combustion")
				     (:file "boiler-examples")
				     (:file "person")
				     (:file "df-interference")
				     (:file "test-family")
				     (:file "test-kid-slotting")
				     (:file "lazy-propagation")
				     (:file "output-setf")
				     (:file "test-lazy")
				     (:file "synapse-testing")))))

(defmethod perform :after ((op load-op) (system (eql (find-system :cells-test))))
  (funcall (find-symbol "CV-TEST" "CELLS")))
)
