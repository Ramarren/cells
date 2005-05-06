;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#+(or allegro lispworks cmu mcl clisp cormanlisp sbcl scl)
(progn
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(asdf:defsystem :cells
  :name "cells"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :version "2.0"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT Style"
  :description "Cells"
  :long-description "The Cells dataflow extension to CLOS."
  :components ((:module "utils-kt"
			:serial t
			:components ((:file "defpackage")
				     (:file "debug")
				     (:file "detritus")
				     (:file "flow-control")
				     (:file "strings")))
               (:file "defpackage" :depends-on ("utils-kt"))
               (:file "cells" :depends-on ("defpackage"))
               (:file "cell-types" :depends-on ("defpackage"))
               (:file "integrity" :depends-on ("cell-types" "cells"))
               (:file "constructors" :depends-on ("integrity" "cells"))
               (:file "initialize" :depends-on ("cells" "cell-types"))
               (:file "md-slot-value" :depends-on ("integrity" "cell-types"))
               (:file "slot-utilities" :depends-on ("cells"))
               (:file "optimization" :depends-on ("cells"))
               (:file "link" :depends-on ("cells"))
               (:file "propagate" :depends-on ("cells" "integrity"))
               (:file "synapse" :depends-on ("cells"))
               (:file "synapse-types" :depends-on ("cells"))
               (:file "model-object" :depends-on ("defpackage"))
               (:file "defmodel" :depends-on ("model-object" "propagate" "constructors"))
               (:file "md-utilities" :depends-on ("cells"))
               (:file "family" :depends-on ("defmodel"))
               (:file "fm-utilities" :depends-on ("cells" "family"))
               (:file "family-values" :depends-on ("family" "propagate" "defmodel" ))
               (:file "test" :depends-on ("family"))
               ))

(defmethod perform ((o load-op) (c (eql (find-system :cells))))
  (pushnew :cells *features*))

(defmethod perform ((o test-op) (c (eql (find-system :cells))))
  (oos 'load-op :cells-test))

(defmethod perform ((o test-op) (c (eql :cells)))
  (oos 'load-op :cells-test))

)