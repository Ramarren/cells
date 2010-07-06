;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#+(or allegro lispworks cmu mcl ccl clisp cormanlisp sbcl scl ecl abcl)
(progn
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(asdf:defsystem :cells
  :name "cells"
  :author "Kenny Tilton <kentilton@gmail.com>"
  :maintainer "Kenny Tilton <kentilton@gmail.com>"
  :licence "Lisp LGPL"
  :description "Cells"
  :long-description "Cells: a dataflow extension to CLOS."
  :version "3.0"
  :serial t
  :depends-on (:utils-kt)
  :components ((:file "defpackage")
               (:file "trc-eko")
               (:file "cells")
               (:file "integrity")
               (:file "cell-types")
               (:file "constructors")
               (:file "initialize")
               (:file "md-slot-value")
               (:file "slot-utilities")
               (:file "link")
               (:file "propagate")
               (:file "synapse")
               (:file "synapse-types")
               (:file "model-object")
               (:file "defmodel")
               (:file "family")
               (:file "md-utilities")
               (:file "fm-utilities")
               (:file "family-values")
               (:file "test-propagation")
               (:file "cells-store")))

(defmethod asdf:perform ((o asdf:load-op) (c (eql (asdf:find-system :cells))))
  (pushnew :cells *features*))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cells))))
  (asdf:oos 'asdf:load-op :cells-test))

(defmethod asdf:perform ((o asdf:test-op) (c (eql :cells)))
  (asdf:oos 'asdf:load-op :cells-test)))
