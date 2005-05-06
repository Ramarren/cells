;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;(declaim (optimize (debug 2) (speed 1) (safety 1) (compilation-speed 1)))
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

;;;(operate 'load-op :asdf-aclproj)
;;;(use-package :asdf-aclproj)

#+(or allegro lispworks cmu mcl clisp cormanlisp sbcl scl)

(asdf:defsystem :utils-kt
  :name "utils-kt"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :version "18-Oct-2004"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT Style"
  :description "Kenny's Utilities"
  :long-description "Low-level utilities used by all of Kenny's projects"
  :components ((:file "defpackage")
               (:file "debug")
               (:file "detritus")
               (:file "flow-control")
               (:file "strings")))

(defmethod perform ((o load-op) (c (eql (find-system :utils-kt))))
  ; (pushnew "CELLS" *modules* :test #'string=)
  (pushnew :utils-kt *features*))
