;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :gui-geometry
  :author "Kenny Tilton <kentilton@gmail.com>"
  :maintainer "Kenny Tilton <kentilton@gmail.com>"
  :licence "Lisp LGPL"
  :depends-on (:cells)
  :serial t
  :components
  ((:file "defpackage")
   (:file "geo-macros")
   (:file "geo-data-structures")
   (:file "coordinate-xform")
   (:file "geometer")
   (:file "geo-family")))
