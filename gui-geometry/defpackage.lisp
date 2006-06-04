;; -*- mode: Lisp; Syntax: Common-Lisp; Package: gui-geometry; -*-
#|

Copyright (C) 2004 by Kenneth William Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(defpackage #:gui-geometry
  (:nicknames #:geo)
  (:use #:common-lisp #:utils-kt #:cells)
  (:export #:geometer #:px #:py #:ll #:lt #:lr #:lb))