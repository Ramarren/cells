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
  (:use #:common-lisp #:excl #:utils-kt #:cells)
  (:export #:geometer #:geo-zero-tl #:geo-inline #:a-stack #:a-row
    #:px #:py #:ll #:lt #:lr #:lb #:pl #:pt #:pr #:pb
    #:^px #:^py #:^ll #:^lt #:^lr #:^lb #:^lb-height
    #:^fill-parent-down
    #:u96ths #:udots #:uinches #:uin #:upoints #:upts #:u8ths #:u16ths #:u32nds
    #:mkr #:v2-nmove #:l-height #:mkv2 #:^offset-within #:inset-lr #:v2-v #:v2-h
    #:r-bounds #:l-box
    #:lb
    #:cs-target-res 
    #:nr-make 
    #:r-contains 
    #:collapsed 
    #:g-box 
    #:v2-in-rect-ratio 
    #:v2-xlate #:v2-in-rect #:v2-add #:v2-subtract 
    #:log2scr 
    #:^lr-width 
    #:px-maintain-pr 
    #:outset 
    #:py-maintain-pb 
    #:cs-logical-dpi 
    #:px-maintain-pl #:py-maintain-pt 
    #:scr2log 
    #:inset-width #:inset-height 
    #:res-to-res 
    #:logical-to-screen-point 
    #:nres-to-res 
    #:cs-logical-screen-resolution 
    #:outl 
    #:with-r-bounds #:r-inset 
    #:ncopy-rect 
    #:l 
    #:r-height #:r-width #:r-top #:r-right #:r-bottom #:r-left 
    #:l-width ))
