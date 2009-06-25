;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
;;; Copyright (c) 2008 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :common-lisp-user)

(defpackage :cells
  (:use #:common-lisp #:utils-kt)
  (:import-from
   ;; MOP
   #+allegro #:excl
   #+clisp #:clos
   #+cmu #:mop
   #+cormanlisp #:common-lisp
   #+lispworks #:clos
   #+sbcl #:sb-mop
   #+openmcl-partial-mop #:openmcl-mop
   #+(and mcl (not openmcl-partial-mop)) #:ccl
   
   #-(or allegro clisp cmu cormanlisp lispworks mcl sbcl)
   #.(cerror "Provide a package name."
       "Don't know how to find the MOP package for this Lisp.")
   
   #:class-precedence-list
   #-(and mcl (not openmcl-partial-mop)) #:class-slots
   #:slot-definition-name
   #:class-direct-subclasses
   )
  (:export #:cell #:.md-name 
           #:c-input #:c-in #:c-in8
           #:c-formula #:c? #:c_? #:c?8 #:c?_ #:c??
           #:with-integrity #:without-c-dependency #:self #:*parent*
           #:.cache #:.with-c-cache #:c-lambda
           #:defmodel #:defmd #:defobserver #:slot-value-observe #:def-c-unchanged-test
           #:new-value #:old-value #:old-value-boundp #:c...
           #:md-awaken
           #:mkpart #:make-kid #:the-kids #:nsib #:value #:^value #:.value #:kids #:^kids #:.kids
           #:cells-reset #:upper #:fm-max #:nearest #:fm-min-kid #:fm-max-kid #:mk-kid-slot 
           #:def-kid-slots #:find-prior #:fm-pos #:kid-no #:fm-includes #:fm-ascendant-common 
           #:fm-kid-containing #:fm-find-if #:fm-ascendant-if #:c-abs #:fm-collect-if #:psib
           #:not-to-be #:ssibno
           #:c-debug #:c-break #:c-assert #:c-stop #:c-stopped #:c-assert #:.stop    #:delta-diff
           #:wtrc #:wnotrc #:eko-if #:trc #:wtrc #:eko #:ekx #:trcp #:trcx
           ;; also exports
           #:fm-collect #:do-like-fm-parts #:fm-find-last-if
           #:c?n-dbg #:c?dbg #:fm-otherx
           #:fm-ascendant-p #:fm-kid-insert-last
           #:md-census-report #:fm-next-sib
           #:fm-kid-replace #:.fbid
           #:fm-ascendant-some #:c?once #:mdead
           #:^k-last #:c_?dbg #:cll-inner
           #:fm-mapc #:fm-ps-parent #:fm-ordered-p
           #:family-values #:.bgo #:fm-find-prior
           #:fm-descendant-named #:.cache-bound-p
           #:fm^ #:c?1 #:fm!v #:fm-collect-some
           #:md-awake-ct #:|^K1| #:c_1
           #:fm-traverse-bf #:u^
           #:fm-descendant-common #:f-sensitivity
           #:kid1 #:c?-with-stored
           #:with-like-fm-parts #:|^K2| #:rg?
           #:fm-find-next #:fm-ancestor-p
           #:^sort-index #:sub-nodes
           #:fm-kid-insert-first #:fm-do-up #:rg!
           #:.cdbg #:name-subscript #:c?n
           #:fm-find-next-within #:fm-ascendant-named
           #:^fm-next-sib #:^sort-predicate
           #:fm-quiesce-all #:container #:cll-outer
           #:with-cc #:store-remove #:f-zerop
           #:with-store-item #:fm-descendant-if
           #:c-value-incf #:md-census-count
           #:c?n-until #:md-name #:cells-store
           #:fm-nearest-if #:fm-other-v #:fm-other?!
           #:fm-find-prior-within #:fm-map
           #:make-part #:fm-prior-sib #:.cause
           #:sort-predicate #:name-root #:c-envalue
           #:fm-kid-named #:c-input-dbg #:fm-kid-add
           #:fm-parent #:fm-traverse #:fm-other!
           #:fm-top #:f-delta #:store-lookup
           #:store-add #:first-born-p
           #:test-cells-store #:make-name
           #:fm-kid-insert #:fm! #:container-typed
           #:mk-part-spec #:fdifferent #:with-synapse
           #:fm-find-one #:*fmdbg* #:last-kid
           #:mk-part #:fm-value-dictionary
           #:fm-kid-remove #:.parent #:sort-direction
           #:fm-find-all #:fm-other #:fm-count-named
           #:bwhen-c-stored #:^sort-direction #:brkx
           #:f-find #:.pa #:fm-first-above
           #:perishable #:fm? #:mk-synapse
           #:md-census-start #:family #:fm-kid-typed
           #:fm-descendant-typed #:fm^v #:c_in
           #:fm-find-kid utils-kt:brk #:bgo
           #:sort-index #:sort-key #:fm-otherv?
           #:.dpid #:family-values-sorted #:fmv
           #:dbg #:fm-other? #:fm-gather
           #:store-items #:print-cell-model
           #:^sort-key #:md-awake-ct-ct #:true-that
           #:c-in-lazy #:fm-ascendant-typed #:model
           #:kid2 #:.stopped #:c?+n
           #:fm-next-sib-if #:f-plusp
           )
  #+allegro (:shadowing-import-from #:excl #:fasl-write #:fasl-read #:gc)
  )

