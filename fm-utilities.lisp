;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
;;; Copyright (c) 1995,2003 by Kenneth William Tilton.
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

(in-package :cells)

(defparameter *fmdbg* nil)

(eval-when (compile eval load)
  (export '(make-part mk-part fm-other fm-other? fm-traverse fm-descendant-typed do-like-fm-parts
             container-typed *fmdbg* fm-other-v fm! fm^ fm-find-one fm-kid-named
             
             fm-value-dictionary fm-otherv?)))

(defun make-part (partname part-class &rest initargs)
  ;;(trc "make-part > name class" partname partclass)
  (when part-class ;;a little programmer friendliness
    (apply #'make-instance part-class (append initargs (list :md-name partname)))))

(defmacro mk-part (md-name (md-class) &rest initargs)
  `(make-part ',md-name ',md-class ,@initargs))

(defmethod make-part-spec ((part-class symbol))
  (make-part part-class part-class))

(defmethod make-part-spec ((part model))
  part)

(defmacro upper (self &optional (type t))
  `(container-typed ,self ',type))

(defmethod container (self) (fm-parent self))

(defmethod container-typed ((self model-object) type)
   (let ((parent (container self))) ;; fm- or ps-parent
      (cond
       ((null parent) nil)
       ((typep parent type) parent)
       (t (container-typed parent type)))))

(defun fm-descendant-typed (self type)
  (when self
    (or (find-if (lambda (k) (typep k type)) (kids self))
        (some (lambda (k)
                  (fm-descendant-typed k type)) (kids self)))))

(defun fm-kid-named (self name)
  (find name (^kids) :key 'md-name))

(defun fm-descendant-named (parent name &key (must-find t))
   (fm-find-one parent name :must-find must-find :global-search nil))

(defun fm-ascendant-named (parent name)
   (when parent
      (or (when (eql (md-name parent) name)
             parent)
          (fm-ascendant-named (fm-parent parent) name))))

(defun fm-ascendant-typed (parent name)
   (when parent
      (or (when (typep parent name)
             parent)
          (fm-ascendant-typed (fm-parent parent) name))))

(defun fm-ascendant-some (parent some-function)
   (when (and parent some-function)
     (or (funcall some-function parent)
         (fm-ascendant-some (fm-parent parent) some-function))))

(defun fm-ascendant-if (self if-function)
   (when (and self if-function)
     (or (when (funcall if-function self)
           self)
         (fm-ascendant-if .parent if-function))))

(defun fm-ascendant-common (d1 d2)
  (fm-ascendant-some d1 (lambda (node)
                            (when (fm-includes node d2)
                              node))))

(defun fm-collect-if (tree test)
  (let (collection)
    (fm-traverse tree (lambda (node)
                        (when (funcall test node)
                          (push node collection))))
    (nreverse collection)))

(defun fm-value-dictionary (tree value-fn &optional include-top)
  (let (collection)
    (fm-traverse tree
      (lambda (node)
        (when (or include-top (not (eq node tree)))
          (bwhen (v (funcall value-fn node))
            (push (cons (md-name node) v) collection)))))
    (nreverse collection)))

(defun fm-max (tree key)
  (let (max)
    (fm-traverse tree (lambda (node)
                        (if max
                            (setf max (max max (funcall key node)))
                          (setf max (funcall key node)))))
    max))


(defun fm-traverse (family applied-fn &key skip-node skip-tree global-search (opaque nil))
   ;;(when *fmdbg* (trc "fm-traverse" family skipTree skipNode global-search))
   (when family
      (labels ((tv-family (fm)
                 (when (and (typep fm 'model-object)
                            (not (eql fm skip-tree)))
                    (let ((outcome (and (not (eql skip-node fm)) ;; skipnode new 990310 kt
                                        (funcall applied-fn fm))))
                       (unless (and outcome opaque)
                          (dolist (kid (kids fm))
                             (tv-family kid))
                          ;(tv-family (mdValue fm))
                          )))))
        (tv-family family)
        (when global-search
           (fm-traverse (fm-parent family) applied-fn 
             :global-search t
             :skip-tree family
             :skip-node skip-node)
           )
        )
      nil))              

(defmethod sub-nodes (other)
  (declare (ignore other)))

(defmethod sub-nodes ((self family))
  (kids self))

(defmethod fm-ps-parent ((self model-object))
  (fm-parent self))

(defmacro with-like-fm-parts ((parts-var (self like-class)) &body body)
   `(let (,parts-var)
       (fm-traverse ,self (lambda (node)
                              ;;(trc "with like sees node" node (type-of node) ',likeclass)
                              (when (typep node ',like-class)
                                 (push node ,parts-var)))
         :skip-node ,self
         :opaque t)
       (setf ,parts-var (nreverse ,parts-var))
       (progn ,@body)))

(defmacro do-like-fm-parts ((part-var (self like-class) &optional return-var) &body body)
   `(progn
     (fm-traverse ,self (lambda (,part-var)
                            (when (typep ,part-var ',like-class)
                               ,@body))
       :skip-node ,self
       :opaque t)
       ,return-var)
   )

;;
;; family member finding
;;


#|
 (defun fm-member-named (kidname kids)
  (member kidname kids :key #'md-name))
 |#

(defun true-that (that) (declare (ignore that)) t)
;;
;; eventually fm-find-all needs a better name (as does fm-collect) and they
;; should be modified to go through 'gather', which should be the real fm-find-all
;;
(defun fm-gather (family &key (test #'true-that))
     (packed-flat!
      (cons (when (funcall test family) family)
        (mapcar (lambda (fm)
                    (fm-gather fm :test test))
          (kids family)))))

(defun fm-find-all (family md-name &key (must-find t) (global-search t))
     (let ((matches (catch 'fm-find-all
                             (with-dynamic-fn
                              (traveller (family)
                               (with-dynamic-fn
                                (filter (kid) (eql md-name (md-name kid)))
                                (let ((matches (remove-if-not filter (kids family))))
                                   (when matches
                                        (throw 'fm-find-all matches)))))
                              (fm-traverse family traveller :global-search global-search)))))
        (when (and must-find (null matches))
           (setf *stop* t)
          (break "fm-find-all > *stop*ping...did not find ~a ~a ~a" family md-name global-search)
          ;; (error 'fm-not-found (list md-name family global-search))
          )
        matches))

(defun fm-find-next (fm test-fn)
  (fm-find-next-within fm test-fn))

(defun fm-find-next-within (fm test-fn &optional upperbound &aux (fm-parent (unless (eql upperbound fm)
                                                                              (fm-parent fm))))
   (let ((sibs (and fm-parent (rest (member fm (kids fm-parent))))))
      (or (dolist (s sibs)
             (let ((winner (fm-find-if s test-fn)))
                (when winner (return winner))))
          (if fm-parent
             (fm-find-next-within fm-parent test-fn upperbound)
             (fm-find-if fm test-fn)))))

(defun fm-find-prior (fm test-fn)
  (fm-find-prior-within fm test-fn))

(defun fm-find-prior-within (fm test-fn &optional upperbound &aux (fm-parent (unless (eql upperbound fm)
                                                                              (fm-parent fm))))
  (let ((sibs (and fm-parent (kids fm-parent))))
    (or (loop with next-ok
            for s on sibs
            for last-ok = nil then (or next-ok last-ok)
            when (eql fm (first s)) do (loop-finish)
              finally (return last-ok)
            do (setf next-ok (fm-find-last-if (car s) test-fn)))
      (if fm-parent
          (fm-find-prior-within fm-parent test-fn upperbound)
        (fm-find-last-if fm test-fn)))))
  
  (defun fm-find-last-if (family test-fn)
    (let ((last))
      (or (and (kids family)
            (dolist (k (kids family) last)
              (setf last (or (fm-find-last-if k test-fn) last))))
        (when (funcall test-fn family)
          family))))

(defun fm-prior-sib (self &optional (test-fn #'true-that)
                      &aux (kids (kids (fm-parent self))))
  "Find nearest preceding sibling passing TEST-FN"
  (find-if test-fn kids :end (position self kids) :from-end t))

(defun fm-next-sib-if (self test-fn)
     (some test-fn (cdr (member self (kids (fm-parent self))))))

(defun fm-next-sib (self)
     (car (cdr (member self (kids (fm-parent self))))))

(defmacro ^fm-next-sib (&optional (self 'self))
     (let ((s (gensym)))
        `(let ((,s ,self))
             (car (cdr (member ,s (kids (fm-parent ,s))))))))

(defun find-prior (self sibs &key (test #'true-that))
  (c-assert (member self sibs)) ;; got this by accidentally having toolbar kids dependent..on second calc,
  ;;                             all newkids got over, and when old kids tried to recalculate...not in sibs!!
  (unless (eql self (car sibs))
    (labels
        ((fpsib (rsibs &aux (psib (car rsibs)))
                (c-assert rsibs () "~&find-prior > fpsib > self ~s not found to prior off" self)
                (if (eql self (cadr rsibs))
                   (when (funcall test psib) psib)
                   (or (fpsib (cdr rsibs))
                       (when (funcall test psib) psib)))))
      (fpsib sibs))))

(defun fm-find-if (family test-fn &key skip-top-p) ;; 99-03 kt why is thsi depth-first?
  (c-assert test-fn)
  (when family
    (or (dolist (b (sub-nodes family))
          (let ((match (fm-find-if b test-fn)))
             (when match (return match))))
        (when (and (not skip-top-p)
                   (funcall test-fn family))
          family))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  family ordering
;;;;
(defun fm-kid-add (fm-parent kid &optional before)
     (c-assert (or (null (fm-parent kid)) (eql fm-parent (fm-parent kid))))
   (c-assert (typep fm-parent 'family))
     (setf (fm-parent kid) fm-parent)
     (fm-kid-insert kid before))

(defun fm-kid-insert-last (goal &aux (fm-parent (fm-parent goal)))
     (setf (kids fm-parent) (nconc (kids fm-parent) (list goal))))

(defun fm-kid-insert-first (goal &aux (fm-parent (fm-parent goal)))
     (setf (kids fm-parent) (cons goal (kids fm-parent))))

(defun fm-kid-insert (kid &optional before &aux (da-kids (kids (fm-parent kid))))
  (c-assert (or (null before) (eql (fm-parent kid) (fm-parent before))))
  (setf (kids (fm-parent kid))
          (if before
             (if (eql before (car da-kids))
                (cons kid da-kids)
                (let ((cell (member before da-kids)))
                   (rplaca cell kid)
                   (rplacd cell (cons before (cdr cell)))
                   (cons (car da-kids) (rest da-kids))))
             (if da-kids
                (progn
                  (rplacd (last da-kids) (cons kid nil))
                  (cons (car da-kids) (rest da-kids)))
                (cons kid da-kids)))))

(defun fm-kid-remove (kid &key (quiesce t) &aux (parent (fm-parent kid)))
  (when quiesce
    (fm-quiesce-all kid))
  (when parent
    (setf (kids parent) (remove kid (kids parent)))
    ;; (setf (fm-parent kid) nil) gratuitous housekeeping caused ensuing focus output
    ;; image-invalidate to fail since no access to containing window via fm-parent chain
    ))

(defun fm-quiesce-all (md)
  (md-quiesce md)
  (dolist (kid (kids md))
    (when (and kid (not (md-untouchable kid)))
      (fm-quiesce-all kid)))
  md)


(defun fm-kid-replace (old-kid new-kid &aux (fm-parent (fm-parent old-kid)))
     (c-assert (member old-kid (kids fm-parent)) ()
        "~&oldkid ~s not amongst kids of its fm-parent ~s"
        old-kid fm-parent)
     (when fm-parent ;; silly test given above assert--which is right?
        (c-assert (typep fm-parent 'family))
          (setf (fm-parent new-kid) fm-parent)
          (setf (kids fm-parent) (substitute new-kid old-kid (kids fm-parent)))
          ;;(rplaca (member oldkid (kids fm-parent)) newkid)
          new-kid))

;----------------------------------------------------------
;;
;; h i g h  -  o r d e r   f a m i l y   o p s
;;
;; currently not in use...someday?
(defmacro ^fm-min-max-kid (min-max slot-name &key (default 0) test (fm-parent 'self))
   (let ((best (copy-symbol 'best))
         (kid (copy-symbol 'kid))
         )
      `(let ((,best ,default))
          (dolist (,kid (kids ,fm-parent) ,best)
            ,(if test
                `(when (funcall ,test ,kid)
                   (setf ,best (funcall ,min-max ,best (,slot-name ,kid))))
                `(bif (slotvalue (,slot-name ,kid))
                    (setf ,best (funcall ,min-max ,best slotvalue))
                    (break "nil slotvalue ~a in kid ~a of parent ~a"
                           ',slot-name ,kid ,fm-parent)))))))

(defun fm-min-kid (self slot-name)
  (or (loop for k in (^kids)
            minimizing (funcall slot-name k))
    0))
(defun fm-max-kid (self slot-name)
  (or (loop for k in (^kids)
            maximizing (funcall slot-name k))
    0))

(defmacro fm-other (md-name &key (starting 'self) skip-tree (test '#'true-that))
  `(fm-find-one ,starting ,(if (consp md-name)
                               `(list ',(car md-name) ,(cadr md-name))
                             `',md-name)
                :must-find t
                :skip-tree ,skip-tree
                :global-search t
                :test ,test))

(defmacro fm-otherx (md-name &key (starting 'self) skip-tree)
   (if (eql starting 'self)
      `(or (fm-find-one ,starting ,(if (consp md-name)
                                      `(list ',(car md-name) ,(cadr md-name))
                                      `',md-name)
             :must-find t
             :skip-tree ,skip-tree
             :global-search t))
      `(fm-find-one ,starting ,(if (consp md-name)
                                  `(list ',(car md-name) ,(cadr md-name))
                                  `',md-name)
         :must-find t
         :skip-tree ,skip-tree
         :global-search t)))

(defun fm-other-v (md-name starting &optional (global-search t))
  (fm-find-one starting md-name
    :must-find nil
    :global-search global-search))

(defmacro fm-otherv? (md-name &optional (starting 'self) (global-search t))
  `(fm-other-v ,md-name ,starting ,global-search))

(defmacro fm-other? (md-name &optional (starting 'self) (global-search t))
    `(fm-find-one ,starting ,(if (consp md-name)
                                               `(list ',(car md-name) ,(cadr md-name))
                                               `',md-name)
          :must-find nil
          :global-search ,global-search))

(defun fm-other! (starting md-name &optional (global-search t))
  (fm-find-one starting md-name
    :must-find t
    :global-search global-search))

(defmacro fm^ (md-name &key (skip-tree 'self))
  `(fm-find-one (fm-parent self) ,md-name
     :skip-tree ,skip-tree
     :must-find t
     :global-search t))

(defmacro fm? (md-name &optional (starting 'self) (global-search t))
    `(fm-find-one ,starting ,(if (consp md-name)
                                               `(list ',(car md-name) ,(cadr md-name))
                                               `',md-name)
          :must-find nil
          :global-search ,global-search))

(defmacro fm! (md-name &optional (starting 'self))
    `(without-c-dependency 
      (fm-find-one ,starting ,(if (consp md-name)
                                  `(list ',(car md-name) ,(cadr md-name))
                                `',md-name)
        :must-find t
        :global-search nil)))

(defmacro fm-other?! (md-name &optional (starting 'self))
   `(fm-find-one ,starting ,(if (consp md-name)
                                         `(list ',(car md-name) ,(cadr md-name))
                                  `',md-name)
     :must-find nil
     :global-search nil))

(defmacro fm-collect (md-name &key (must-find t))
   `(fm-find-all self ',md-name :must-find ,must-find)) ;deliberate capture

(defmacro fm-map (fn md-name)
         `(mapcar ,fn (fm-find-all self ',md-name))) ;deliberate capture

(defmacro fm-mapc (fn md-name)
   `(mapc ,fn (fm-find-all self ',md-name))) ;deliberate capture

(defun fm-pos (goal &aux (fm-parent (fm-parent goal)))
   (when fm-parent
           (or (position goal (kids fm-parent))
                               (length (kids fm-parent))))) ;; ?!!

(defmacro fm-count-named (family md-name &key (global-search t))
    `(length (fm-find-all ,family ,md-name
                 :must-find nil
                 :global-search ,global-search)))
;---------------------------------------------------------------

(defun fm-top (fm &optional (test #'true-that) &aux (fm-parent (fm-parent fm)))
    (cond ((null fm-parent) fm)
                ((not (funcall test fm-parent)) fm)
                (t (fm-top fm-parent test))))

(defun fm-first-above (fm &key (test #'true-that) &aux (fm-parent (fm-parent fm)))
    (cond ((null fm-parent) nil)
              ((funcall test fm-parent) fm-parent)
              (t (fm-first-above fm-parent :test test))))

(defun fm-nearest-if (test fm)
  (when fm
    (if (funcall test fm)
       fm
       (fm-nearest-if test (fm-parent fm)))))

(defun fm-includes (fm sought)
  (fm-ancestor-p fm sought))

(defun fm-ancestor-p (fm sought)
   (c-assert fm)
   (when sought
      (or (eql fm sought)
          (fm-includes fm (fm-parent sought)))))

(defun fm-kid-containing (fm-parent descendant)
   (with-dynamic-fn (finder (node) (not (eql fm-parent node)))
     (fm-top descendant finder)))

(defun make-name (root &optional subscript)
   (if subscript (list root subscript) root))

(defun name-root (md-name)
   (if (atom md-name) md-name (car md-name)))

(defun name-subscript (md-name)
   (when (consp md-name) (cadr md-name)))

(defun fm-find-one (family md-name &key (must-find t)
                     (global-search t) skip-tree (test #'true-that)
                     &aux diag)
  (count-it :fm-find-one)
  (flet ((matcher (fm)
           (when diag
             (trc "fm-find-one matcher sees" md-name fm (md-name fm)))
           (when (and (eql (name-root md-name)(md-name fm))
                   (or (null (name-subscript md-name))
                     (eql (name-subscript md-name) (fm-pos fm)))
                   (progn
                     (when diag
                       (trc "fm-find-one testing" fm))
                     (funcall test fm)))
             (throw 'fm-find-one fm))))
    #-lispworks (declare (dynamic-extent matcher))
    (trc nil "fm-find-one> entry " md-name family)    
    (let ((match (catch 'fm-find-one
                   (fm-traverse family #'matcher
                     :skip-tree skip-tree
                     :global-search global-search))))
      (when (and must-find (null match))
        (trc "fm-find-one > erroring fm-not-found" family md-name must-find global-search)
        ;;(inspect family)
        (setq diag t must-find nil)
        (fm-traverse family #'matcher
                     :skip-tree skip-tree
                     :global-search global-search)
        (c-break "fm-find-one > *stop*ping...did not find ~a ~a ~a" family md-name global-search)
        )
      match)))

(defun fm-find-kid (self name)
   (find name (kids self) :key #'md-name))

(defun fm-kid-typed (self type)
   (c-assert self)
  (find type (kids self) :key #'type-of))

(defun kid-no (self)
  (unless (typep self 'model-object)
    (break "not a model object ~a" self))
  (when (and self (fm-parent self))
    (c-assert (member self (kids (fm-parent self))))
    (position self (kids (fm-parent self)))))


