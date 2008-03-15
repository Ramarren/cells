
#|

From: Erik Naggum (erik@naggum.no)
Subject: Re: XML->sexpr ideas
Newsgroups: comp.lang.lisp
Date: 2004-01-19 04:24:43 PST

* Kenny Tilton
| Of course it is easy enough for me to come up with a sexpr format off
| the top of my head, but I seem to recall someone (Erik? Tim? Other?)
| saying they had done some work on a formal approach to an alternative
| to XML/HTML/whatever.
| 
| True that? If so, I am all ears.

  Really?  You are?  Maybe I didn't survive 2003 and this is some Hell
  where people have to do eternal penance, and now I get to do SGML all
  over again.

  Much processing of SGML-like data appears to be stream-like and will
  therefore appear to be equivalent to an in-order traversal of a tree,
  which can therefore be represented with cons cells while the traverser
  maintains its own backward links elsewhere, but this is misleading.

  The amount of work and memory required to maintain the proper backward
  links and to make the right decisions is found in real applications to
  balloon and to cause random hacks; the query languages reflect this
  complexity.  Ease of access to the parent element is crucial to the
  decision-making process, so if one wants to use a simple list to keep
  track of this, the most natural thing is to create a list of the
  element type, the parent, and the contents, such that each element has
  the form (type parent . contents), but this has the annoying property
  that moving from a particular element to the next can only be done by
  remembering the position of the current element in a list, just as one
  cannot move to the next element in a list unless you keep the cons
  cell around.  However, the whole point of this exercise is to be able
  to keep only one pointer around.  So the contents of an element must
  have the form (type parent contents . tail) if it has element contents
  or simply a list of objects, or just the object if simple enough.

  Example: <foo>123</foo> would thus be represented by (foo nil "123"),
  <foo>123</foo><bar>456</bar> by (foo nil "123" bar nil "456"), and
  <zot><foo>123</foo><bar>456</bar></zot> by #1=(zot nil (foo #1# "123"
  bar #1# "456")).

  Navigation inside this kind of structure is easy: When the contents in
  CADDR is exhausted, the CDDDR is the next element, or if NIL, we have
  exhausted the contents of the parent and move up to the CADR and look
  for its next element, etc.  All the important edges of the containers
  that make up the *ML document are easily detectible and the operations
  that are usually found at the edges are normally tied to the element
  type (or as modified by its parents), are easily computable.  However,
  using a list for this is cumbersome, so I cooked up the «quad».  The
  «quad» is devoid of any intrinsic meaning because it is intended to be
  a general data structure, so I looked for the best meaningless names
  for the slots/accessors, and decided on QAR, QBR, QCR, and QDR.  The
  quad points to the element type (like the operator in a sexpr) in the
  QAR, the parent (or back) quad in the QBR, the contents of the element
  in the QCR, and the usual pointer to the next quad in the QDR.

  Since the intent with this model is to «load» SGML/XML/SALT documents
  into memory, one important issue is how to represent long stretches of
  character content or binary content.  The quad can easily be used to
  represent a (sequence of) entity fragments, with the source in QAR,
  the start position in QBR, and the end position in QCR, thereby using
  a minimum of memory for the contents.  Since very large documents are
  intended to be loaded into memory, this property is central to the
  ability to search only selected elements for their contents -- most
  searching processors today parse the entire entity structure and do
  very little to maintain the parsed element structure.

  Speaking of memory, one simple and efficient way to implement the quad
  on systems that lack the ability to add native types without overhead,
  is to use a two-dimensional array with a second dimension of 4 and let
  quad pointers be integers, which is friendly to garbage collection and
  is unambiguous when the quad is used in the way explained above.

  Maybe I'll talk about SALT some other day.

-- 
Erik Naggum | Oslo, Norway

Act from reason, and failure makes you rethink and study harder.
Act from faith, and failure makes you blame someone and push harder.

|#

(in-package :ukt)

;;;(defstruct (juad jar jbr jcr jdr)


  
(defun qar (q) (car q))
(defun (setf qar) (v q) (setf (car q) v))

(defun qbr (q) (cadr q))
(defun (setf qbr) (v q) (setf (cadr q) v))

(defun qcr (q) (caddr q))
(defun (setf qcr) (v q) (setf (caddr q) v))

(defun qdr (q) (cdddr q))
(defun (setf qdr) (v q) (setf (cdddr q) v))

(defun sub-quads (q)
  (loop for childq on (qcr q) by #'qdr
      collecting childq))

(defun sub-quads-do (q fn)
  (loop for childq on (qcr q) by #'qdr
      do (funcall fn childq)))

(defun quad-traverse (q fn &optional (depth 0))
  (funcall fn q depth)
  (sub-quads-do q
    (lambda (subq)
      (quad-traverse subq fn (1+ depth)))))

(defun quad (operator parent contents next)
  (list operator parent contents next))

(defun quad* (operator parent contents next)
  (list operator parent contents next))

(defun qups (q)
  (loop for up = (qbr q) then (qbr up)
        unless up do (loop-finish)
        collecting up))

(defun quad-tree (q)
  (list* (qar q)
    (loop for childq on (qcr q) by #'qdr
        while childq
          collecting (quad-tree childq))))

(defun tree-quad (tree &optional parent)
  (let* ((q (quad (car tree) parent nil nil))
         (kids (loop for k in (cdr tree)
                     collecting (tree-quad k q))))
    (loop for (k n) on kids
          do (setf (qdr k) n))
    (setf (qcr q) (car kids))
    q))

#+test
(test-qt)

(defun test-qt ()
  (print (quad-tree #1='(zot nil (foo #1# ("123" "abc")
                                . #2=(bar #1# (ding #2# "456"
                                                dong #2# "789")))))))

(print #1='(zot nil (foo #1# ("123" "abc")
                          . #2=(bar #1# (ding #2# "456"
                                          dong #2# "789")))))
#+xxxx
(test-tq)

(defun test-tq ()
  (let ((*print-circle* t)
        (tree '(zot (foo ("123")) (bar (ding) (dong)))))
    (assert (equal tree (quad-tree (tree-quad tree))))))

(defun testq ()
  (let ((*print-circle* t))
    (let ((q #1='(zot nil (foo #1# ("123" "abc")
                            . #2=(bar #1# (ding #2# "456"
                                            dong #2# "789"))))))
      (print '(traverse showing each type and data preceded by its depth))
      
      (quad-traverse q (lambda (q depth)
                         (print (list depth (qar q)(qcr q)))))
      (print `(listify same ,(quad-tree q))))
    (let ((q #2='(zot nil (ding #2# "456"
                                  dong #2# "789"))))
      (print '(traverse showing each "car" and itd parentage preceded by its depth))
      (print '(of data (zot (ding (dong)))))
      (quad-traverse q (lambda (q depth)
                         (print (list depth (qar q)
                                  (mapcar 'qar (qups q)))))))))

;;;(defun tree-quad (tree)
  

(defun testq2 ()
  (let ((*print-circle* t))
    (let ((q #2='(zot nil (ding #2# "456"
                            dong #2# "789"))))
      (print '(traverse showing each "car" and itd parentage preceded by its depth))
      (print '(of data (zot (ding (dong)))))
      (quad-traverse q (lambda (q depth)
                         (print (list depth (qar q)
                                  (mapcar 'qar (qups q)))))))))


              
  