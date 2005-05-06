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


(defun md-slot-value (self slot-name &aux (c (md-slot-cell self slot-name)))
  (when *stop*
    (princ #\.)
    (return-from md-slot-value))
  ;; (count-it :md-slot-value slot-name)
  (if c
      (prog1
          (with-integrity (:md-slot-value)
            (c-value-ensure-current c))
        (when (car *c-calculators*)
          (c-link-ex c)))
    (values (bd-slot-value self slot-name) nil)))
  
(defun c-value-ensure-current (c)
  (count-it :c-value-ensure-current)
  (trc nil "c-value-ensure-current>" c)
  (cond
   ((c-inputp c))
   ((c-currentp c))
   ((or (not (c-validp c)) 
      (c-influenced-by-pulse c))
    (c-calculate-and-set c))
   (t (c-pulse-update c :valid-uninfluenced)))

  ;;(unless (cmdead c)
  (when (c-unboundp c)
    (error 'unbound-cell :instance (c-model c) :name (c-slot-name c)))

  (c-value c))

(defun c-influenced-by-pulse (c); &aux (ip *data-pulse-id*))
  (unless (c-currentp c)
    (count-it :c-influenced-by-pulse)
    (trc c "c-influenced-by-pulse> " c (c-useds c))
    (some (lambda (used)
            (c-value-ensure-current used)
            (when (and (c-changed used) (> (c-pulse used)(c-pulse c)))
              #+chya (trc nil "used changed" used :asker c
                :inpulse ip :pulse *data-pulse-id*)
              t))
      (c-useds c))))

(defun c-calculate-and-set (c)
  (flet ((body ()
           (when (c-stopped)
             (princ #\.)
             (return-from c-calculate-and-set))
    
           (when (find c *c-calculators*) ;; circularity
             (trc "c-calculate-and-set breaking on circularity" c)
             (c-break ;; break is problem when testing cells on some CLs
              "cell ~a midst askers: ~a" c *c-calculators*))
    
           (count-it :c-calculate-and-set)
           ;;;  (count-it :c-calculate-and-set (type-of (c-model c))) ;; (c-slot-name c))
    
           (cd-usage-clear-all c)
    
           (let ((raw-value
                  (progn
                    (let ((*c-calculators* (cons c *c-calculators*)))
                      (trc nil "c-calculate-and-set> new *c-calculators*:"
                        *c-calculators*)
                      (c-assert (c-model c))
                      (funcall (cr-rule c) c)))))
             (when (and *c-debug* (typep raw-value 'cell))
               (c-break "new value for cell ~s is itself a cell: ~s. probably nested (c? ... (c? ))"
                 c raw-value))
        
             (c-unlink-unused c)
             (md-slot-value-assume c raw-value))))
    (if nil ;; *dbg*
        (ukt::wtrc (0 100 "calcnset" c) (body))(body))))

;-------------------------------------------------------------


(defun md-slot-makunbound (self slot-name
                            &aux (c (md-slot-cell self slot-name)))
  (unless c
    (c-break ":md-slot-makunbound > cellular slot ~a of ~a cannot be unbound unless initialized as inputp"
      slot-name self))

  (when (eql :unbound (c-value-state c))
    (return-from md-slot-makunbound nil))
  
  (without-c-dependency
   ; --- cell & slot maintenance ---
   (let ((prior-value (c-value c)))
     (setf (c-value-state c) :unbound
       (c-value c) nil
       (c-state c) :awake)
     (bd-slot-makunbound self slot-name)
   
     ; --- data flow propagation -----------
     ;
     (setf (c-changed c) t)
     (when (eql '.kids (c-slot-name c))
       (md-kids-change (c-model c) nil prior-value :makunbound))

     (let ((causation *causation*))
       (with-integrity (:makunbound :makunbound c)
         (let ((*causation* causation))
           (c-propagate c prior-value t)))))))


(defun (setf md-slot-value) (new-value self slot-name
                              &aux (c (md-slot-cell self slot-name)))
  
  (trc nil "(setf md-slot-value)" *data-pulse-id* c 
    new-value self slot-name)
  (when *c-debug*
    (c-setting-debug self slot-name c new-value))
  
  (if c
      (when (find c *causation*)
        (case (c-cyclicp c)
          (:run-on (trc "cyclicity running on" c))
          ((t)
            (progn
              (trc "cyclicity handled gracefully" c)
              (c-pulse-update c :cyclicity-1)
              (return-from md-slot-value new-value)))
          (otherwise
           (c-break "(setf md-slot-value) setf looping ~a ~a" c *causation*))))
    (progn
      (c-break "(setf md-slot-value)> cellular slot ~a of ~a cannot be setf unless initialized as inputp"
        slot-name self)))
  
  (let ((causation *causation*))
    (with-integrity (:setf :setf c new-value)
      (let ((*causation* causation))
        (trc nil "(setf md-slot-value) calling assume" c new-value)
        (md-slot-value-assume c new-value))))

  new-value)


                    
(defmethod md-slot-value-assume (c raw-value)
  (assert c)
  (bif (c-pos (position c *causation*))
    (bif (cyclic-pos (position-if 'c-cyclicp *causation* :end c-pos))
      (progn
        (c-pulse-update c :cyclicity-0)
        (return-from md-slot-value-assume raw-value))
      (c-break "md-slot-value-assume looping ~a ~a" c *causation*)))

  (without-c-dependency
   (let ((prior-state (c-value-state c))
         (prior-value (c-value c))
         (absorbed-value (c-absorb-value c raw-value)))
     
     ; --- slot maintenance ---
     (unless (c-synaptic c)
       (md-slot-value-store (c-model c) (c-slot-name c) absorbed-value))
     
     ; --- cell maintenance ---
     (c-pulse-update c :slotv-assume)
     (setf
      (c-value c) absorbed-value
      (c-value-state c) :valid
      (c-state c) :awake)

     (unless (typep c 'c-stream) ;; c-stream needs to run out first stream at least
       (c-optimize-away?! c)) ;;; put optimize test here to avoid needless linking
     
     
     ; --- data flow propagation -----------
     ;
     (if (and (eql prior-state :valid)
           (c-no-news c absorbed-value prior-value))
         (progn
           (trc nil "(setf md-slot-value) >no-news" prior-state (c-no-news c absorbed-value prior-value))
           (count-it :no-news))
       (progn
         (setf (c-changed c) t)
         (trc nil "sv-assume: flagging as changed" c absorbed-value prior-value prior-state)
         (when (eql '.kids (c-slot-name c))
           (md-kids-change (c-model c) absorbed-value prior-value :mdslotvalueassume))

         (c-propagate c prior-value (not (eql :unbound prior-state)))))
     
     absorbed-value)))


    