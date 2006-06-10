

(defpackage #:tu-rule-once-then-input (:use :cl :utils-kt :cells :tu-cells))
(in-package #:tu-rule-once-then-input)


#|

Often in interactive applications one needs to do interesting things to come up
with an initial value for a field which then is to be edited by a user, or
for some other reason regularly fed as a C-INPUT.

|#

(defvar *db-entry*)

(defun get-age (id)
  (bwhen (props (cdr (assoc id *db-entry* :test 'string=)))
    (getf props :age)))

(defmodel kenny-view ()
  ((age :accessor age :initform (c-formula (:inputp t)
                                  (- (get-age "555-55-5555")
                                    (^grecian-formula-amt))))
   (grecian-formula-amt :accessor grecian-formula-amt
      :initform (c-in 5))))

(defobserver age ((self kenny-view))
  (setf (getf (cdr (assoc "555-55-5555" *db-entry* :test 'string=)) :age) new-value))

#+test
(let ((*db-entry* (copy-list '(("555-55-5555" . (:name "ken" :age 54))
               ("666-66-6666" . (:name "satan" :age most-positive-fixnum))))))
  (cells-reset)
  (let ((kv (make-instance 'kenny-view)))
    (print `(:age-init ,(age kv)))
    (assert (= 49 (age kv)))

    (incf (grecian-formula-amt kv) 10) ;; try looking younger
    (assert (= 15 (grecian-formula-amt kv)))

    (assert (= 49 (age kv))) ;; unchanged -- the age rule is gone

    (print `(:happy-birthday ,(incf (age kv))))
    (assert (= 50 (age kv)(get-age "555-55-5555")))
    ;
    ; just showin' off...
    (assert (= 51 (1+ (age kv))(incf (age kv))(get-age "555-55-5555")))))