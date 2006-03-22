;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
;;; 
;;;
;;; Copyright © 1995,2003 by Kenneth William Tilton.
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

(in-package :utils-kt)

(eval-when (compile load eval)
  (export '(os-tickcount time-of-day now hour-min-of-day time-in-zone dd-mmm-yy mmm-dd-yyyy)))

(defun os-tickcount ()
  (cl:get-internal-real-time))

(defun now ()
  (/ (get-internal-real-time)
    internal-time-units-per-second))

(defun time-of-day (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~A:~2,,,'0@A:~2,,,'0@A" hours minutes seconds)))

(defun hour-min-of-day (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~2,,,'0@A:~2,,,'0@A" hours minutes)))

(defun time-in-zone (inzone &optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylightsavingsp this-zone)
    (decode-universal-time i-time)
      (declare (ignorable this-zone day-of-week daylightsavingsp))
    (encode-universal-time seconds minutes hours date month year (- inzone (if daylightsavingsp 1 0)))))

(defun dd-mmm-yy (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~A-~A-~2,,,'0@A" date (month-abbreviation month)
           (mod year 100))))

(defun mmm-dd-yyyy (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~A ~A, ~A" (month-abbreviation month)
            date year)))

(eval-when (compile load eval)
  (export '(month-abbreviation weekday-abbreviation week-time mdyy-yymd u-time u-date)))

(defun month-abbreviation (month)
  (elt '("Jan" "Feb" "Mar" "Apr" "May" "June"
         "July" "Aug" "Sept" "Oct" "Nov" "Dec") (1- month)))

(defun weekday-abbreviation (day)
  (elt '("Mon" "Tue" "Wed" "Thur" "Fri" "Sat" "Sun") day))

(defun week-time (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~A ~A ~A, ~A ~a:~2,'0d ~a"
      (weekday-abbreviation day-of-week)
      (month-abbreviation month)
      
      date
      year
      (if (= 12 hours) hours (mod hours 12))  ; JP 010911 since (mod 12 12) = 0, treat 12 as a special case.
      minutes (if (>= hours 12) "PM" "AM"))))


(defun mdyy-yymd (d)
  (assert (eql 8 (length d)))
  (conc$ (right$ d 4) (left$ d 4)))

(defun u-time (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~2,d:~2,'0d ~a"
      ;; /// time-zone, really Naggum's stuff
      (mod hours 12) minutes
      (if (>= hours 12) "PM" "AM"))))

(defun u-date (&optional (i-time (get-universal-time)))
  (multiple-value-bind
        (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
      (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                               month year day-of-week
                               daylight-saving-time-p time-zone))
    (format nil "~A-~A-~A"
      date
      (elt '("Jan" "Feb" "Mar" "Apr" "May" "June"
             "July" "Aug" "Sept" "Oct" "Nov" "Dec") (1- month))
      year
      )))

(eval-when (compile load eval)
  (export '(u-day multiple-value-bind m/d/y mm/dd yyyy-mm-dd)))

(defun u-day (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (elt '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") day-of-week)))

(defun u-day3 (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (elt '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week)))

(defun m/d/y (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~2,,,'0@A/~2,,,'0@A/~2,,,'0@A" month date (mod year 100))))

(defun mm/dd (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~2,,,'0@A/~2,,,'0@A" month date)))

(defun yyyy-mm-dd (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~4,,,'0@A~2,,,'0@A~2,,,'0@A"
      year month date)))

(eval-when (compile load eval)
  (export '(ymdhmsh)))

(defun ymdhmsh (&optional (i-time (get-universal-time)))
  (multiple-value-bind
    (seconds minutes hours date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time i-time)
    (declare (ignorable seconds minutes hours date
                                 month year day-of-week
                                 daylight-saving-time-p time-zone))
    (format nil "~4,,,'0@A:~2,,,'0@A:~2,,,'0@A:~2,,,'0@A:~2,,,'0@A:~2,,,'0@A:~2,,,'0@A"
      year month date hours minutes seconds (floor (now) 10))))

(defun hyphenated-time-string ()
  (substitute #\- #\: (ymdhmsh)))
  

