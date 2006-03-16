;; -*- lisp-version: "8.0 [Windows] (Mar 7, 2006 20:04)"; cg: "1.81"; -*-

(in-package :cg-user)

(defpackage :CELLS)

(define-project :name :test
  :modules (list (make-instance 'module :name "test.lisp")
                 (make-instance 'module :name "test-ephemeral.lisp")
                 (make-instance 'module :name "test-cycle.lisp")
                 (make-instance 'module :name "test-synapse.lisp")
                 (make-instance 'module :name "output-timing.lisp"))
  :projects (list (make-instance 'project-module :name "..\\cells"))