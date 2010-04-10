;; -*- lisp-version: "8.2 [64-bit Linux (x86-64)] (Mar 3, 2010 14:34)"; cg: "1.134"; -*-

(in-package :cg-user)

(defpackage :cells)

(define-project :name :cells-test
  :modules (list (make-instance 'module :name "test.lisp")
                 (make-instance 'module :name "hello-world.lisp")
                 (make-instance 'module :name "test-kid-slotting.lisp")
                 (make-instance 'module :name "test-lazy.lisp")
                 (make-instance 'module :name "person.lisp")
                 (make-instance 'module :name "df-interference.lisp")
                 (make-instance 'module :name "test-family.lisp")
                 (make-instance 'module :name "output-setf.lisp")
                 (make-instance 'module :name "test-cycle.lisp")
                 (make-instance 'module :name "test-ephemeral.lisp")
                 (make-instance 'module :name "test-synapse.lisp")
                 (make-instance 'module :name "deep-cells.lisp"))
  :projects (list (make-instance 'project-module :name "../cells" :show-modules nil))
  :libraries nil
  :editable-files nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :cells
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules (list :cg-dde-utils :cg.base)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags nil
  :build-flags (list :allow-runtime-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+M +t \"Console for Debugging\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :build-number 0
  :run-with-console nil
  :project-file-version-info nil
  :on-initialization 'cells::test-cells
  :default-error-handler-for-delivery 'report-unexpected-error-and-exit
  :on-restart 'do-default-restart)

;; End of Project Definition
