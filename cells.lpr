;; -*- lisp-version: "8.1 [Windows] (Oct 11, 2008 17:00)"; cg: "1.103.2.10"; -*-

(in-package :cg-user)

(defpackage :CELLS)

(define-project :name :cells
  :modules (list (make-instance 'module :name "defpackage.lisp")
                 (make-instance 'module :name "trc-eko.lisp")
                 (make-instance 'module :name "cells.lisp")
                 (make-instance 'module :name "integrity.lisp")
                 (make-instance 'module :name "cell-types.lisp")
                 (make-instance 'module :name "constructors.lisp")
                 (make-instance 'module :name "initialize.lisp")
                 (make-instance 'module :name "md-slot-value.lisp")
                 (make-instance 'module :name "slot-utilities.lisp")
                 (make-instance 'module :name "link.lisp")
                 (make-instance 'module :name "propagate.lisp")
                 (make-instance 'module :name "synapse.lisp")
                 (make-instance 'module :name "synapse-types.lisp")
                 (make-instance 'module :name "model-object.lisp")
                 (make-instance 'module :name "defmodel.lisp")
                 (make-instance 'module :name "md-utilities.lisp")
                 (make-instance 'module :name "family.lisp")
                 (make-instance 'module :name "fm-utilities.lisp")
                 (make-instance 'module :name "family-values.lisp")
                 (make-instance 'module :name "test-propagation.lisp")
                 (make-instance 'module :name "cells-store.lisp")
                 (make-instance 'module :name "test-cc.lisp"))
  :projects (list (make-instance 'project-module :name
                                 "utils-kt\\utils-kt" :show-modules
                                 nil))
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :cells
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules nil
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :local-name-info)
  :build-flags (list :allow-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :build-number 0
  :on-initialization 'cells::test-with-cc
  :on-restart 'do-default-restart)

;; End of Project Definition
