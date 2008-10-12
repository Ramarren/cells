;; -*- lisp-version: "8.1 [Windows] (Oct 11, 2008 17:00)"; cg: "1.103.2.10"; -*-

(in-package :cg-user)

(define-project :name :utils-kt
  :modules (list (make-instance 'module :name "defpackage.lisp")
                 (make-instance 'module :name "core.lisp")
                 (make-instance 'module :name "debug.lisp")
                 (make-instance 'module :name "flow-control.lisp")
                 (make-instance 'module :name "detritus.lisp")
                 (make-instance 'module :name "strings.lisp")
                 (make-instance 'module :name "datetime.lisp")
                 (make-instance 'module :name "split-sequence.lisp"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-lisp
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
  :on-initialization 'default-init-function
  :on-restart 'do-default-restart)

;; End of Project Definition
