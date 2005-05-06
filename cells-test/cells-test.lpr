;; -*- lisp-version: "7.0 [Windows] (Apr 6, 2005 17:03)"; cg: "1.54.2.17"; -*-

(in-package :cg-user)

(defpackage :CELLS)

(define-project :name :cells-test
  :modules (list (make-instance 'module :name "test.lisp")
                 (make-instance 'module :name "hello-world.lisp")
                 (make-instance 'module :name
                                "internal-combustion.lisp")
                 (make-instance 'module :name "boiler-examples.lisp")
                 (make-instance 'module :name "person.lisp")
                 (make-instance 'module :name "df-interference.lisp")
                 (make-instance 'module :name "test-family.lisp")
                 (make-instance 'module :name "test-kid-slotting.lisp")
                 (make-instance 'module :name "lazy-propagation.lisp")
                 (make-instance 'module :name "output-setf.lisp")
                 (make-instance 'module :name "test-lazy.lisp")
                 (make-instance 'module :name "synapse-testing.lisp"))
  :projects (list (make-instance 'project-module :name "..\\cells"))
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
  :include-flags '(:local-name-info)
  :build-flags '(:allow-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'cells::cv-test
  :on-restart 'do-default-restart)

;; End of Project Definition
