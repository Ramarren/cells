;; -*- lisp-version: "8.0 [Windows] (May 22, 2006 0:51)"; cg: "1.81"; -*-

(in-package :cg-user)

(defpackage :TU-CELLS)

(define-project :name :tutorial
  :modules (list (make-instance 'module :name "test.lisp")
                 (make-instance 'module :name "01-lesson.lisp")
                 (make-instance 'module :name "01a-dataflow.lisp")
                 (make-instance 'module :name
                                "01b-change-handling.lisp")
                 (make-instance 'module :name "01c-cascade.lisp")
                 (make-instance 'module :name "02-lesson.lisp")
                 (make-instance 'module :name "03-ephemeral.lisp")
                 (make-instance 'module :name
                                "04-formula-once-then-input.lisp")
                 (make-instance 'module :name "05-class-cell.lisp")
                 (make-instance 'module :name
                                "..\\gotchas\\lost-ephemeral-init.lisp")
                 (make-instance 'module :name "chat-cells.lisp")
                 (make-instance 'module :name "df-interference.lisp"))
  :projects (list (make-instance 'project-module :name "..\\cells"))
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :tu-cells
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.bitmap-pane
                     :cg.bitmap-pane.clipboard :cg.bitmap-stream
                     :cg.button :cg.caret :cg.check-box :cg.choice-list
                     :cg.choose-printer :cg.clipboard
                     :cg.clipboard-stack :cg.clipboard.pixmap
                     :cg.color-dialog :cg.combo-box :cg.common-control
                     :cg.comtab :cg.cursor-pixmap :cg.curve
                     :cg.dialog-item :cg.directory-dialog
                     :cg.directory-dialog-os :cg.drag-and-drop
                     :cg.drag-and-drop-image :cg.drawable
                     :cg.drawable.clipboard :cg.dropping-outline
                     :cg.edit-in-place :cg.editable-text
                     :cg.file-dialog :cg.fill-texture
                     :cg.find-string-dialog :cg.font-dialog
                     :cg.gesture-emulation :cg.get-pixmap
                     :cg.get-position :cg.graphics-context
                     :cg.grid-widget :cg.grid-widget.drag-and-drop
                     :cg.group-box :cg.header-control :cg.hotspot
                     :cg.html-dialog :cg.html-widget :cg.icon
                     :cg.icon-pixmap :cg.ie :cg.item-list
                     :cg.keyboard-shortcuts :cg.lamp :cg.lettered-menu
                     :cg.lisp-edit-pane :cg.lisp-text :cg.lisp-widget
                     :cg.list-view :cg.mci :cg.menu :cg.menu.tooltip
                     :cg.message-dialog :cg.multi-line-editable-text
                     :cg.multi-line-lisp-text :cg.multi-picture-button
                     :cg.multi-picture-button.drag-and-drop
                     :cg.multi-picture-button.tooltip :cg.ocx
                     :cg.os-widget :cg.os-window :cg.outline
                     :cg.outline.drag-and-drop
                     :cg.outline.edit-in-place :cg.palette
                     :cg.paren-matching :cg.picture-widget
                     :cg.picture-widget.palette :cg.pixmap
                     :cg.pixmap-widget :cg.pixmap.file-io
                     :cg.pixmap.printing :cg.pixmap.rotate :cg.printing
                     :cg.progress-indicator :cg.project-window
                     :cg.property :cg.radio-button :cg.rich-edit
                     :cg.rich-edit-pane :cg.rich-edit-pane.clipboard
                     :cg.rich-edit-pane.printing :cg.sample-file-menu
                     :cg.scaling-stream :cg.scroll-bar
                     :cg.scroll-bar-mixin :cg.selected-object
                     :cg.shortcut-menu :cg.static-text :cg.status-bar
                     :cg.string-dialog :cg.tab-control
                     :cg.template-string :cg.text-edit-pane
                     :cg.text-edit-pane.file-io :cg.text-edit-pane.mark
                     :cg.text-or-combo :cg.text-widget :cg.timer
                     :cg.toggling-widget :cg.toolbar :cg.tooltip
                     :cg.trackbar :cg.tray :cg.up-down-control
                     :cg.utility-dialog :cg.web-browser
                     :cg.web-browser.dde :cg.wrap-string
                     :cg.yes-no-list :cg.yes-no-string :dde)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:top-level :debugger)
  :build-flags '(:allow-runtime-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+M +t \"Console for Debugging\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'tu-cells::tu-chat-2
  :on-restart 'do-default-restart)

;; End of Project Definition
