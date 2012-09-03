(defpackage :fernsteuerung-gui
  (:use :clim :clim-lisp :ol)
  (:export
   :run-model-selector))

(in-package :fernsteuerung-gui)

(define-application-frame model-selector ()
  ((models           :initarg :models
                     :initform nil
                     :accessor models)
   (selected-model   :initform nil
                     :accessor selected-model)
   (bindings         :initarg :bindings
                     :initform nil
                     :accessor bindings)
   (selected-binding :initform nil
                     :accessor selected-binding))
  (:panes
   (model-menu (make-pane 'list-pane
                          :items #2=(models *application-frame*)
                          :mode :one-of))
   (binding-menu (make-pane 'list-pane
                            :items #3=(bindings *application-frame*)
                            :mode :one-of))
   (start-button (make-pane 'push-button-pane
                            :label "Steuerung starten"
                            :activate-callback 'start-steuerung))
   (quit-button (make-pane 'push-button-pane
                           :label "Abbrechen")))
  (:layouts (default
                (vertically ()
                  (300 (horizontally ()
                       (200 model-menu)
                       (200 binding-menu)))
                  (25 (horizontally ()
                        (120 quit-button)
                        (120 start-button)))))))

(define-model-selector-command (com-quit-frame :name t :menu nil)
    ()
  (frame-exit *application-frame*))

(defun run-model-selector ()
  (run-frame-top-level
   (make-application-frame 'model-selector :pretty-name "Modellauswahl"
                           :models (datprot:model-list))))

(defun start-steuerung (button)
  (declare (ignorable button)))
