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
   (model-menu (make-pane 'option-pane
                          :items (models *application-frame*)
                          :mode :one-of))
   (binding-menu (make-pane 'option-pane
                            :items (bindings *application-frame*)
                            :mode :one-of))
   (start-button (make-pane 'push-button-pane
                            :label "Steuerung starten"
                            :activate-callback 'start-steuerung))
   (quit-button (make-pane 'push-button-pane
                           :label "Abbrechen")))
  (:layouts (default
                (vertically (:width 400)
                  (300 (horizontally ()
                       (1/2 model-menu)
                       (1/2 binding-menu)))
                  (25 (horizontally ()
                        (60 quit-button)
                        (60 start-button)))))))

(define-model-selector-command (com-quit-frame :name t :menu nil)
    ()
  (frame-exit *application-frame*))

(defun run-model-selector ()
  (run-frame-top-level
   (make-application-frame 'model-selector :pretty-name "Modellauswahl"
                           :models (datprot:model-list))))
