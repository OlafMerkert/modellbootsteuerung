(defpackage :joystick-eingabe
  (:nicknames :joystick)
  (:use :cl :ol )
  (:export
   :list-joysticks))

(in-package :joystick-eingabe)


(defun experiment-fn ()
  "figure out how to use sdl"
  (sdl:with-init (sdl:sdl-init-joystick)
    (sdl:with-events ()
      (:quit-event () t)
      (:joy-axis-motion-event
       (:which joystick :axis axis :value value)
       (format t "Joystick ~A moved axis ~A to ~D" joystick axis value)))))

(defun list-joysticks ()
  "list all the joysticks found on this system, with name."
  (sdl:with-init (sdl:sdl-init-joystick)
    (loop for i from 0 below (sdl:num-joysticks)
       collect (sdl:sdl-joystick-name i))))

(defparameter fighterstick-spec
  '(:id    1
    :gas   2
    :ruder 0))

(defun read-joystick-continually (js-spec)
  (sdl:with-init (sdl:sdl-init-joystick)
    (destructuring-bind (&key id gas ruder) js-spec
      (let ((js (lispbuilder-sdl-cffi::sdl-joystick-open id)))
        (unwind-protect
             (loop do
                  (format t "Gas:  ~A   Ruder:  ~A~%"
                          (lispbuilder-sdl-cffi::sdl-joystick-get-axis js gas)
                          (lispbuilder-sdl-cffi::sdl-joystick-get-axis js ruder))
                  (sleep 1))
          (lispbuilder-sdl-cffi::sdl-joystick-close js))))))
