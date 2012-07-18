(defpackage :joystick-eingabe
  (:nicknames :joystick)
  (:use :cl :ol )
  (:export
   :list-joysticks))

(in-package :joystick-eingabe)


(defun joystick-main-loop (js-spec data)
  "listen for joystick events, put the axis data into DATA according
to JS-SPEC and call the SEND method on DATA from time to time."
  (destructuring-bind (&key id gas ruder) js-spec
   (sdl:with-init (sdl:sdl-init-joystick)
     (sdl:window 400 100
                 :title-caption "Hello Katja"
                 :icon-caption  "Hello Katja")
     (setf (sdl:frame-rate) 30)
     (sdl-cffi::sdl-joystick-event-state sdl-cffi::sdl-enable)
     (let ((js   (sdl-cffi::sdl-joystick-open id)))
       (sdl:with-events ()
         (:quit-event ()
                      (when js
                        (sdl-cffi::sdl-joystick-close js))
                      t)
         (:joy-axis-motion-event
          (:which joystick :axis axis :value value)
          (cond ((eql axis gas)
                 (setf (gas data) value))
                ((eql axis ruder)
                 (setf (ruder data) value))))
         (:idle ()
                (sdl:clear-display sdl:*black*)
                (sdl:draw-string-solid-* (format nil "Hello Katja")
                                         200 50
                                         :color sdl:*white*
                                         :justify :center)
                (sdl:update-display))
         (:video-expose-event ()
                              (sdl:update-display)))))))

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
