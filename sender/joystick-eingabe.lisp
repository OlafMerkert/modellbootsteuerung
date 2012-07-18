(defpackage :joystick-eingabe
  (:nicknames :joystick)
  (:use :cl :ol )
  (:export
   :list-joysticks
   :steuerung-main))

(in-package :joystick-eingabe)

(defparameter servo-max (- (expt 2 datenprotokoll:servo-resolution) 1))

(defclass steuerungsdaten ()
  ((gas   :initarg :gas
          :initform 0
          :accessor gas)
   (ruder :initarg :ruder
          :initform 0
          :accessor ruder))
  (:documentation "halte die Steuerungsdaten für das Boot vor."))

(defmethod get-gas ((steuerungsdaten steuerungsdaten))
  (floor (gas steuerungsdaten)))

(defmethod get-ruder ((steuerungsdaten steuerungsdaten))
  (floor (ruder steuerungsdaten)))

(defclass steuerungsdaten-with-curves (steuerungsdaten)
  ((gas-curve :initarg :gas-curve
              :initform #1=(make-instance 'affine-curve
                                          :in-min  -32768
                                          :in-max 32767
                                          :out-min 0
                                          :out-max servo-max)
              :accessor gas-curve)
   (ruder-curve :initarg :ruder-curve
                :initform #1#
                :accessor ruder-curve))
  (:documentation "filter daten through curves, before one sends
  them."))

(defclass affine-curve ()
  ((scale)
   (in-min  :initarg :in-min
            :initform 0
            :reader in-min)
   (in-max  :initarg :in-max
            :initform 1
            :reader in-max)
   (out-min :initarg :out-min
            :initform 0
            :reader out-min)
   (out-max :initarg :out-max
            :initform 1
            :reader out-max))
  (:documentation "affine transformation of input data to output data."))

(defmethod initialize-instance :after ((affine-curve affine-curve) &rest args)
  (declare (ignore args))
  (with-slots (scale in-min in-max out-min out-max) affine-curve
    (setf scale
          (coerce (/ (- out-max out-min)
                     (- in-max in-min)) 'float))))

(defmethod apply-curve ((curve affine-curve) number)
  (with-slots (scale in-min  out-min) curve
    (+ (* scale (- number in-min)) out-min)))

(defmethod get-gas ((steuerungsdaten-with-curves steuerungsdaten-with-curves))
  (with-slots (gas gas-curve) steuerungsdaten-with-curves
    (floor (apply-curve gas-curve gas))))

(defmethod get-ruder ((steuerungsdaten-with-curves steuerungsdaten-with-curves))
  (with-slots (ruder ruder-curve) steuerungsdaten-with-curves
    (floor (apply-curve ruder-curve ruder))))

(defgeneric send (daten))


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
