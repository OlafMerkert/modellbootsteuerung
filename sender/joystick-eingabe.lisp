(defpackage :joystick-eingabe
  (:nicknames :joystick)
  (:use :cl :ol )
  (:export
   :list-joysticks
   :steuerung-main))

(in-package :joystick-eingabe)

(defparameter servo-max (- (expt 2 13) 1))

(defclass steuerung ()
  ((io-stream :initarg :io-stream
              :reader   io-stream)
   (gas   :initarg :gas
          :initform 0
          :accessor gas)
   (ruder :initarg :ruder
          :initform 0
          :accessor ruder))
  (:documentation "halte die Steuerungsdaten für das Boot vor."))

(defmethod get-gas ((steuerung steuerung))
  (floor (gas steuerung)))

(defmethod get-ruder ((steuerung steuerung))
  (floor (ruder steuerung)))

(defclass steuerung-with-curves (steuerung)
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

(defmethod get-gas ((steuerung-with-curves steuerung-with-curves))
  (with-slots (gas gas-curve) steuerung-with-curves
    (floor (apply-curve gas-curve gas))))

(defmethod get-ruder ((steuerung-with-curves steuerung-with-curves))
  (with-slots (ruder ruder-curve) steuerung-with-curves
    (floor (apply-curve ruder-curve ruder))))

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


(defun send (steuerung) 
  (format t "Gas:  ~A  Ruder:  ~A~%"
          (get-gas output-to-serial)
          (get-ruder output-to-serial))
  (datenprotokoll:write-object
   (datenprotokoll:make-boot-steuerung
    (get-gas   output-to-serial)
    (get-ruder output-to-serial))
   (io-stream output-to-serial))
  (finish-output (io-stream output-to-serial) )
  (sleep #.(/ 10 1000)))

(defun make-steuerung (serial-path &key
                                            (ruder-min 0)
                                            (ruder-max servo-max)
                                            (gas-min 0)
                                            (gas-max servo-max))
  (make-instance 'steuerung-with-curves
                 :ruder-curve (make-instance 'affine-curve
                                             :in-min -32768
                                             :in-max 32767
                                             :out-min ruder-min
                                             :out-max ruder-max)
                 :gas-curve (make-instance 'affine-curve
                                           :in-min -32768
                                           :in-max 32767
                                           :out-min gas-min
                                           :out-max gas-max)
                 :io-stream (datenprotokoll:open-serial serial-path)))

(defun joystick-main-loop (js-spec data &optional quit-callback)
  "listen for joystick events, put the axis data into DATA according
to JS-SPEC and call the SEND method on DATA from time to time."
  (destructuring-bind (&key id gas ruder) js-spec
   (sdl:with-init (sdl:sdl-init-joystick)
     (sdl:window 400 100
                 :title-caption "Hello Katja"
                 :icon-caption  "Hello Katja")
     (setf (sdl:frame-rate) 30)
     (sdl:initialise-default-font sdl:*font-8x8*)
     (sdl-cffi::sdl-joystick-event-state sdl-cffi::sdl-enable)
     (let ((js   (sdl-cffi::sdl-joystick-open id)))
       (sdl:with-events ()
         (:quit-event ()
                      (when js
                        (sdl-cffi::sdl-joystick-close js))
                      (when quit-callback
                        (funcall quit-callback))
                      t)
         (:joy-axis-motion-event
          (:which joystick :axis axis :value value)
          (cond ((and (eql joystick id)
                      (eql axis gas))
                 (setf (gas data) value))
                ((and (eql joystick id)
                      (eql axis ruder))
                 (setf (ruder data) value)))
          (sleep #.(/ 1 100))
          (sb-thread:thread-yield))
         (:idle ()
                (sdl:clear-display sdl:*black*)
                (sdl:draw-string-solid-* (format nil "Hello Katja")
                                         200 50
                                         :color sdl:*white*
                                         :justify :center
                                         :surface sdl:*default-display*)
                (sdl:update-display)
                (sleep #.(/ 1 100))
                (sb-thread:thread-yield))
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

(defparameter serial-io-path ;"/tmp/bootsteuerung.test";
  "/dev/ttyUSB0"
  )

(defun steuerung-main (spec)
  (let ((so (make-steuerung serial-io-path))
        (stop nil))
    (unwind-protect
         (progn
           (sb-thread:make-thread
            (lambda () (joystick-main-loop spec so
                                      (lambda () (setf stop t)))))
           (do () (stop)
             (send so)
             (sb-thread:thread-yield)))
      (datenprotokoll:close-serial (io-stream so)))))
