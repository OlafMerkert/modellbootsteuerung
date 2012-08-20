(defpackage :joystick-eingabe
  (:nicknames :jsin)
  (:use :cl :ol )
  (:export
   :list-joysticks
   :steuerung-main
   :js-min
   :js-max
   :find-joystick
   :process-js-input
   :define-joystick-binding
   :joystick-main-loop))

(in-package :joystick-eingabe)

(defparameter js-min -32768)
(defparameter js-max  32767)

(defun list-joysticks ()
  "list all the joysticks found on this system, with name."
  (sdl:with-init (sdl:sdl-init-joystick)
    (loop for i from 0 below (sdl:num-joysticks)
       collect (sdl:sdl-joystick-name i))))

(defmacro in (one &rest others)
  "Generate a test clauses that yields t if ONE is eql to any of the
  OTHERS."
  `(or ,@(mapcar #`(eql ,one ,a1) others)))

(defun char-equal/relaxed (a b)
  "treat spaces and dashes as equal."
  (or (char-equal a b)
      (and (in a #\space #\-)
           (in b #\space #\-))))

(defun find-joystick (controller)
  "search for a joystick id whose name contains CONTROLLER (which is
usually a symbol)."
  (position-if (lambda (name)
                 (search (mkstr controller) name :test #'char-equal/relaxed))
               (list-joysticks)))


(defgeneric process-js-input (controller model axis number))

(defmethod process-js-input  (controller model (axis integer) (number number)))

(defmacro! define-joystick-binding (controller model bindings)
  `(progn
     ,@(mapcar
        (lambda (binding)
          (destructuring-bind (name &key axis (min js-min) (max js-max) (reverse nil)) binding
            (when reverse
              (rotatef min max))
            `(let ((,g!curve
                    (make-instance 'affine-curve
                                   :in-min js-min :in-max max
                                   :out-min (getf (datprot:axis-range ',model ',name) :min)
                                   :out-max (getf (datprot:axis-range ',model ',name) :max))))
               (defmethod process-js-input ((,g!controller (eql ',controller))
                                            (,g!model ',model)
                                            (,g!axis (eql ,axis))
                                            (,g!number number))
                 (setf (slot-value ,g!model ',name)
                       (floor (apply-curve ,g!curve ,g!number)))))))
        bindings)))

(define-joystick-binding fighterstick boot
  ((gas   :axis 2)
   (ruder :axis 0)))

(define-joystick-binding xbox-controller boot
  ((gas   :axis 5)
   (ruder :axis 3)))

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

;; TODO allow using more than one stick at a time.

(defun joystick-main-loop (joystick model &optional quit-callback)
  "listen for joystick events for the JOYSTICKS identified by symbols
and send axis data from them to the model."
  (sdl:with-init (sdl:sdl-init-joystick)
    (sdl:window 400 100
                :title-caption "Hello Katja"
                :icon-caption  "Hello Katja")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font sdl:*font-8x8*)
    (sdl-cffi::sdl-joystick-event-state sdl-cffi::sdl-enable)
    (let* ((id (find-joystick joystick))
           (js (sdl-cffi::sdl-joystick-open id)))
      (sdl:with-events ()
        (:quit-event ()
                     (when js
                       (sdl-cffi::sdl-joystick-close js))
                     (when quit-callback
                       (funcall quit-callback))
                     t)
        (:joy-axis-motion-event
         (:which joystick :axis axis :value value)
         (when (eql joystick id)
           (process-js-input joystick model axis value))
         (sleep #.(expt 10 -3))
         (sb-thread:thread-yield))
        (:idle ()
               (sdl:clear-display sdl:*black*)
               (sdl:draw-string-solid-* (format nil "Hello Katja")
                                        200 50
                                        :color sdl:*white*
                                        :justify :center
                                        :surface sdl:*default-display*)
               (sdl:update-display)
               (sleep #.(expt 10 -3))
               (sb-thread:thread-yield))
        (:video-expose-event ()
                             (sdl:update-display))))))
