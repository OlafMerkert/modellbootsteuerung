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
   :joystick-main-loop
   :affine-curve))

(in-package :joystick-eingabe)

(ew (defparameter js-min -32768)
    (defparameter js-max  32767))

(defun list-joysticks (&optional sdl-on)
  "list all the joysticks found on this system, with name."
  (if (not sdl-on)
      (sdl:with-init (sdl:sdl-init-joystick)
        #1=(loop for i from 0 below (sdl:num-joysticks)
              collect (sdl:sdl-joystick-name i)))
      #1#))

(defmacro in (one &rest others)
  "Generate a test clauses that yields t if ONE is eql to any of the
  OTHERS."
  `(or ,@(mapcar #`(eql ,one ,a1) others)))

(defun char-equal/relaxed (a b)
  "treat spaces and dashes as equal."
  (or (char-equal a b)
      (and (in a #\space #\-)
           (in b #\space #\-))))

(defun find-joystick (joystick &optional sdl-on)
  "search for a joystick id whose name contains CONTROLLER (which is
usually a symbol)."
  (position-if (lambda (name)
                 (search (mkstr joystick) name :test #'char-equal/relaxed))
               (list-joysticks sdl-on)))

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

(defgeneric process-js-input (model joystick axis number))

(defmethod process-js-input  (model joystick (axis integer) (number number)))

(defmacro! define-joystick-binding (model joystick bindings)
  `(progn
     ,@(mapcar
        (lambda (binding)
          (destructuring-bind (name &key axis (min js-min) (max js-max) (reverse nil)) binding
            (when reverse
              (rotatef min max))
            `(let ((,g!curve
                    (make-instance 'affine-curve
                                   :in-min ,min :in-max ,max
                                   :out-min (getf (datprot:axis-range ',model ',name) :min)
                                   :out-max (getf (datprot:axis-range ',model ',name) :max))))
               (defmethod process-js-input ((,g!model ,model)
                                            (,g!joystick (eql ',joystick))
                                            (,g!axis (eql ,axis))
                                            (,g!number number))
                 (setf (slot-value ,g!model ',name)
                       (floor (apply-curve ,g!curve ,g!number)))))))
        bindings)))

;; TODO allow using more than one stick at a time.

(defun joystick-main-loop (model joystick &optional quit-callback)
  "listen for joystick events for the JOYSTICKS identified by symbols
and send axis data from them to the model."
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-joystick)
    (sdl:window 400 100
                :title-caption "Hello Katja"
                :icon-caption  "Hello Katja")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font sdl:*font-8x8*)
    (sdl-cffi::sdl-joystick-event-state sdl-cffi::sdl-enable)
    (let* ((id (find-joystick joystick t))
           (js (progn
                 (dbug "Joystick id: ~A" id)
                 (sdl-cffi::sdl-joystick-open id))))
      (sdl:with-events ()
        (:quit-event ()
                     (when js
                       (sdl-cffi::sdl-joystick-close js))
                     (when quit-callback
                       (funcall quit-callback))
                     t)
        (:joy-axis-motion-event
         (:which jsid :axis axis :value value)
         (when (eql jsid id)
           (process-js-input model joystick axis value))
         (sleep #.(expt 10 -3))
         (sb-thread:thread-yield))
        (:idle ()
               (sdl:clear-display sdl:*black*)
               (sdl:draw-string-solid-* (format nil "Hello Katja")
                                        200 50
                                        :color sdl:*white*
                                        :justify :center)
               (sdl:update-display)
               (sleep #.(expt 10 -3))
               (sb-thread:thread-yield))
        (:video-expose-event ()
                             (sdl:update-display))))))
