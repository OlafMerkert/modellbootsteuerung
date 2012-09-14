(defpackage :joystick-eingabe
  (:nicknames :jsin)
  (:use :cl :ol )
  (:export
   :list-joysticks
   :steuerung-main
   :js-min
   :js-max
   :find-joystick
   :process-js-axis
   :process-js-button
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

(defmethod swap-curve-bounds ((curve affine-curve))
  (with-slots (in-min in-max out-min out-max) curve
    (rotatef out-min out-max)
    (rotatef in-min in-max)))

(defmethod initialize-instance :after ((affine-curve affine-curve) &rest args)
  (declare (ignore args))
  (with-slots (scale in-min in-max out-min out-max) affine-curve
    ;; ensure out-min is smaller than out-max
    (cond ((> out-min out-max)
           (swap-curve-bounds affine-curve))
          ((= out-min out-max)
           (error "out-min and out-max have same value ~A - that is not acceptable." out-min)))
    ;; calculate and store scaling factor
    (setf scale
          (coerce (/ (- out-max out-min)
                     (- in-max in-min)) 'float))))

(defun ensure-between-bounds (min max number)
  "Ensure NUMBER is in the interval between MIN and MAX where (< MIN
MAX) holds."
  (cond ((< max number) max)
        ((< number min) min)
        (t number)))

(defmethod apply-curve ((curve affine-curve) number)
  (with-slots (scale in-min out-min out-max) curve
    (ensure-between-bounds out-min out-max
                           (+ (* scale (- number in-min)) out-min))))

(defclass trimmed-curve (affine-curve)
  ((trim-unit      :initarg :trim-unit
                   :initform 1
                   :accessor trim-unit)
   (trim-position :initarg :trim-position
                  :initform 0
                  :accessor trim-position))
  (:documentation "add a trimming functionality to the curve."))

(defmethod swap-curve-bounds :after ((curve trimmed-curve))
  (with-slots (trim-unit) curve
    (setf trim-unit (- trim-unit))))

(defmethod apply-curve ((curve trimmed-curve) number)
  (call-next-method curve (+ (trim-position curve) number)))

(defmethod adjust-trim ((curve trimmed-curve) (direction number))
  (with-slots (trim-unit trim-position) curve
   (cond ((minusp direction)
          (decf trim-position trim-unit))
         ((plusp direction)
          (incf trim-position trim-unit)))))

(defgeneric process-js-axis   (model joystick axis number))
(defgeneric process-js-button (model joystick button))
(defgeneric process-js-hat    (model joystick hat direction))

(defmethod process-js-axis    (model joystick (axis integer) (number number)))
(defmethod process-js-button  (model joystick (button integer)))
(defmethod process-js-hat     (model joystick (hat integer) direction))


(defmacro! define-joystick-binding (model joystick axis-bindings &optional button-bindings)
  `(progn
     ;; generate the axis-bindings
     ,@(mapcar
        (lambda (binding)
          (destructuring-bind (name &key axis (min js-min) (max js-max) (reverse nil) (trimming nil)) binding
            (when reverse
              (rotatef min max))
            `(let ((,g!curve
                    (make-instance 'affine-curve
                                   :in-min ,min :in-max ,max
                                   :out-min (getf (datprot:axis-range ',model ',name) :min)
                                   :out-max (getf (datprot:axis-range ',model ',name) :max))))
               (defmethod process-js-axis ((,g!model ,model)
                                            (,g!joystick (eql ',joystick))
                                            (,g!axis (eql ,axis))
                                            (,g!number number))
                 (setf (slot-value ,g!model ',name)
                       (floor (apply-curve ,g!curve ,g!number))))
               ;; TODO handle trimming of this axis
               )))
        axis-bindings)
     ;; generate the button and hat bindings (event could perhaps be
     ;; named (button x) and (hat x :north) or just (hat :north) for
     ;; the standard hat nr 1?)
     ;; TODO what facilities do we need here?
     ))

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
                 (dbug "Joystick name: ~A and id: ~A" joystick id)
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
           (process-js-axis model joystick axis value))
         (sleep #.(expt 10 -3))
         (sb-thread:thread-yield))
        (:joy-button-down-event
         (:which jsid :button button :state state)
         ;; state is either sdl-pressed or sdl-released
         (declare (ignorable state))
         (when (eql jsid id)
           (process-js-button model joystick button))
         (sleep #.(expt 10 -3))
         (sb-thread:thread-yield))
        (:joy-hat-motion-event
         (:which jsid :axis hat :value value)
         ;; TODO what does value look like??         
         (when (eql jsid id)
           (process-js-hat model joystick hat value))
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
