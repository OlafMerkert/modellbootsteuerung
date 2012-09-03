(defpackage :datenprotokoll
  (:nicknames :datprot)
  (:import-from :com.gigamonkeys.binary-data
                :read-object
                :write-object)
  (:use :cl :ol
        :com.gigamonkeys.binary-data)
  (:export
   :close-serial
   :open-serial
   :axis-value
   :nullbyte
   :read-object
   :write-object
   :servo-resolution
   :setup-serial-port
   :servo-max
   :model
   :make-model
   :define-rc-model
   :axis-range
   :serial-io-path
   :register-model
   :register-binding
   :known-models))

(in-package :datenprotokoll)

(defconstant servo-resolution 14)

(defparameter servo-max (- (expt 2 13) 1))

(defparameter serial-io-path #P"/dev/ttyUSB0")

(defvar known-models (make-hash-table))

(defun register-model (model)
  (unless (nth-value 1 #1=(gethash model known-models))
    (setf #1# nil)))

(defun register-binding (model joystick)
  (pushnew joystick (gethash model known-models)))

(defconstant baudrate 115200)

(defun setup-serial-port (path)
  "Configure the serial port to above settings."
  (sb-ext:run-program
   "setserial"
   (list path
         "auto_irq"
         "skip_test"
         "autoconfig"))
  (sb-ext:run-program
   "stty"
   (list baudrate
         "cs8"
         "-parenb" "-crtscts"
         "-echo"
         "-F" path)))

(defun open-serial (path)
  "Return a read/write stream to the serial interface at PATH."
  ;; configure serial interface
  ;;  (setup-serial-port path)
  (open path :direction :io
        :element-type '(unsigned-byte 8)
        :if-exists :append
        :if-does-not-exist :error))

(defun close-serial (stream)
  "Close the stream to the serial interface."
  (close stream ))

 ;; Read a byte, where the first bit is always 1, and we examine just
;; the last BITLENGTH bits.  BITLENGTH must not exceed 7.
(define-binary-type axis-part (bitlength)
  (:reader (in)
           (let ((byte (read-byte in)))
             (unless (and (eql (ldb (byte 1 7) byte) 1))
               (error "axis-part byte must start with 1."))
             (ldb (byte bitlength 0) byte)))
  (:writer (out integer)
           (setf (ldb (byte 1 7) integer) 1)
           (write-byte integer out)))

(defun split-bitlength (bl)
  "compute the sizes best suited to partition a bitvector into two
parts."
   (let ((smaller-part (floor bl 2)))
    (values smaller-part (- bl smaller-part))))

;; Split a an unsigned integer of BITLENGTH into two bytes, by
;; splitting it evenly into the last BITLENGTH/2 bits of two bytes,
;; that each begin with 1.. For odd bitlength, the first byte will use
;; less bits than the second.
(define-binary-type axis-value (bitlength)
  (:reader (in)
           (multiple-value-bind (first-length second-length) (split-bitlength bitlength)
            (+ (* (expt 2 second-length)
                  (read-value 'axis-part in :bitlength first-length))
               (read-value 'axis-part in :bitlength second-length))))
  (:writer (out integer)
           (multiple-value-bind (first-length second-length) (split-bitlength bitlength)
  (multiple-value-bind (first second) (floor integer (expt 2 second-length))
    (write-value 'axis-part out first  :bitlength first-length)
    (write-value 'axis-part out second :bitlength second-length)))))

(define-binary-type nullbyte ()
  (:reader (in)
           (aprog1 (read-byte in)
             (unless (eql it 0)
               (error "Expected nullbyte, found data."))))
  (:writer (out value)
           (declare (ignore value))
           (write-byte 0 out)))

(defclass model ()
  ())

(defgeneric make-model (model &rest args))

(defgeneric axis-range (model axis))

(defmacro define-rc-model (name axes)
  (let* ((axes (mapcar (lambda (x) (if (consp x) x (list x))) axes))
         (axes-names (mapcar #'first axes)))
    `(progn
       (register-model ',name)
       (define-binary-class ,name (model)
         (,@(mapcar #`(, a1 (axis-value :bitlength servo-resolution)) axes-names)
            (term nullbyte)))
       ,@(mapcar
          (lambda (axis)
            (destructuring-bind (axis &key (min 0) (max servo-max) (reverse nil)) axis
              (when reverse
                (rotatef min max))
              `(defmethod axis-range ((,name (eql ',name)) (,axis (eql ',axis)))
                 `(:min ,,min :max ,,max))))
          axes)
       (defmethod make-model ((,name (eql ',name)) &rest args)
         (make-instance ',name
                        :term nil
                        ,@(mapcan (lambda (x) (list (keyw x)
                                               `(or (pop args) 0)))
                                  axes-names)))
       (defmethod print-object ((,name ,name) stream)
         (with-slots ,axes-names ,name
           (format stream
                   ,(format nil "~:(~A~)  ~{~:(~A~): ~~5D~^  ~}" name axes-names)
                   ,@axes-names))))))
