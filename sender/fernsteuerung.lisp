(defpackage :fernsteuerung
  (:use :cl :ol
        :datprot :jsin)
  (:export
   :steuerung))

(in-package :fernsteuerung)

(defun steuerung (joystick model-type)
  (let ((model (make-steuerung 'model-type))
        (stop  nil)
        (io-stream (open-serial serial-io-path)))
    (unwind-protect
         (progn
           (sb-thread:make-thread
            (lambda () (joystick-main-loop joystick model
                                           (lambda () (setf stop t)))))
           (do () (stop)
             (format t "~A~%" model)
             ;; TODO what if the js loop writes to this model at the
             ;; same time?
             (write-object model io-stream)
             (finish-output io-stream)
             (sleep #.(expt 10 -1.5))
             (sb-thread:thread-yield)))))
  )
