(defpackage :fernsteuerung
  (:use :cl :ol
        :datprot :jsin)
  (:export
   :steuerung))

(in-package :fernsteuerung)

(defun steuerung (model-type joystick)
  (let ((model (make-model model-type))
        (stop  nil)
        (io-stream (open-serial serial-io-path)))
    (unwind-protect
         (progn
           (sb-thread:make-thread
            (lambda () (joystick-main-loop model joystick
                                           (lambda () (setf stop t)))))
           (do () (stop)
             (format t "~A~%" model)
             ;; TODO what if the js loop writes to this model at the
             ;; same time?
             (write-object model io-stream)
             (finish-output io-stream)
             (sleep #.(expt 10 -1.5))
             (sb-thread:thread-yield)))
      (close-serial io-stream))))

(progn
  (define-rc-model boot
     ((gas   :min (/ servo-max 2)   :max servo-max)
      (ruder :min (* 1/4 servo-max) :max (* 3/4 servo-max)
             :reverse t)))

  ;; NOTE: changes to rc-model axis scaling take effect only when the
  ;; bindings are redefined.
  (define-joystick-binding boot fighterstick
    ((gas   :axis 2)
     (ruder :axis 0)))

  (define-joystick-binding boot xbox-controller
    ((gas   :axis 5)
     (ruder :axis 3))))


(let ((servo-max (- (expt 2 12) 1)))
  (define-rc-model flugzeug
     ((gas         :min (/ servo-max 2)   :max servo-max)
      (hoehenruder :min (* 1/4 servo-max) :max (* 3/4 servo-max))
      (seitenruder :min (* 1/4 servo-max) :max (* 3/4 servo-max))))
  
  (define-joystick-binding flugzeug hotas-x
    ((gas         :axis 2 :reverse t)
     (hoehenruder :axis 1)
     (seitenruder :axis 0)))

  (define-joystick-binding flugzeug fighterstick
    ((gas :axis 2)
     (hoehenruder :axis 1 :trimming 1/20)
     (seitenruder :axis 0 :trimming 1/20))
    ((hat :north)  (trim hoehenruder +1))
    ((hat :south)  (trim hoehenruder -1))
    ((hat 0 :east) (trim seitenruder +1))
    ((hat 0 :west) (trim seitenruder -1))))

;;; some test functions
(defun test-model/boot ()
  "Create test output for controlling the boat."
  (let* ((testfile #P "/tmp/steuerung.test")
         (stream (open-serial testfile))
         (model (make-model 'boot)))
    (unwind-protect
         (loop
            for i from 0 below 12
            do
              (setf (gas model) (expt 2 i)
                    (ruder model) (expt 2 i))
              (write-object model stream))
      (close-serial stream))))
