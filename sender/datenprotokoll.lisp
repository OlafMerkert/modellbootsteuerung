(defpackage :datenprotokoll
  (:nicknames :datprot)
  (:use :cl :ol
        :com.gigamonkeys.binary-data)
  (:import-from :com.gigamonkeys.binary-data
                :read-object
                :write-object)
  (:export
   :close-serial
   :open-serial))

(in-package :datenprotokoll)

(defun open-serial (path)
  "Return a read/write stream to the serial interface at PATH."
  (open path :direction :io
        :element-type '(unsigned-byte 8)
        :if-exists :append
        :if-does-not-exist :error))

(defun close-serial (stream)
  "Close the stream to the serial interface."
  (close stream ))

 ;; Read a byte, where the first bit is always 1, the next two bits
 ;;  are INDEX and the other 5 bits represent an unsigned integer.
(define-binary-type axis-part (index )
  (:reader (in)
           (let ((byte (read-byte in)))
             (unless (and (eql (ldb (byte 1 7) byte) 1)
                          (eql (ldb (byte 2 5) byte) (or index 0)))
               (error "axis-part header invalid for index ~A" index))
             (ldb (byte 5 0) byte)))
  (:writer (out integer)
           (setf (ldb (byte 2 5) integer) (or index 0)
                 (ldb (byte 1 7) integer) 1)
           (write-byte integer out)))

;; Split a 10bit unsigned integer into two bytes. The first byte
;;   starts with 101, then follow the five higher bits of the integer,
;;   the second byte starts with 110, then follow the five lower bits
;;   of the integer.
(define-binary-type axis-value ()
  (:reader (in)
           (+ (* #.(expt 2 5)
                 (read-value 'axis-part in :index 1))
              (read-value 'axis-part in :index 2)))
  (:writer (out integer)
           (multiple-value-bind (first second) (floor integer #.(expt 2 5))
             (write-value 'axis-part out first  :index 1)
             (write-value 'axis-part out second :index 2))))

(define-binary-type nullbyte ()
  (:reader (in)
           (aprog1 (read-byte in)
             (unless (eql it 0)
               (error "Expected nullbyte, found data."))))
  (:writer (out value)
           (declare (ignore value))
           (write-byte 0 out)))

(define-binary-class boot-steuerung ()
  ((gas    axis-value)
   (ruder  axis-value)
   (term   nullbyte)))

(defun make-boot-steuerung (gas ruder)
  (make-instance 'boot-steuerung :term nil
                 :gas gas
                 :ruder ruder))

(defun test/boot-steuerung ()
  "Create test output for controlling the boat."
  (let* ((testfile #P "/tmp/bootsteuerung.test")
         (stream (open-serial testfile))
         (steuer (make-boot-steuerung 0 0)))
    (unwind-protect
         (loop
            for i from 0 below 10
            do
              (setf (gas steuer) (expt 2 i)
                    (ruder steuer) (expt 2 i))
              (write-object steuer stream))
      (close-serial stream))))
