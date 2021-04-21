;;;; mpf.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith

(in-package #:hypergeometrica)

(defstruct (mpf (:predicate mpf?)
                (:copier nil))
  "A representation of an arbitrary precision rational number. (F for Float, but it's not a float).

Equal to

    SIGN * STORAGE * $BASE ^ (EXPN - size(STORAGE))

where STORAGE is the mantissa between 0 and $BASE. "
  (sign 1                   :type sign)
  (expn 0                   :type fixnum)
  (storage (make-storage 0) :type storage :read-only t))

(defun mpf-digit (mpf index)
  (if (>= index (length (storage mpf)))
      0
      (aref (storage mpf) index)))

(declaim (ftype (function (mpf) alexandria:array-length) mpf-size))
(defun mpf-size (mpf)
  "How many digits does the mantissa of MPF have?"
  (declare (optimize speed (safety 0) (debug 0)))
  (loop :with raw := (raw-storage-of-storage (mpf-storage mpf))
        :for i :from (1- (length raw)) :downto 0
        :unless (zerop (aref raw i))
          :do (return (1+ i))
        :finally (return 0)))

(defun mpf-zerop (mpf)
  (every #'zerop (storage mpf)))

(defun mpf-zero! (mpf)
  "Overwrite MPF to be zero."
  (resize-storage (mpf-storage mpf) 0)
  (setf (mpf-expn mpf) $expn-zero
        (mpf-sign mpf) 1)
  nil)

(defun make-mpf-zero ()
  "Make an MPF value equal to zero."
  (make-mpf :sign 1
            :expn 0
            :storage (make-storage 0)))

(defun mpf-integralp (mpf)
  "Does the MPF represent an integral (i.e., integer) value?"
  (or (mpf-zerop mpf)
      (not (plusp (mpf-expn mpf)))
      (<= (mpf-size mpf) (mpf-expn mpf))))

(defun mpf-fractionalp (mpf)
  "Does the MPF represent a fractional (i.e., non-integer) value?"
  (not (mpf-integralp mpf)))

(defun mpf-plusp (mpf)
  (and (not (mpf-zerop mpf))
       (= 1 (mpf-sign mpf))))

(defun mpf-minusp (mpf)
  (and (not (mpf-zerop mpf))
       (= -1 (mpf-sign mpf))))

(defun copy-mpf (mpf)
  (make-mpf :sign (mpf-sign mpf)
            :expn (mpf-expn mpf)
            :storage (copy-storage (mpf-storage mpf))))

(defun mpf-rational (mpf)
  "Convert an MPF into a Lisp RATIONAL."
  (* (mpf-sign mpf)
     (digits-to-integer (mpf-storage mpf))
     (expt $base (- (mpf-expn mpf) (mpf-size mpf)))))

(defun mpf-negate! (mpf)
  (setf (mpf-sign mpf) (- (mpf-sign mpf)))
  nil)

(defun mpf-negate (mpf)
  (let ((mpf (copy-mpf mpf)))
    (mpf-negate! mpf)
    mpf))

(defmethod print-object ((mpf mpf) stream)
  (print-unreadable-object (mpf stream :type t :identity t)
    (format stream "~:[Frac~;Int~] (~D bit~:P)"
            (mpf-integralp mpf)
            (mpf-mantissa-bits mpf))))

(define-symbolic-enumeration comparison
  cmp/in                                ; INcomparable
  cmp/gt                                ; Greater Than
  cmp/lt                                ; Less Than
  cmp/eq)                               ; EQual to

#+ig
(defun mpf-absolute-compare (a b)
  "Compare the absolute values of A and B. (NaN == NaN is true here.)"
  (cond
    ((/= (mpf-expn a) (mpf-expn b))     ; Checks finiteness too.
     (if (< (mpf-expn a) (mpf-expn b))
         cmp/lt
         cmp/gt))
    (t
     (let ((len (max (mpf-size a) (mpf-size b))))
       (loop :for i :from (1- len) :downto 0 :do
         (let ((ai (mpf-digit a i))
               (bi (mpf-digit b i)))
           (when (/= ai bi)
             (return-from mpf-absolute-compare
               (if (< ai bi)
                   cmp/lt
                   cmp/gt)))))
       ;; If we made it here, they're equal.
       cmp/eq))))

#+ig
(defun mpf-compare (a b)
  (cond
    ((or (mpf-nan? a) (mpf-nan? b))
     cmp/in)
    ((/= (mpf-sign a) (mpf-sign b))
     (if (and (mpf-zerop a) (mpf-zerop b))
         cmp/eq
         (if (plusp (mpf-sign a))
             cmp/gt
             cmp/lt)))
    (t
     (ecase (mpf-absolute-compare a b)
       (cmp/lt (if (= -1 (mpf-sign a)) cmp/gt cmp/lt))
       (cmp/gt (if (= -1 (mpf-sign a)) cmp/lt cmp/gt))
       (cmp/eq cmp/eq)))))
