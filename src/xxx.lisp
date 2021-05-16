(in-package :hypergeometrica)

(declaim (ftype (function ((unsigned-byte 64))
                          (mpz))
                x^x^x/ram))
(defun x^x^x/ram (x)
  (mpz-expt (integer-mpz x) (expt x x)))

(declaim (ftype (function ((unsigned-byte 64))
                          (mpz))
                x^x^x/disk))
(defun x^x^x/disk (x)
  (let ((*maximum-vector-size* 0))
    (mpz-expt (integer-mpz x) (expt x x))))
