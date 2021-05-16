(in-package :hypergeometrica)

(defun x^x^x/ram (x)
  (mpz-expt (integer-mpz x) (expt x x)))

(defun x^x^x/disk (x)
  (let ((*maximum-vector-size* 0))
    (mpz-expt (integer-mpz x) (expt x x))))
