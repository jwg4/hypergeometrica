(in-package #:hypergeometrica-tests)

(defun test-it (x)
    (let (ram disk)
      (setf ram (time (x^x^x/ram x)))
      (setf disk (time (x^x^x/disk x)))
      (mpz-= ram disk)))

(deftest test-nine-nine-nine ()
    (test-it 9)
)

(deftest test-eight-eight-eight ()
    (test-it 8)
)
