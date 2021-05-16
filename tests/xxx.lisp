(in-package #:hypergeometrica-tests)

(defun test-it (x)
    (let (ram disk)
      (setf ram (time (h::x^x^x/ram x)))
      (setf disk (time (h::x^x^x/disk x)))
      (h::mpz-= ram disk)))

(deftest test-nine-nine-nine ()
    (test-it 9)
)

(deftest test-eight-eight-eight ()
    (test-it 8)
)
