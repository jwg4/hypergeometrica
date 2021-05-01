;;;; suite.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

;;; Tests start HERE!

(deftest test-sundries ()
  ;; POWER-OF-TWO-P
  (is (not (h::power-of-two-p 0)))
  (is (h::power-of-two-p 1))
  (is (h::power-of-two-p 2))
  (is (not (h::power-of-two-p 3)))
  (is (h::power-of-two-p 4))
  (loop :for i :from 1 :to 25
        :do (is (h::power-of-two-p (expt 2 i)))
            (is (not (h::power-of-two-p (+ 3 (expt 2 i))))))
