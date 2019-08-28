;;;; math-utilities.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica)

;;; This file contains mathematical utilities.

(defun power-of-two-p (n)
  "Is N a power-of-two?"
  (and (plusp n)
       (zerop (logand n (1- n)))))

(defun next-power-of-two (n)
  "Find the minimum K such that N <= 2^K."
  (if (power-of-two-p n)
      (1- (integer-length n))
      (integer-length n)))
