;;;; moduli.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:hypergeometrica-tests)

;;; Tests start HERE!

(deftest test-scheme-is-sufficient ()
  (is (<= 3 (length (h::scheme-moduli h::**scheme**))))
  (is (<= 50 (h::scheme-max-transform-length h::**scheme**)))
  (is (<= (+ 64 64 50) (reduce #'+ (h::scheme-moduli h::**scheme**) :key #'h::lg))))
