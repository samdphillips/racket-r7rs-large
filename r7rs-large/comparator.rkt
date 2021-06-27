#lang r7rs

(define-library (scheme comparators)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme char) (scheme complex) (scheme inexact))
  (import (only (racket base)
                arithmetic-shift bitwise-and bitwise-ior random sub1
                system-type))
  (export comparator? comparator-ordered? comparator-hashable?)
  (export make-comparator
          make-pair-comparator make-list-comparator make-vector-comparator
          make-eq-comparator make-eqv-comparator make-equal-comparator)
  (export boolean-hash char-hash char-ci-hash
          string-hash string-ci-hash symbol-hash number-hash)
  (export make-default-comparator default-hash comparator-register-default!)
  (export comparator-type-test-predicate comparator-equality-predicate
        comparator-ordering-predicate comparator-hash-function)
  (export comparator-test-type comparator-check-type comparator-hash)
  (export hash-bound hash-salt)
  (export =? <? >? <=? >=?)
  (export comparator-if<=>)
  (include "private/include/comparator/comparator-impl.scm")
  (include "private/include/comparator/default.scm"))

