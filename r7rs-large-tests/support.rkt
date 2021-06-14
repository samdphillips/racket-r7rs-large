#lang racket/base

(require rackunit)

(provide (rename-out
          [test-case test-group]
          [check-true test-assert]
          [void test-exit])
         test
         test-error)

(define-syntax-rule (test expected actual)
  (check-equal? actual expected))

(define-syntax-rule (test-error expr)
  (check-exn exn:fail? (lambda () expr)))

