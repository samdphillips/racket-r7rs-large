#lang r7rs

(import (scheme base)
        (scheme list)
        (r7rs-tests support))

(test (cons 'a '())        '(a))
(test (cons '(a) '(b c d)) '((a) b c d))
(test (cons "a" '(b c))    '("a" b c))
(test (cons 'a 3)          '(a . 3))
(test (cons '(a b) 'c)     '((a b) . c))

(test (list 'a (+ 3 4) 'c) '(a 7 c))
(test (list)               '())

(test (xcons '(b c) 'a) '(a b c))

(test (cons* 1 2 3 4) '(1 2 3 . 4))
(test (cons* 1) 1)

(test (make-list 4 'c) '(c c c c))

