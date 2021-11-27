#lang r7rs

(import (scheme base)
        (scheme list)
        (r7rs-tests support))

;;
;; Constructors
;;

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

(test (list-tabulate 4 values) '(0 1 2 3))

(let ((cl (circular-list 'z 'q)))
  (test (list-ref cl 0) 'z)
  (test (list-ref cl 1) 'q)
  (test (list-ref cl 2) 'z)
  (test (list-ref cl 3) 'q)
  (test-assert (circular-list? cl)))

(test (iota 5) '(0 1 2 3 4))
(test (iota 5 0 -1/10) '(0 -1/10 -2/10 -3/10 -4/10))

;;
;; Predicates
;;

(test (proper-list? '(a b c d)) #t)
(test (proper-list? '(a . b)) #f)
(test (proper-list? '(a b c . d)) #f)
(test (proper-list? (circular-list 'z 'q)) #f)

(test (circular-list? (circular-list 'z 'q)) #t)
(test (circular-list? '(a b c d)) #f)
(test (circular-list? '(a . b)) #f)

(test (dotted-list? '(a . b)) #t)
(test (dotted-list? '(a b c . d)) #t)
(test (dotted-list? '(a b c)) #f)
(test (dotted-list? (circular-list 'z 'q)) #f)

(test (pair? '(a . b)) #t)
(test (pair? '(a b c)) #t)
(test (pair? '())      #f)
(test (pair? '#(a b))  #f)
(test (pair? 7)        #f)
(test (pair? 'a)       #f)

(test (null-list? '()) #t)
(test (null-list? '(a)) #f)
(test (null-list? (circular-list 'a)) #f)
(test-error (null-list? 42))
;; XXX: I believe this is the correct behavior, but Gauche, Chibi, and the
;; reference implementation return `#f`.
; (test-error (null-list? '(a . b)))

(test (not-pair? '(a . b)) #f)
(test (not-pair? '(a b c)) #f)
(test (not-pair? '())      #t)
(test (not-pair? '#(a b))  #t)
(test (not-pair? 7)        #t)
(test (not-pair? 'a)       #t)

(test-assert (list= eq?))
(test-assert (list= eq? '(a)))
(test-assert (list= = '(1 2 3) '(1 2 3)))
(test-assert (list= = '(1 2 3) '(1 2 3) '(1 2 3)))
(test (list= = '(1 2 3) '(1 3 3) '(1 2 3)) #f)
(test-assert (list= eq? '(a b c) '(a b c)))

;;
;; Selectors
;;

(let ((xs '(0 1 2 3 4 5 6 7 8 9)))
  (test (first xs)      0)
  (test (second xs)     1)
  (test (third xs)      2)
  (test (fourth xs)     3)
  (test (fifth xs)      4)
  (test (sixth xs)      5)
  (test (seventh xs)    6)
  (test (eighth xs)     7)
  (test (ninth xs)      8)
  (test (tenth xs)      9))
(let ()
  (call-with-values
    (lambda () (car+cdr '(a b)))
    (lambda (x y)
      (test x 'a)
      (test y '(b)))))

(test (take '(a b c d e)  2) '(a b))
(test (drop '(a b c d e)  2) '(c d e))
(test (take '(1 2 3 . d) 2)  '(1 2))
(test (drop '(1 2 3 . d) 2)  '(3 . d))
(test (take '(1 2 3 . d) 3)  '(1 2 3))
(test (drop '(1 2 3 . d) 3)  'd)

(test (take-right '(a b c d e) 2) '(d e))
(test (drop-right '(a b c d e) 2) '(a b c))
(test (take-right '(1 2 3 . d) 2) '(2 3 . d))
(test (drop-right '(1 2 3 . d) 2) '(1))
(test (take-right '(1 2 3 . d) 0) 'd)
(test (drop-right '(1 2 3 . d) 0) '(1 2 3))

(test (take! '(a b c d e) 2) '(a b))
(test (take! '(1 2 3 . d) 2) '(1 2))
(test (take! '(1 2 3 . d) 3) '(1 2 3))

(test (drop-right! '(a b c d e) 2) '(a b c))
(test (drop-right! '(1 2 3 . d) 2) '(1))
(test (drop-right! '(1 2 3 . d) 0) '(1 2 3))

(test-group "split-at"
  (call-with-values
    (lambda ()
      (split-at '(a b c d e f g h) 3))
    (lambda (x y)
      (test x '(a b c))
      (test y '(d e f g h)))))

(test-group "split-at!"
  (call-with-values
    (lambda ()
      (split-at! '(a b c d e f g h) 3))
    (lambda (x y)
      (test x '(a b c))
      (test y '(d e f g h)))))

(test (last '(a b c)) 'c)
(test (last-pair '(a b c)) '(c))

