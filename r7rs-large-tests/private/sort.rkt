#lang r7rs
;;; Test program for SRFI 132 (Sort Libraries).

;;; Copyright © William D Clinger (2016).
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; This code is
;;;     Copyright (c) 1998 by Olin Shivers.
;;; The terms are: You may do as you please with this code, as long as
;;; you do not delete this notice or hold me responsible for any outcome
;;; related to its use.
;;;
;;; Blah blah blah. Don't you think source files should contain more lines
;;; of code than copyright notice?

(import (scheme base)
        (scheme sort)
        (scheme write)
        (only (srfi 27) random-integer)
        (only (rackunit) check-true check-equal?)
        )

(define (random-vector size)
  (let ((v (make-vector size)))
    (fill-vector-randomly! v (* 10 size))
    v))

(define (fill-vector-randomly! v range)
  (let ((half (quotient range 2)))
    (do ((i (- (vector-length v) 1) (- i 1)))
      ((< i 0))
      (vector-set! v i (- (random-integer range) half)))))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(check-true (list-sorted? > '()) "list-sorted?:empty-list")

(check-true (list-sorted? > '(987)) "list-sorted?:singleton")

(check-true (list-sorted? > '(9 8 7)) "list-sorted?:non-empty-list")

(check-true (vector-sorted? > '#()) "vector-sorted?:empty-vector")

(check-true (vector-sorted? > '#(987)) "vector-sorted?:singleton")

(check-true
 (vector-sorted? > '#(9 8 7 6 5))
 "vector-sorted?:non-empty-vector")

(check-true (vector-sorted? > '#() 0) "vector-sorted?:empty-vector:0")

(check-true (vector-sorted? > '#(987) 1) "vector-sorted?:singleton:")


(check-true
 (vector-sorted? > '#(9 8 7 6 5) 1)
 "vector-sorted?:non-empty-vector:1")

(check-true (vector-sorted? > '#() 0 0) "vector-sorted?:empty-vector:0:0")

(check-true (vector-sorted? > '#(987) 1 1) "vector-sorted?:singleton:1:1")

(check-true
 (vector-sorted? > '#(9 8 7 6 5) 1 2)
 "vector-sorted?:non-empty-vector:1:2")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (list-sort > (list)) '() "list-sort:empty-list")

(check-equal? (list-sort > (list 987)) '(987) "list-sort:singleton")

(check-equal? (list-sort > (list 987 654)) '(987 654) "list-sort:doubleton")

(check-equal?
 (list-sort > (list 9 8 6 3 0 4 2 5 7 1))
 '(9 8 7 6 5 4 3 2 1 0)
 "list-sort:iota10")

(check-equal? (list-stable-sort > (list)) '() "list-stable-sort:empty-list")

(check-equal?
 (list-stable-sort > (list 987))
 '(987)
 "list-stable-sort:singleton")

(check-equal?
 (list-stable-sort > (list 987 654))
 '(987 654)
 "list-stable-sort:doubleton")

(check-equal?
 (list-stable-sort > (list 9 8 6 3 0 4 2 5 7 1))
 '(9 8 7 6 5 4 3 2 1 0)
 "list-stable-sort:iota10")

(check-equal?
 (list-stable-sort
  (lambda (x y) (> (quotient x 2) (quotient y 2)))
  (list 9 8 6 3 0 4 2 5 7 1))
 '(9 8 6 7 4 5 3 2 0 1)
 "list-stable-sort:iota10-quotient2")

(check-equal?
 (let ((v (vector))) (vector-sort > v))
 '#()
 "vector-sort:empty-vector")

(check-equal?
 (let ((v (vector 987))) (vector-sort > (vector 987)))
 '#(987)
 "vector-sort:singleton")

(check-equal?
 (let ((v (vector 987 654))) (vector-sort > v))
 '#(987 654)
 "vector-sort:doubleton")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort > v))
 '#(9 8 7 6 5 4 3 2 1 0)
 "vector-sort:iota10")

(check-equal?
 (let ((v (vector))) (vector-stable-sort > v))
 '#()
 "vector-stable-sort:empty-vector")

(check-equal?
 (let ((v (vector 987))) (vector-stable-sort > (vector 987)))
 '#(987)
 "vector-stable-sort:singleton")

(check-equal?
 (let ((v (vector 987 654))) (vector-stable-sort > v))
 '#(987 654)
 "vector-stable-sort:doubleton")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort > v))
 '#(9 8 7 6 5 4 3 2 1 0)
 "vector-stable-sort:iota10")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
   (vector-stable-sort (lambda (x y) (> (quotient x 2) (quotient y 2))) v))
 '#(9 8 6 7 4 5 3 2 0 1)
 "vector-stable-sort:iota10-quotient2")

(check-equal?
 (let ((v (vector))) (vector-sort > v 0))
 '#()
 "vector-sort:empty-vector:0")

(check-equal?
 (let ((v (vector 987))) (vector-sort > (vector 987) 1))
 '#()
 "vector-sort:singleton:1")

(check-equal?
 (let ((v (vector 987 654))) (vector-sort > v 1))
 '#(654)
 "vector-sort:doubleton:1")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort > v 3))
 '#(7 5 4 3 2 1 0)
 "vector-sort:iota10:3")

(check-equal?
 (let ((v (vector))) (vector-stable-sort > v 0))
 '#()
 "vector-stable-sort:empty-vector:0")

(check-equal?
 (let ((v (vector 987))) (vector-stable-sort > (vector 987) 1))
 '#()
 "vector-stable-sort:singleton:1")

(check-equal?
 (let ((v (vector 987 654))) (vector-stable-sort < v 0 2))
 '#(654 987)
 "vector-stable-sort:doubleton:0:2")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort > v 3))
 '#(7 5 4 3 2 1 0)
 "vector-stable-sort:iota10:3")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
   (vector-stable-sort (lambda (x y) (> (quotient x 2) (quotient y 2))) v 3))
 '#(7 4 5 3 2 0 1)
 "vector-stable-sort:iota10-quotient2:3")

(check-equal?
 (let ((v (vector))) (vector-sort > v 0 0))
 '#()
 "vector-sort:empty-vector:0:0")
(check-equal?
 (let ((v (vector 987))) (vector-sort > (vector 987) 1 1))
 '#()
 "vector-sort:singleton:1:1")
(check-equal?
 (let ((v (vector 987 654))) (vector-sort > v 1 2))
 '#(654)
 "vector-sort:doubleton:1:2")
(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort > v 4 8))
 '#(5 4 2 0)
 "vector-sort:iota10:4:8")
(check-equal?
 (let ((v (vector))) (vector-stable-sort > v 0 0))
 '#()
 "vector-stable-sort:empty-vector:0:0")
(check-equal?
 (let ((v (vector 987))) (vector-stable-sort > (vector 987) 1 1))
 '#()
 "vector-stable-sort:singleton:1:1")
(check-equal?
 (let ((v (vector 987 654))) (vector-stable-sort > v 1 2))
 '#(654)
 "vector-stable-sort:doubleton:1:2")
(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort > v 2 6))
 '#(6 4 3 0)
 "vector-stable-sort:iota10:2:6")
(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
   (vector-stable-sort (lambda (x y) (> (quotient x 2) (quotient y 2))) v 1 8))
 '#(8 6 4 5 3 2 0)
 "vector-stable-sort:iota10-quotient2:1:8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (list-sort! > (list)) '() "list-sort!:empty-list")

(check-equal? (list-sort! > (list 987)) '(987) "list-sort!:singleton")

(check-equal? (list-sort! > (list 987 654)) '(987 654) "list-sort!:doubleton")

(check-equal?
 (list-sort! > (list 9 8 6 3 0 4 2 5 7 1))
 '(9 8 7 6 5 4 3 2 1 0)
 "list-sort!:iota10")

(check-equal? (list-stable-sort! > (list)) '() "list-stable-sort!:empty-list")

(check-equal?
 (list-stable-sort! > (list 987))
 '(987)
 "list-stable-sort!:singleton")

(check-equal?
 (list-stable-sort! > (list 987 654))
 '(987 654)
 "list-stable-sort!:doubleton")

(check-equal?
 (list-stable-sort! > (list 9 8 6 3 0 4 2 5 7 1))
 '(9 8 7 6 5 4 3 2 1 0)
 "list-stable-sort!:iota10")

(check-equal?
 (list-stable-sort!
  (lambda (x y) (> (quotient x 2) (quotient y 2)))
  (list 9 8 6 3 0 4 2 5 7 1))
 '(9 8 6 7 4 5 3 2 0 1)
 "list-stable-sort!:iota10-quotient2")

(check-equal?
 (let ((v (vector))) (vector-sort! > v) v)
 '#()
 "vector-sort!:empty-vector")

(check-equal?
 (let ((v (vector 987))) (vector-sort! > (vector 987)) v)
 '#(987)
 "vector-sort!:singleton")

(check-equal?
 (let ((v (vector 987 654))) (vector-sort! > v) v)
 '#(987 654)
 "vector-sort!:doubleton")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort! > v) v)
 '#(9 8 7 6 5 4 3 2 1 0)
 "vector-sort!:iota10")

(check-equal?
 (let ((v (vector))) (vector-stable-sort! > v) v)
 '#()
 "vector-stable-sort!:empty-vector")

(check-equal?
 (let ((v (vector 987))) (vector-stable-sort! > (vector 987)) v)
 '#(987)
 "vector-stable-sort!:singleton")

(check-equal?
 (let ((v (vector 987 654))) (vector-stable-sort! > v) v)
 '#(987 654)
 "vector-stable-sort!:doubleton")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort! > v) v)
 '#(9 8 7 6 5 4 3 2 1 0)
 "vector-stable-sort!:iota10")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
   (vector-stable-sort! (lambda (x y) (> (quotient x 2) (quotient y 2))) v)
   v)
 '#(9 8 6 7 4 5 3 2 0 1)
 "vector-stable-sort!:iota10-quotient2")

(check-equal?
 (let ((v (vector))) (vector-sort! > v 0) v)
 '#()
 "vector-sort!:empty-vector:0")

(check-equal?
 (let ((v (vector 987))) (vector-sort! > (vector 987) 1) v)
 '#(987)
 "vector-sort!:singleton:1")

(check-equal?
 (let ((v (vector 987 654))) (vector-sort! > v 1) v)
 '#(987 654)
 "vector-sort!:doubleton:1")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort! > v 3) v)
 '#(9 8 6 7 5 4 3 2 1 0)
 "vector-sort!:iota10:3")

(check-equal?
 (let ((v (vector))) (vector-stable-sort! > v 0) v)
 '#()
 "vector-stable-sort!:empty-vector:0")

(check-equal?
 (let ((v (vector 987))) (vector-stable-sort! > (vector 987) 1) v)
 '#(987)
 "vector-stable-sort!:singleton:1")

(check-equal?
 (let ((v (vector 987 654))) (vector-stable-sort! < v 0 2) v)
 '#(654 987)
 "vector-stable-sort!:doubleton:0:2")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort! > v 3) v)
 '#(9 8 6 7 5 4 3 2 1 0)
 "vector-stable-sort!:iota10:3")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
   (vector-stable-sort! (lambda (x y) (> (quotient x 2) (quotient y 2))) v 3)
   v)
 '#(9 8 6 7 4 5 3 2 0 1)
 "vector-stable-sort!:iota10-quotient2:3")

(check-equal?
 (let ((v (vector))) (vector-sort! > v 0 0) v)
 '#()
 "vector-sort!:empty-vector:0:0")

(check-equal?
 (let ((v (vector 987))) (vector-sort! > (vector 987) 1 1) v)
 '#(987)
 "vector-sort!:singleton:1:1")

(check-equal?
 (let ((v (vector 987 654))) (vector-sort! > v 1 2) v)
 '#(987 654)
 "vector-sort!:doubleton:1:2")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-sort! > v 4 8) v)
 '#(9 8 6 3 5 4 2 0 7 1)
 "vector-sort!:iota10:4:8")

(check-equal?
 (let ((v (vector))) (vector-stable-sort! > v 0 0) v)
 '#()
 "vector-stable-sort!:empty-vector:0:0")

(check-equal?
 (let ((v (vector 987))) (vector-stable-sort! > (vector 987) 1 1) v)
 '#(987)
 "vector-stable-sort!:singleton:1:1")

(check-equal?
 (let ((v (vector 987 654))) (vector-stable-sort! > v 1 2) v)
 '#(987 654)
 "vector-stable-sort!:doubleton:1:2")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1))) (vector-stable-sort! > v 2 6) v)
 '#(9 8 6 4 3 0 2 5 7 1)
 "vector-stable-sort!:iota10:2:6")

(check-equal?
 (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
   (vector-stable-sort! (lambda (x y) (> (quotient x 2) (quotient y 2))) v 1 8)
   v)
 '#(9 8 6 4 5 3 2 0 7 1)
 "vector-stable-sort!:iota10-quotient2:1:8")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (list-merge > (list) (list)) '() "list-merge:empty:empty")

(check-equal?
 (list-merge > (list) (list 9 6 3 0))
 '(9 6 3 0)
 "list-merge:empty:nonempty")

(check-equal?
 (list-merge > (list 9 7 5 3 1) (list))
 '(9 7 5 3 1)
 "list-merge:nonempty:empty")

(check-equal?
 (list-merge > (list 9 7 5 3 1) (list 9 6 3 0))
 '(9 9 7 6 5 3 3 1 0)
 "list-merge:nonempty:nonempty")

(check-equal? (list-merge! > (list) (list)) '() "list-merge!:empty:empty")

(check-equal?
 (list-merge! > (list) (list 9 6 3 0))
 '(9 6 3 0)
 "list-merge!:empty:nonempty")

(check-equal?
 (list-merge! > (list 9 7 5 3 1) (list))
 '(9 7 5 3 1)
 "list-merge!:nonempty:empty")

(check-equal?
 (list-merge! > (list 9 7 5 3 1) (list 9 6 3 0))
 '(9 9 7 6 5 3 3 1 0)
 "list-merge!:nonempty:nonempty")

(check-equal?
 (vector-merge > (vector) (vector))
 '#()
 "vector-merge:empty:empty")

(check-equal?
 (vector-merge > (vector) (vector 9 6 3 0))
 '#(9 6 3 0)
 "vector-merge:empty:nonempty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector))
 '#(9 7 5 3 1)
 "vector-merge:nonempty:empty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0))
 '#(9 9 7 6 5 3 3 1 0)
 "vector-merge:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector)) v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0))
   v)
 '#(9 6 3 0 #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector))
   v)
 '#(9 7 5 3 1 #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0))
   v)
 '#(9 9 7 6 5 3 3 1 0 #f #f #f)
 "vector-merge!:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector) 0) v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:0")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 0)
   v)
 '#(9 6 3 0 #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:0")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 0)
   v)
 '#(9 7 5 3 1 #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty:0")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 0)
   v)
 '#(9 9 7 6 5 3 3 1 0 #f #f #f)
 "vector-merge!:nonempty:nonempty:0")

(check-equal?
 (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector) 2) v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 2)
   v)
 '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 2)
   v)
 '#(#f #f 9 7 5 3 1 #f #f #f #f #f)
 "vector-merge!:nonempty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
   v)
 '#(#f #f 9 9 7 6 5 3 3 1 0 #f)
 "vector-merge!:nonempty:nonempty:2")

(check-equal?
 (vector-merge > (vector) (vector) 0)
 '#()
 "vector-merge:empty:empty")

(check-equal?
 (vector-merge > (vector) (vector 9 6 3 0) 0)
 '#(9 6 3 0)
 "vector-merge:empty:nonempty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector) 2)
 '#(5 3 1)
 "vector-merge:nonempty:empty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
 '#(9 6 5 3 3 1 0)
 "vector-merge:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector) 2 0) v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 2 0)
   v)
 '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2)
   v)
 '#(#f #f 5 3 1 #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2)
   v)
 '#(#f #f 9 6 5 3 3 1 0 #f #f #f)
 "vector-merge!:nonempty:nonempty:2")

(check-equal?
 (vector-merge > (vector) (vector) 0 0)
 '#()
 "vector-merge:empty:empty")

(check-equal?
 (vector-merge > (vector) (vector 9 6 3 0) 0 0)
 '#(9 6 3 0)
 "vector-merge:empty:nonempty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector) 2 5)
 '#(5 3 1)
 "vector-merge:nonempty:empty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 5)
 '#(9 6 5 3 3 1 0)
 "vector-merge:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector) 2 0 0) v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
   v)
 '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 5)
   v)
 '#(#f #f 5 3 1 #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 5)
   v)
 '#(#f #f 9 6 5 3 3 1 0 #f #f #f)
 "vector-merge!:nonempty:nonempty:2")

(check-equal?
 (vector-merge > (vector) (vector) 0 0)
 '#()
 "vector-merge:empty:empty")

(check-equal?
 (vector-merge > (vector) (vector 9 6 3 0) 0 0)
 '#(9 6 3 0)
 "vector-merge:empty:nonempty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector) 2 4)
 '#(5 3)
 "vector-merge:nonempty:empty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4)
 '#(9 6 5 3 3 0)
 "vector-merge:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f))) (vector-merge! > v (vector) (vector) 2 0 0) v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
   v)
 '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4)
   v)
 '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4)
   v)
 '#(#f #f 9 6 5 3 3 0 #f #f #f #f)
 "vector-merge!:nonempty:nonempty:2")

(check-equal?
 (vector-merge > (vector) (vector) 0 0 0)
 '#()
 "vector-merge:empty:empty")

(check-equal?
 (vector-merge > (vector) (vector 9 6 3 0) 0 0 0)
 '#(9 6 3 0)
 "vector-merge:empty:nonempty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0)
 '#(5 3)
 "vector-merge:nonempty:empty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 0)
 '#(9 6 5 3 3 0)
 "vector-merge:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector) 2 0 0 0)
   v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 0)
   v)
 '#(#f #f 9 6 3 0 #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
   v)
 '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 0)
   v)
 '#(#f #f 9 6 5 3 3 0 #f #f #f #f)
 "vector-merge!:nonempty:nonempty:2")

(check-equal?
 (vector-merge > (vector) (vector) 0 0 0)
 '#()
 "vector-merge:empty:empty")

(check-equal?
 (vector-merge > (vector) (vector 9 6 3 0) 0 0 1)
 '#(6 3 0)
 "vector-merge:empty:nonempty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0)
 '#(5 3)
 "vector-merge:nonempty:empty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1)
 '#(6 5 3 3 0)
 "vector-merge:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector) 2 0 0 0)
   v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1)
   v)
 '#(#f #f 6 3 0 #f #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
   v)
 '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1)
   v)
 '#(#f #f 6 5 3 3 0 #f #f #f #f #f)
 "vector-merge!:nonempty:nonempty:2")

(check-equal?
 (vector-merge > (vector) (vector) 0 0 0 0)
 '#()
 "vector-merge:empty:empty")

(check-equal?
 (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 4)
 '#(6 3 0)
 "vector-merge:empty:nonempty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0)
 '#(5 3)
 "vector-merge:nonempty:empty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 4)
 '#(6 5 3 3 0)
 "vector-merge:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector) 2 0 0 0 0)
   v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 4)
   v)
 '#(#f #f 6 3 0 #f #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
   v)
 '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 4)
   v)
 '#(#f #f 6 5 3 3 0 #f #f #f #f #f)
 "vector-merge!:nonempty:nonempty:2")

(check-equal?
 (vector-merge > (vector) (vector) 0 0 0 0)
 '#()
 "vector-merge:empty:empty")

(check-equal?
 (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 2)
 '#(6)
 "vector-merge:empty:nonempty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0)
 '#(5 3)
 "vector-merge:nonempty:empty")

(check-equal?
 (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 2)
 '#(6 5 3)
 "vector-merge:nonempty:nonempty")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector) 2 0 0 0 0)
   v)
 '#(#f #f #f #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 2)
   v)
 '#(#f #f 6 #f #f #f #f #f #f #f #f #f)
 "vector-merge!:empty:nonempty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
   v)
 '#(#f #f 5 3 #f #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:empty:2")

(check-equal?
 (let ((v (make-vector 12 #f)))
   (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 2)
   v)
 '#(#f #f 6 5 3 #f #f #f #f #f #f #f)
 "vector-merge!:nonempty:nonempty:2")

(check-equal?
 (list-delete-neighbor-dups char=? (list))
 '()
 "list-delete-neighbor-dups:empty")

(check-equal?
 (list-delete-neighbor-dups char=? (list #\a))
 '(#\a)
 "list-delete-neighbor-dups:singleton")

(check-equal?
 (list-delete-neighbor-dups char=? (list #\a #\a #\a #\b #\b #\a))
 '(#\a #\b #\a)
 "list-delete-neighbor-dups:nonempty")

(check-equal?
 (list-delete-neighbor-dups! char=? (list))
 '()
 "list-delete-neighbor-dups!:empty")

(check-equal?
 (list-delete-neighbor-dups! char=? (list #\a))
 '(#\a)
 "list-delete-neighbor-dups!:singleton")

(check-equal?
 (list-delete-neighbor-dups! char=? (list #\a #\a #\a #\b #\b #\a))
 '(#\a #\b #\a)
 "list-delete-neighbor-dups!:nonempty")

(check-equal?
 (let ((v (vector))) (vector-delete-neighbor-dups char=? v))
 '#()
 "vector-delete-neighbor-dups:empty")

(check-equal?
 (let ((v (vector #\a))) (vector-delete-neighbor-dups char=? v))
 '#(#\a)
 "vector-delete-neighbor-dups:singleton")

(check-equal?
 (let ((v (vector #\a #\a #\a #\b #\b #\a)))
   (vector-delete-neighbor-dups char=? v))
 '#(#\a #\b #\a)
 "vector-delete-neighbor-dups:nonempty")

(check-equal?
 (let ((v (vector))) (list (vector-delete-neighbor-dups! char=? v) v))
 '(0 #())
 "vector-delete-neighbor-dups!:empty")

(check-equal?
 (let ((v (vector #\a))) (list (vector-delete-neighbor-dups! char=? v) v))
 '(1 #(#\a))
 "vector-delete-neighbor-dups!:singleton")

(check-equal?
 (let ((v (vector #\a #\a #\a #\b #\b #\a)))
   (list (vector-delete-neighbor-dups! char=? v) v))
 '(3 #(#\a #\b #\a #\b #\b #\a))
 "vector-delete-neighbor-dups!:nonempty")

(check-equal?
 (let ((v (vector))) (vector-delete-neighbor-dups char=? v 0))
 '#()
 "vector-delete-neighbor-dups:empty:0")

(check-equal?
 (let ((v (vector #\a))) (vector-delete-neighbor-dups char=? v 0))
 '#(#\a)
 "vector-delete-neighbor-dups:singleton:0")

(check-equal?
 (let ((v (vector #\a #\a #\a #\b #\b #\a)))
   (vector-delete-neighbor-dups char=? v 0))
 '#(#\a #\b #\a)
 "vector-delete-neighbor-dups:nonempty:0")

(check-equal?
 (let ((v (vector))) (list (vector-delete-neighbor-dups! char=? v 0) v))
 '(0 #())
 "vector-delete-neighbor-dups!:empty:0")

(check-equal?
 (let ((v (vector #\a))) (list (vector-delete-neighbor-dups! char=? v 0) v))
 '(1 #(#\a))
 "vector-delete-neighbor-dups!:singleton:0")

(check-equal?
 (let ((v (vector #\a #\a #\a #\b #\b #\a)))
   (list (vector-delete-neighbor-dups! char=? v 0) v))
 '(3 #(#\a #\b #\a #\b #\b #\a))
 "vector-delete-neighbor-dups!:nonempty:0")

(check-equal?
 (let ((v (vector))) (vector-delete-neighbor-dups char=? v 0))
 '#()
 "vector-delete-neighbor-dups:empty:0")

(check-equal?
 (let ((v (vector #\a))) (vector-delete-neighbor-dups char=? v 1))
 '#()
 "vector-delete-neighbor-dups:singleton:1")

(check-equal?
 (let ((v (vector #\a #\a #\a #\b #\b #\a)))
   (vector-delete-neighbor-dups char=? v 3))
 '#(#\b #\a)
 "vector-delete-neighbor-dups:nonempty:3")

(check-equal?
 (let ((v (vector))) (list (vector-delete-neighbor-dups! char=? v 0) v))
 '(0 #())
 "vector-delete-neighbor-dups!:empty:0")

(check-equal?
 (let ((v (vector #\a))) (list (vector-delete-neighbor-dups! char=? v 1) v))
 '(1 #(#\a))
 "vector-delete-neighbor-dups!:singleton:1")

(check-equal?
 (let ((v (vector #\a #\a #\a #\b #\b #\a)))
   (list (vector-delete-neighbor-dups! char=? v 3) v))
 '(5 #(#\a #\a #\a #\b #\a #\a))
 "vector-delete-neighbor-dups!:nonempty:3")

(check-equal?
 (let ((v (vector))) (vector-delete-neighbor-dups char=? v 0 0))
 '#()
 "vector-delete-neighbor-dups:empty:0:0")

(check-equal?
 (let ((v (vector #\a))) (vector-delete-neighbor-dups char=? v 1 1))
 '#()
 "vector-delete-neighbor-dups:singleton:1:1")

(check-equal?
 (let ((v (vector #\a #\a #\a #\b #\b #\a)))
   (vector-delete-neighbor-dups char=? v 3 5))
 '#(#\b)
 "vector-delete-neighbor-dups:nonempty:3:5")

(check-equal?
 (let ((v (vector))) (list (vector-delete-neighbor-dups! char=? v 0 0) v))
 '(0 #())
 "vector-delete-neighbor-dups!:empty:0:0")

(check-equal?
 (let ((v (vector #\a))) (list (vector-delete-neighbor-dups! char=? v 0 1) v))
 '(1 #(#\a))
 "vector-delete-neighbor-dups!:singleton:0:1")

(check-equal?
 (let ((v (vector #\a))) (list (vector-delete-neighbor-dups! char=? v 1 1) v))
 '(1 #(#\a))
 "vector-delete-neighbor-dups!:singleton:1:1")

(check-equal?
 (let ((v (vector #\a #\a #\a #\b #\b #\a)))
   (list (vector-delete-neighbor-dups! char=? v 3 5) v))
 '(4 #(#\a #\a #\a #\b #\b #\a))
 "vector-delete-neighbor-dups!:nonempty:3:5")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (vector-find-median < (vector) "knil")
 "knil"
 "vector-find-median:empty")

(check-equal?
 (vector-find-median < (vector 17) "knil")
 17
 "vector-find-median:singleton")

(check-equal?
 (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil")
 12
 "vector-find-median:8same")

(check-equal?
 (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil")
 23/2
 "vector-find-median:8diff")

(check-equal?
 (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil" list)
 (list 12 12)
 "vector-find-median:8samelist")

(check-equal?
 (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil" list)
 (list 11 12)
 "vector-find-median:8difflist")

(check-equal?
 (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil")
 7
 "vector-find-median:9")

(check-equal?
 (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil" list)
 7
 "vector-find-median:9list")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (let ((v (vector 19))) (vector-select! < v 0))
 19
 "vector-select!:singleton:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 0))
 3
 "vector-select!:ten:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 2))
 9
 "vector-select!:ten:2")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 8))
 22
 "vector-select!:ten:8")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 9))
 23
 "vector-select!:ten:9")

(check-equal?
 (let ((v (vector 19))) (vector-select! < v 0 0))
 19
 "vector-select!:singleton:0:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 0 0))
 3
 "vector-select!:ten:0:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 2 0))
 9
 "vector-select!:ten:2:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 8 0))
 22
 "vector-select!:ten:8:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 9 0))
 23
 "vector-select!:ten:9:0")

(check-equal?
 (let ((v (vector 19))) (vector-select! < v 0 0 1))
 19
 "vector-select!:singleton:0:0:1")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 0 0 10))
 3
 "vector-select!:ten:0:0:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 2 0 10))
 9
 "vector-select!:ten:2:0:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 8 0 10))
 22
 "vector-select!:ten:8:0:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 9 0 10))
 23
 "vector-select!:ten:9:0:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 0 4 10))
 3
 "vector-select!:ten:0:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 2 4 10))
 13
 "vector-select!:ten:2:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 4 4 10))
 21
 "vector-select!:ten:4:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 5 4 10))
 23
 "vector-select!:ten:5:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 0 4 10))
 3
 "vector-select!:ten:0:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 2 4 10))
 13
 "vector-select!:ten:2:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 3 4 10))
 13
 "vector-select!:ten:3:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 4 4 10))
 21
 "vector-select!:ten:4:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 5 4 10))
 23
 "vector-select!:ten:9:4:10")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 0 4 8))
 9
 "vector-select!:ten:0:4:8")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 1 4 8))
 13
 "vector-select!:ten:1:4:8")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 2 4 8))
 13
 "vector-select!:ten:2:4:8")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23))) (vector-select! < v 3 4 8))
 21
 "vector-select!:ten:3:4:8")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (let ((v (vector)))
   (vector-separate! < v 0)
   (vector-sort < (vector-copy v 0 0)))
 '#()
 "vector-separate!:empty:0")

(check-equal?
 (let ((v (vector 19)))
   (vector-separate! < v 0)
   (vector-sort < (vector-copy v 0 0)))
 '#()
 "vector-separate!:singleton:0")

(check-equal?
 (let ((v (vector 19)))
   (vector-separate! < v 1)
   (vector-sort < (vector-copy v 0 1)))
 '#(19)
 "vector-separate!:singleton:1")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
   (vector-separate! < v 0)
   (vector-sort < (vector-copy v 0 0)))
 '#()
 "vector-separate!:ten:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
   (vector-separate! < v 3)
   (vector-sort < (vector-copy v 0 3)))
 '#(3 8 9)
 "vector-separate!:ten:3")

(check-equal?
 (let ((v (vector)))
   (vector-separate! < v 0 0)
   (vector-sort < (vector-copy v 0 0)))
 '#()
 "vector-separate!:empty:0:0")

(check-equal?
 (let ((v (vector 19)))
   (vector-separate! < v 0 0)
   (vector-sort < (vector-copy v 0 0)))
 '#()
 "vector-separate!:singleton:0:0")

(check-equal?
 (let ((v (vector 19)))
   (vector-separate! < v 1 0)
   (vector-sort < (vector-copy v 0 1)))
 '#(19)
 "vector-separate!:singleton:1:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
   (vector-separate! < v 0 0)
   (vector-sort < (vector-copy v 0 0)))
 '#()
 "vector-separate!:ten:0:0")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
   (vector-separate! < v 3 0)
   (vector-sort < (vector-copy v 0 3)))
 '#(3 8 9)
 "vector-separate!:ten:3:0")

(check-equal?
 (let ((v (vector 19)))
   (vector-separate! < v 0 1)
   (vector-sort < (vector-copy v 1 1)))
 '#()
 "vector-separate!:singleton:0:1")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
   (vector-separate! < v 0 2)
   (vector-sort < (vector-copy v 2 2)))
 '#()
 "vector-separate!:ten:0:2")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
   (vector-separate! < v 3 2)
   (vector-sort < (vector-copy v 2 5)))
 '#(3 9 13)
 "vector-separate!:ten:3:2")

(check-equal?
 (let ((v (vector)))
   (vector-separate! < v 0 0 0)
   (vector-sort < (vector-copy v 0 0)))
 '#()
 "vector-separate!:empty:0:0:0")

(check-equal?
 (let ((v (vector 19)))
   (vector-separate! < v 0 1 1)
   (vector-sort < (vector-copy v 1 1)))
 '#()
 "vector-separate!:singleton:0:1:1")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
   (vector-separate! < v 0 2 8)
   (vector-sort < (vector-copy v 2 2)))
 '#()
 "vector-separate!:ten:0:2:8")

(check-equal?
 (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
   (vector-separate! < v 3 2 8)
   (vector-sort < (vector-copy v 2 5)))
 '#(9 13 13)
 "vector-separate!:ten:3:2:8")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sorting routines often have internal boundary cases or
;;; randomness, so it's prudent to run a lot of tests with
;;; different lengths.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (all-sorts-okay? m n)
  (if (> m 0)
      (let* ((v (random-vector n))
             (v2 (vector-copy v))
             (lst (vector->list v))
             (ans (vector-sort < v2))
             (med (cond ((= n 0) -97)
                        ((odd? n)
                         (vector-ref ans (quotient n 2)))
                        (else
                         (/ (+ (vector-ref ans (- (quotient n 2) 1))
                               (vector-ref ans (quotient n 2)))
                            2)))))
        (define (dsort vsort!)
          (let ((v2 (vector-copy v)))
            (vsort! < v2)
            v2))
        (and (equal? ans (list->vector (list-sort < lst)))
             (equal? ans (list->vector (list-stable-sort < lst)))
             (equal? ans (list->vector (list-sort! < (list-copy lst))))
             (equal? ans (list->vector (list-stable-sort! < (list-copy lst))))
             (equal? ans (vector-sort < v2))
             (equal? ans (vector-stable-sort < v2))
             (equal? ans (dsort vector-sort!))
             (equal? ans (dsort vector-stable-sort!))
             (equal? med (vector-find-median < v2 -97))
             (equal? v v2)
             (equal? lst (vector->list v))
             (equal? med (vector-find-median! < v2 -97))
             (equal? ans v2)
             (all-sorts-okay? (- m 1) n)))
      #t))

(define (test-all-sorts m n)
  (or (all-sorts-okay? m n)
      (fail (list 'test-all-sorts m n))))

(for-each test-all-sorts
          '( 3  5 10 10 10 20 20 10 10 10 10 10  10  10  10  10  10)
          '( 0  1  2  3  4  5 10 20 30 40 50 99 100 101 499 500 501))

