#lang r7rs

(define-library (scheme list)
  (import (scheme base)
          (scheme cxr))
  (export xcons make-list list-tabulate cons* list-copy
          proper-list? circular-list? dotted-list? not-pair? null-list? list=
          circular-list length+
          iota
          first second third fourth fifth sixth seventh eighth ninth tenth
          car+cdr
          take       drop
          take-right drop-right
          take!      drop-right!
          split-at   split-at!
          last last-pair
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count
          append! append-reverse append-reverse! concatenate concatenate!
          unfold       fold       pair-fold       reduce
          unfold-right fold-right pair-fold-right reduce-right
          append-map append-map! map! pair-for-each filter-map map-in-order
          filter  partition  remove
          filter! partition! remove!
          find find-tail any every list-index
          take-while drop-while take-while!
          span break span! break!
          delete delete!
          alist-cons alist-copy
          delete-duplicates delete-duplicates!
          alist-delete alist-delete!
          reverse!
          lset<= lset= lset-adjoin
          lset-union  lset-intersection  lset-difference  lset-xor  lset-diff+intersection
          lset-union! lset-intersection! lset-difference! lset-xor! lset-diff+intersection!)

  (export cons pair? null? car cdr set-car! set-cdr!
          list length append reverse list-ref
          caaaar caaadr
          caaar caadar
          caaddr caadr
          cadaar cadadr
          cadar caddar
          cadddr caddr
          cdaaar cdaadr
          cdaar cdadar
          cdaddr cdadr
          cddaar cddadr
          cddar cdddar
          cddddr cdddr
          memq memv assq assv)

  (begin
    ;; check-arg used as primitive contract check throughout library
    (define-syntax check-arg
      (syntax-rules ()
        ((_ pred? v caller)
         (let ((val v))
           (unless (pred? val)
             (error "Bad argument"
                    val
                    'pred?
                    'caller)))))))
  (begin
    (define-syntax :optional
      (syntax-rules ()
        ((_ v default) (if (null? v) default (car v))))))

  (begin
    ;; let-optionals used for some optional args
    (define-syntax let-optionals
      (syntax-rules ()
        ((_ e ((v0 d0) (v* d*) ...) body ...)
         (let* ((vs e)
                (v0 (:optional vs d0))
                (v  (if (null? vs) '() (cdr vs))))
           (let-optionals v ((v* d*) ...) body ...)))
        ((_ e () body ...) (let () body ...)))))

  (begin
    ;; receive from SRFI-8
    (define-syntax receive
      (syntax-rules ()
        ((_ vars expr body0 body ...)
         (call-with-values
           (lambda () expr)
           (lambda vars body0 body ...))))))

  (include "private/include/srfi-1.scm"))

