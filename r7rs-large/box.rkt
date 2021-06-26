#lang r7rs

(define-library (scheme box)
  (export box box? unbox set-box!)
  (import (only (racket base) box box? unbox set-box!)))

