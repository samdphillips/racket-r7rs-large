#lang r7rs

(define-library (scheme sort)
  (export list-sorted?               vector-sorted?
          list-sort                  vector-sort
          list-stable-sort           vector-stable-sort
          list-sort!                 vector-sort!
          list-stable-sort!          vector-stable-sort!
          list-merge                 vector-merge
          list-merge!                vector-merge!
          list-delete-neighbor-dups  vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median         vector-find-median!
          vector-select!             vector-separate!
          )

  (import (except (scheme base) vector-copy vector-copy!)
          (rename (only (scheme base) vector-copy vector-copy! vector-fill!)
                  (vector-copy  r7rs-vector-copy)
                  (vector-copy! r7rs-vector-copy!)
                  (vector-fill! r7rs-vector-fill!))
          (only (srfi 27) random-integer))

  (import (only (rnrs base-6) assert)
          (rename (rnrs sorting-6)
                  (list-sort    r6rs-list-sort)
                  (vector-sort  r6rs-vector-sort)
                  (vector-sort! r6rs-vector-sort!)))

  (include "private/include/sort/merge.scm")
  (include "private/include/sort/delndups.scm")     ; list-delete-neighbor-dups etc
  (include "private/include/sort/sortp.scm")        ; list-sorted?, vector-sorted?
  (include "private/include/sort/vector-util.scm")
  (include "private/include/sort/sortfaster.scm")
  (include "private/include/sort/select.scm"))

