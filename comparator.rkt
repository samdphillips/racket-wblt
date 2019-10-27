#lang racket/base

(require rebellion/base/comparator)

(provide (all-defined-out)
         (all-from-out rebellion/base/comparator))

(define (compare>? cmp a b)
  (equal? greater (compare cmp a b)))

(define-syntax refine-compare
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e1 en ...)
     (let ([v e0])
       (if (equal? equivalent v)
           (refine-compare e1 en ...)
           v))]))