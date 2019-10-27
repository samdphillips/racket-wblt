#lang racket/base

(require rebellion/base/comparator)

(provide (all-defined-out)
         (all-from-out rebellion/base/comparator))

(define (compare>? cmp a b)
  (equal? greater (compare cmp a b)))

(define-syntax refine-compare
  (syntax-rules ()
    [(_ (cmp e0 e1)) (compare cmp e0 e1)]
    [(_ (cmp e0 e1) c1 cn ...)
     (let ([v (compare cmp e0 e1)])
       (if (equal? equivalent v)
           (refine-compare c1 cn ...)
           v))]))