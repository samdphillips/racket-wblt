#lang racket/base

(require rebellion/base/pair
         rebellion/base/variant

         rebellion/collection/list
         
         rebellion/streaming/transducer
         
         rebellion/type/tuple

         rebellion/private/impossible

         "comparator.rkt"
         "wblt.rkt")

(provide sorting)

(define (sorting-tree-compare cmp)
  (define value-cmp
    (comparator-map cmp pair-first))
  (define epoch-cmp
    (comparator-map (comparator-reverse real<=>) pair-second))
  (tree-compare
   (make-comparator
    (lambda (a b)
      (refine-compare
       (value-cmp a b)
       (epoch-cmp a b))))))

(module+ test
  (require rackunit)

  (define-check (check-tree-compare? value-cmp e0 e1 expected)
    (let* ([cmp (sorting-tree-compare value-cmp)]
           [a e0]
           [b e1]
           [actual (compare cmp a b)])
      (with-check-info*
          (list (make-check-info 'actual actual)
                (make-check-info 'expected expected)
                (make-check-info 'a-value (tree-value a))
                (make-check-info 'b-value (tree-value b)))
        (lambda ()
          (unless (equal? actual expected)
            (fail-check))))))
    
  (check-tree-compare? real<=>
                       (make-singleton-tree (pair 1 0))
                       (make-singleton-tree (pair 2 1))
                       lesser)
  
  (check-tree-compare? real<=>
                       (make-singleton-tree (pair 2 1))
                       (make-singleton-tree (pair 1 0))
                       greater)

  (check-tree-compare? real<=>
                       (make-singleton-tree (pair 42 100))
                       (make-singleton-tree (pair 42 50))
                       lesser))

(define (sorting [comparator real<=>] #:key [key-function values])
  (define cmp
    (sorting-tree-compare
     (comparator-map comparator key-function))) 
  
  (define (start)
    (variant #:consume empty-list))

  (define (consume state element)
    (variant #:consume (list-insert state element)))
  
  (define (half-close state)
    (define next-state
      (tree-insert* cmp
                    empty-tree
                    (for/list ([v (in-list state)]
                               [n (in-naturals)])
                      (pair v n))))
    (if (empty-tree? next-state)
        (variant #:finish #f)
        (variant #:half-closed-emit next-state)))             
  
  (define (half-closed-emit tree)
    (define-values (element next-tree) (tree-remove-min cmp tree))
    (define next-state
      (cond
        [(empty-tree? next-tree) (variant #:finish #f)]
        [else (variant #:half-closed-emit next-tree)]))
    (half-closed-emission next-state (pair-first element)))
  
  (make-transducer
   #:starter start
   #:consumer consume
   #:emitter impossible
   #:half-closer half-close
   #:half-closed-emitter half-closed-emit
   #:finisher void
   #:name 'sorting))
