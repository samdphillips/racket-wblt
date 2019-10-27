#lang racket/base

(require syntax/parse/define
         
         rebellion/type/record
         rebellion/type/singleton

         "comparator.rkt")

(provide empty-tree
         empty-tree?

         make-singleton-tree
         
         tree-value
         tree-left
         tree-right

         tree-insert
         tree-insert*

         tree-remove-min
         tree-compare)         

(define-singleton-type empty-tree)
(define-record-type tree (value weight left right))

(define (weight t)
  (cond
    [(empty-tree? t) 0]
    [else (tree-weight t)]))

(define (make-singleton-tree value)
  (tree #:value value
        #:weight 1
        #:left empty-tree
        #:right empty-tree))

(define (tree-compare cmp)
  (comparator-map cmp tree-value))

(define (tree-merge cmp a b)
  (cond
    [(empty-tree? a) b]
    [(empty-tree? b) a]
    [(compare>? cmp a b) (tree-merge cmp b a)]
    [else
     (define a-weight (tree-weight a))
     (define b-weight (tree-weight b))
     (define-values (left right)
       (let* ([left  (tree-left a)]
              [right (tree-right a)]
              [left-weight (weight left)]
              [right-weight (weight right)]
              [merged (tree-merge cmp right b)])
         (if (< left-weight (+ right-weight b-weight))
             (values merged left)
             (values left merged))))            
     (tree #:value (tree-value a)
           #:weight (+ a-weight b-weight)
           #:left left
           #:right right)]))

(define (tree-merge* cmp ts)
  (define (merge-all ts ms)
    (if (null? ts)
        (merge-all ms null)
        (let ([t (car ts)]
              [ts (cdr ts)])
          (cond
            [(and (null? ms) (null? ts)) t]
            [(null? ts) (merge-all (cons t ms) null)]
            [else
             (merge-all (cdr ts)
                        (cons
                         (tree-merge cmp t (car ts))
                         ms))]))))
  (if (null? ts)
      ts
      (merge-all ts null)))

(define (tree-insert cmp t v)
  (tree-merge cmp t (make-singleton-tree v)))

(define (tree-insert* cmp t vs)  
  (define ts
    (append (if (empty-tree? t) null (list t))
            (map make-singleton-tree vs)))
  (tree-merge* cmp ts))

(define (tree-remove-min cmp t)  
  (values (tree-value t)
          (tree-merge cmp
                      (tree-left t)
                      (tree-right t))))


