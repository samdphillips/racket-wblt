#lang racket/base

(require racket/format

         pict
         pict/tree-layout

         rebellion/base/comparator
         "wblt.rkt")

(define (tree->layout t)
  (cond
    [(empty-tree? t) (tree-layout)]
    [else
     (tree-layout #:pict (text (~a (tree-value t)))
                  (tree->layout (tree-left t))
                  (tree->layout (tree-right t)))]))

(define (viz vs)
  (for/fold ([t empty-tree]
             [ps null]
             #:result (reverse ps))
            ([x (in-list vs)])    
    (define new (tree-insert t x))
    (values new (cons (naive-layered (tree->layout new)) ps))))

(define (dt t)
  (naive-layered
   (tree->layout t)))