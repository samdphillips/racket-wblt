#lang racket/base

;; from https://gist.github.com/jackfirth/876bbf8802ddd6864872f250d3b96a47

(require racket/format
         racket/list
         racket/random
         racket/sequence
         racket/set

         rebellion/base/option

         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list

         rebellion/streaming/reducer
         rebellion/streaming/transducer

         rebellion/type/record
         rebellion/type/wrapper

         (prefix-in wblt: "transducer-sorting-wblt.rkt"))

(define-record-type gemstone (kind weight))

(define (random-gemstone weight)
  (gemstone #:kind (random-ref (set 'ruby 'sapphire 'emerald 'topaz))
            #:weight (random 1 (add1 weight))))

(define (random-gems count)
  (for/vector ([_ (in-range count)])
    (random-gemstone count)))

(define (bottom-10/racket gems)
  (take (sort (sequence->list gems) < #:key gemstone-weight) 10))

(define (bottom-10/rebellion gems)
  (transduce gems
             (sorting #:key gemstone-weight)
             (taking 10)
             #:into into-list))

(define (bottom-10/wblt gems)
  (transduce gems
             (wblt:sorting #:key gemstone-weight)
             (taking 10)
             #:into into-list))

(define-record-type timing-data (label cpu-time real-time gc-time))

(define (measure-runtime label thunk)
  (define-values (_ cpu real gc) (time-apply thunk empty-list))
  (timing-data #:label label #:cpu-time cpu #:real-time real #:gc-time gc))

(define (bottom-10-benchmark input-length)
  (define gems (random-gems input-length))
  (set (measure-runtime "Standard racket sort (milliseconds)"
                        (λ () (bottom-10/racket gems)))
       (measure-runtime "Rebellion lazy sort (milliseconds)"
                        (λ () (bottom-10/rebellion gems)))
       (measure-runtime "WBLT (milliseconds)"
                        (λ () (bottom-10/wblt gems)))))       

(define-record-type stats (sum count max min average))

(define (single-datum-stats x)
  (stats #:sum x #:count 1 #:max x #:min x #:average x))

(define (stats+ s p)
  (define sum (+ (stats-sum s) (stats-sum p)))
  (define count (+ (stats-count s) (stats-count p)))
  (stats #:sum sum
         #:count count
         #:max (max (stats-max s) (stats-max p))
         #:min (min (stats-min s) (stats-min p))
         #:average (/ sum count)))

(define into-stats
  (make-fold-reducer
   (λ (accumulated next)
     (option-case accumulated
                  #:present (λ (acc) (present (stats+ acc next)))
                  #:absent (λ () (present next))))
   absent
   #:name 'into-stats))

(define (format-entry e)
  (define (~n v) (~r #:min-width 9 #:precision '(= 2) v))
  (define s (entry-value e))
  (~a #:separator " "
      (~a #:min-width 40 (entry-key e))      
      (~n (stats-min s))
      (~n (stats-max s))
      (~n (stats-average s))))

(define (run-benchmark benchmark #:size size #:iterations iterations)
  (transduce (make-list iterations size)
             (append-mapping benchmark)
             (bisecting timing-data-label timing-data-cpu-time)
             (mapping-values single-datum-stats)
             (grouping into-stats)
             (mapping-values present-value)
             (mapping format-entry)
             #:into (into-for-each displayln)))

(module+ main
  (run-benchmark bottom-10-benchmark
                 #:size 1000
                 #:iterations 100))