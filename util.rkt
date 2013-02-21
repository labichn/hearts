#lang racket

(require rackunit)
(provide (struct-out none)
         (struct-out some)
         opt-map try-opt-map)

;; an opt[x] is one of:
;; - (none)
;; - (some X)
(struct none () #:transparent)
(struct some (val) #:transparent)

;; opt-map : [x -> y] opt[x] -> opt[y]
(define (opt-map f ov)
  (if (some? ov) (some (f (some-val ov))) (none)))

;; try-opt-map : [x -> y] opt[x] -> opt[y]
(define (try-opt-map f ov)
  (with-handlers (((λ (x) #t) (λ (x) (none)))) (opt-map f ov)))
