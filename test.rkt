#lang racket

(require rackunit)

;; test ((x (λ (x) x)) (y (λ (y) y)))
;; => (list 'x ((λ (x) x) (test-x 'x))
;;          'y ((λ (y) y) (test-y 'y)))
;; => (list 'x 'x 'y 'y)

(define-syntax (test stx)
  (syntax-case stx ()
    ((_ name (clause ...))
     #'(define (name x) x))
    (_ (error 'test "bad syntax"))))
;;(check-equal? (test-one 5) 5)

