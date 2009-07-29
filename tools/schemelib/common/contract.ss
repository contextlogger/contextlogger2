#lang scheme

(provide define/c* unary-predicate?)

(define-syntax define/c*
  (syntax-rules ()
    ((_ (name arg ... . rest) contract body ...)
     (begin
       (define (name arg ... . rest) contract body ...)
       (provide/contract (name contract))))
    ((_ (name arg ...) contract body ...)
     (begin
       (define (name arg ...) body ...)
       (provide/contract (name contract))))
    ((_ name contract body)
     (begin
       (define name body)
       (provide/contract (name contract))))))

(define unary-predicate? (-> any/c boolean?))
