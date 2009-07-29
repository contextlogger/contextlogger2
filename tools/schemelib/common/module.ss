#lang scheme

;; 
;; module.ss
;; 
;; Copyright 2008 Tero Hasu
;; 

(provide define* define-syntax*
         define-struct*
         define-signature* define-unit*)

(define-syntax define*
  (syntax-rules ()
    ((_ (name arg ... . rest) body ...)
     (begin
       (define (name arg ... . rest) body ...)
       (provide name)))
    ((_ (name arg ...) body ...)
     (begin
       (define (name arg ...) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define name body ...)
       (provide name)))))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (name stx) body ...)
     (begin
       (define-syntax (name stx) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define-syntax name body ...)
       (provide name)))))

(define-syntax define-struct*
  (syntax-rules ()
    ((_ (name super-name) rest ...)
     (begin
       (define-struct (name super-name) rest ...)
       (provide (struct-out name))))
    ((_ name rest ...)
     (begin
       (define-struct name rest ...)
       (provide (struct-out name))))))

(define-syntax define-signature*
  (syntax-rules ()
    ((_ name rest ...)
     (begin
       (define-signature name rest ...)
       (provide name)))))

(define-syntax define-unit*
  (syntax-rules ()
    ((_ name rest ...)
     (begin
       (define-unit name rest ...)
       (provide name)))))
