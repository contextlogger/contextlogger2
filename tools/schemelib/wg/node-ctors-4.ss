#lang scheme/base

(require (lib "usual-4.ss" "common"))
(require* "node-ctors.scm")

(define-syntax sswitch/i
  (syntax-rules (case else)
    ((_)
     'nocode)
    ((_ (else z ...))
     (switch-default (block z ...)))
    ((_ (case x y ...) z ...)
     (switch-case x (block y ... (cbreak)) (sswitch/i z ...)))
    ))

;; An alternative SPECS switch implementation.
;; 
;; For some reason this does not work if defined in the mzscheme
;; language.
(define-syntax* sswitch
  (syntax-rules ()
    ((_ ex x ...)
     (switch-stmt ex (sswitch/i x ...)))))

