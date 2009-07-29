#lang scheme

;; This module provides functionality that helps with interacting with
;; "legacy" mzscheme libraries.

(require (lib "usual.scm" "wg"))
(require (prefix-in mz. mzscheme))

;; This is useful when, in the "scheme" language", invoking an
;; "mzscheme" language function that accepts keyword arguments.
;; Instead of using (f arg0 arg1), just do (mz-call f arg0 arg1).
(define mz-call
  (make-keyword-procedure
   (lambda (kw-names kw-vals f . args)
     (let* ((op (apply append (zip kw-names kw-vals))))
       (mz.apply f (append args op))))))

(provide mz-call)
