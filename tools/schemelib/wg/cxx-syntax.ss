#lang scheme/base

;; 
;; Copyright 2009 Helsinki Institute for Information Technology (HIIT)
;; and the authors. All rights reserved.
;; 
;; Authors: Tero Hasu <tero.hasu@hut.fi>
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

;; This module basically tries to make it more difficult to make
;; mistakes when building ASTs using node-ctors.scm. This is done by
;; exporting node ctors with contracts. The contracts are not as
;; strict as they could be, but should catch quite a few syntax
;; errors.

;; All of these we shall rename to avoid conflicts with the "scheme"
;; language. This is the good opportunity to do so without breaking
;; existing code.
(require (except-in scheme
                    class
                    const
                    export
                    file
                    import
                    init
                    new
                    private
                    public
                    struct
                    this
                    unit
                    ))

(require (lib "usual-4.ss" "common"))
(require "node-ctors.scm")
(require "type-util.scm")
(require (prefix-in a. "ast-spec.scm"))

(provide ccond cswitch)

(define cclass class)
(define cexport export)
(define cfile file)
(define cimport import)
(define cinit init)
(define cnew new)
(define cprivate private)
(define cpublic public)
(define cstruct struct)
(define cthis this)
(define cunit unit)
(define cconst const) ;; defined in scheme and scheme/function (in v4.2.1)

(define (elem-named? name elem)
  (list-named? elem name))

(define (make-elem-pred name)
  (lambda (elem) (list-named? elem name)))

(define-for-syntax (make-pred-symbol name)
  (string->symbol (string-append (symbol->string name) "?")))
 
(define-for-syntax (datum-apply stx f . args)
  (datum->syntax stx (apply f (map syntax->datum args))))

(define-for-syntax (make-pred-name name^)
  (datum-apply name^ make-pred-symbol name^))

(define-syntax* (define-elem-pred stx)
  (syntax-case stx ()
    ((_ name/)
     (with-syntax ((pred/ (make-pred-name (syntax name/))))
       (syntax/loc stx
         (define pred/ (make-elem-pred (quote name/))))))))

(define-syntax func-listof/c
  (syntax-rules ()
    ((_ elem/c result/c)
     (->* () () #:rest (listof elem/c) result/c))))

;; We are not using the ADT declarations to declare attributes, and
;; hence do not yet have predicates for there, but for contracts we
;; can use them.
(define-elem-pred arg)
(define-elem-pred args)
(define-elem-pred base)
(define-elem-pred basename)
(define-elem-pred bases)
(define-elem-pred body)
(define-elem-pred ctor-init-list)
(define-elem-pred include)
(define-elem-pred includes)
(define-elem-pred init)
(define-elem-pred leaving)
(define-elem-pred name)
(define-elem-pred non-sharable)
(define-elem-pred returns)
(define-elem-pred splice)
(define-elem-pred type)

;; Here the "/c" means "coercable".
(define* (symbol/c? x)
  (some-hold (list string? symbol?) x))

;; Here the "/c" means "coercable".
(define* (expr/c? x)
  (some-hold (list a.expression? string? symbol? integer?) x))

(define (class-attr-symbol? x)
  (true? (memq x '(export))))

(define (var-attr-symbol? x)
  (true? (memq x '(public private protected))))

(define (func-attr-symbol? x)
  (true? (memq x '(export extern-c inline non-modifying public private protected pure static virtual))))

(define texpr-arg/c
  (or/c symbol/c? a.type-expression?))

(define new/c
  (->* (texpr-arg/c) ((listof expr/c?)) any/c))

(define call/c
  (->* (expr/c?) ((listof expr/c?)) any/c))

(define call/id/c
  (->* (expr/c? expr/c?) ((listof expr/c?)) any/c))

(define func-decl/c
  (func-listof/c (or/c args? returns? func-attr-symbol? leaving? name? a.block? ctor-init-list?) any/c))

(provide/contract
 (arg (func-listof/c (or/c name? type?) any/c))
 (args (func-listof/c arg? any/c))
 (basename (-> symbol/c? any/c))
 (bases (func-listof/c (or/c symbol/c? base?) any/c))
 (block (func-listof/c (or/c a.var-decl? a.statement? a.expression? a.cxx-chunk? splice?) any/c))
 (body (func-listof/c (or/c a.declaration? splice?) any/c))
 (call call/c)
 (call-on call/id/c)
 (call-via call/id/c)
 (cclass (func-listof/c (or/c bases? class-attr-symbol? non-sharable? name? a.docstring? body?) any/c))
 (cconst (-> texpr-arg/c any/c))
 (cdelete (-> expr/c? any/c))
 (cdelete-array (-> expr/c? any/c))
 (cexport symbol?)
 (cinit (-> expr/c? any/c))
 (cnew new/c)
 (const (-> texpr-arg/c any/c)) ;; xxx deprecated
 (cpp-else (->* () () #:rest any/c any/c)) ;; xxx would like to use context-spec? here if not for the kw arg differences
 (cpp-end (->* () () #:rest any/c any/c))
 (cpp-if (->* (string?) () #:rest any/c any/c))
 (cprivate symbol?)
 (cpublic symbol?)
 (ctor func-decl/c)
 (ctor-init-list (func-listof/c (or/c a.ctor-super? a.ctor-var?) any/c))
 (ctor-super (-> texpr-arg/c (listof expr/c?) any/c))
 (ctor-var (-> symbol/c? (listof expr/c?) any/c))
 (cunit (func-listof/c (or/c basename? includes? body? a.docstring?) any/c))
 (cxx-cmt-doc (func-listof/c string? any/c))
 (cxx-doc (func-listof/c string? any/c))
 (cxx-exported-declarations (func-listof/c string? any/c))
 (cxx-internal-declarations (func-listof/c string? any/c))
 (cxx-line (-> string? any/c))
 (doc (func-listof/c string? any/c))
 (dtor func-decl/c)
 (extern-c symbol?)
 (field-on (-> expr/c? symbol/c? any/c))
 (field-via (-> expr/c? symbol/c? any/c))
 (func func-decl/c)
 (includes (func-listof/c (or/c include? splice?) any/c))
 (inline symbol?)
 (leaving leaving?)
 (leaving-new new/c)
 (local-include (-> string? any/c))
 (name (-> (or/c symbol? string?) name?))
 (non-modifying symbol?)
 (non-sharable non-sharable?)
 (not-leaving leaving?)
 (protected symbol?)
 (ptr-to (-> texpr-arg/c any/c))
 (pure symbol?)
 (qual-name (func-listof/c (or/c symbol/c?) any/c))
 (ref-to (-> texpr-arg/c any/c))
 (return (->* () (expr/c?) any/c))
 (returns (func-listof/c (or/c type?) any/c))
 (static symbol?)
 (system-include (-> string? any/c))
 (type (-> texpr-arg/c any/c))
 (var (func-listof/c (or/c type? name? init? var-attr-symbol? a.docstring?) any/c))
 (virtual symbol?)
 )
