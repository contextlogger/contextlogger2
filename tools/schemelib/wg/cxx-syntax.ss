#lang scheme/base

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
  (true? (memq x '(export public private protected pure static virtual))))

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
 (cdelete (-> expr/c? any/c))
 (cdelete-array (-> expr/c? any/c))
 (cpp-else (->* () () #:rest any/c any/c)) ;; xxx would like to use context-spec? here if not for the kw arg differences
 (cpp-end (->* () () #:rest any/c any/c))
 (cpp-if (->* (string?) () #:rest any/c any/c))
 (const (-> texpr-arg/c any/c))
 (ctor func-decl/c)
 (ctor-init-list (func-listof/c (or/c a.ctor-super? a.ctor-var?) any/c))
 (ctor-super (-> texpr-arg/c (listof expr/c?) any/c))
 (ctor-var (-> symbol/c? (listof expr/c?) any/c))
 (cxx-cmt-doc (func-listof/c string? any/c))
 (cxx-doc (func-listof/c string? any/c))
 (cxx-exported-declarations (func-listof/c string? any/c))
 (cxx-internal-declarations (func-listof/c string? any/c))
 (cxx-line (-> string? any/c))
 (doc (func-listof/c string? any/c))
 (dtor func-decl/c)
 (cexport symbol?)
 (func func-decl/c)
 (includes (func-listof/c (or/c include? splice?) any/c))
 (cinit (-> expr/c? any/c))
 (leaving leaving?)
 (leaving-new new/c)
 (local-include (-> string? any/c))
 (name (-> (or/c symbol? string?) name?))
 (cnew new/c)
 (non-sharable non-sharable?)
 (not-leaving leaving?)
 (cprivate symbol?)
 (protected symbol?)
 (ptr-to (-> texpr-arg/c any/c))
 (cpublic symbol?)
 (pure symbol?)
 (qual-name (func-listof/c (or/c symbol/c?) any/c))
 (ref-to (-> texpr-arg/c any/c))
 (return (->* () (expr/c?) any/c))
 (returns (func-listof/c (or/c type?) any/c))
 (static symbol?)
 (system-include (-> string? any/c))
 (type (-> texpr-arg/c any/c))
 (cunit (func-listof/c (or/c basename? includes? body? a.docstring?) any/c))
 (var (func-listof/c (or/c type? name? init? var-attr-symbol? a.docstring?) any/c))
 (virtual symbol?)
 )
