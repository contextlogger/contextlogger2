(module
  active-object
  mzscheme

  (require (lib "usual.scm" "wg"))
  (require (lib "ast-util.scm" "wg"))
  (require (lib "compact.scm" "wg"))
  (require (lib "node-ctors.scm" "wg"))
  (require (lib "local-util.scm" "codegen"))

  (define (has-elem? name ast)
    (true? (assq name (cdr ast))))
  
  (define (ensure-has-elems elems ast)
    (for-each
     (lambda (elem)
       (alet name (car elem)
             (unless (has-elem? name ast)
               (push-set! ast elem))))
     elems)
    ast)
  
  (define* ao-scheduler-add-this
    (call 'CActiveScheduler::Add '(this)))

  (define/kw* (ao-ctor-super #:key (priority 'EPriorityStandard))
    (ctor-super 'CActive (list priority)))

  (define* (ao-ctor-init-list . op)
    (ctor-init-list (apply ao-ctor-super op)))

  (define* ao-basic-dtor
    (dtor
     public virtual
     (block
      (call 'Cancel))))
  
  (define* (run-l . args)
    ;; RunL has no arguments or return values, and the body is
    ;; application specific, so there is not all that much we can do
    ;; apart for giving default values to certain attributes.
    (alet ast (apply func (name 'RunL) args)
          (ensure-has-elems
           (list 
            '(private #t)
            '(virtual #t)
            '(leaving #t))
           ast)))

  (define* (do-cancel . args)
    (define ast
      (apply func
             (name 'DoCancel)
             args))
    (ensure-has-elems
     (list
      '(private #t)
      '(virtual #t)
      '(leaving #f))
     ast))

  (define* (run-error . args)
    (define ast
      (apply func
             (name 'RunError)
             args))
    (ensure-has-elems
     (list
      (args (arg (type 'TInt) (name 'aError)))
      (returns (type 'TInt))
      '(private #t)
      '(virtual #t)
      '(leaving #f))
     ast))
  
  (define* (ao-class . args)
    (define ast
      (apply class args))

    ;; For easier analysis.
    (set! ast (compact-flatten ast))

    (set! ast
          (ensure-has-elems
           (list
            (bases 'CActive))
           ast))

    ast)

) ; end module
