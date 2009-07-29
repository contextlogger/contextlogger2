;; 
;; compiler.scm
;; 
;; Copyright 2006 Helsinki Institute for Information Technology (HIIT)
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

(module
 compiler 
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (lib "settings.scm" "wg"))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "tables.scm" "wg"))
 (require (lib "phases.scm" "wg"))
 (require (lib "phase-cxx-print.scm" "wg"))

 (require "pass-decl.scm")
 (require "pass-ctx-info.scm")
 (require "pass-globals.scm")
 (require "pass-name-resol.scm")
 (require "pass-leaving.scm")
 (require "pass-backend-adapt.scm")
 (require "cxx-concretifier.scm")

 ;; The pass system we have is such that some data that represents the
 ;; program being manipulated flows between passes; this data is named
 ;; "ast". There is also some control data that can be used to pass
 ;; related information along the compilation pipeline; this data is
 ;; named "tables". Each pass supports option may also be passed some;
 ;; options do not flow from one pass to another, as they are pass
 ;; specific.
 
 ;; For debugging.
 (define pass-dump-ast
   (carry-tables
    (lambda (ast)
      (pretty-nl ast)
      ast)))

 (phase-add 'ast-dump pass-dump-ast)

 ;; For debugging.
 (define (pass-dump-state ast tables)
  (parameterize ((print-struct #t))
    (pretty-nl (list ast tables)))
  (list ast tables))

 (phase-add 'state-dump pass-dump-state)

 ;; For debugging.
 (define (pass-dump-tables ast tables)
  (parameterize ((print-struct #t))
    (pretty-nl tables))
  (list ast tables))

 (phase-add 'tables-dump pass-dump-tables)
 
 ;; For debugging.
 (define (pass-stop . args)
   (error "stop"))

 (phase-add 'stop pass-stop)
 
 (define (pass-analyze-ast . args)
   (carry
    (list 
     pass-compact ; general purpose
     pass-prop-to-nlist ; translation
     pass-check-form ; analysis
   ;pass-dump-ast pass-stop
     pass-lift-stmt ; translation
     pass-stmt-seq ; translation
     pass-self-type ; analysis
     pass-func-type ; analysis
     pass-ctx-rename ; analysis
     pass-artificial ; analysis
     pass-access ; analysis
     pass-default-access ; analysis
     pass-flags ; analysis
     pass-expr-stmt ; analysis
     pass-name-unnamed ; analysis
     pass-add-implicits ; analysis
     pass-add-decl-id ; analysis
     pass-table-globals ; analysis
     pass-update-decl-table ; analysis
     pass-resolve-names ; analysis
     pass-update-decl-table ; analysis
     pass-ctor-flag
     pass-leave-infer ; analysis
     pass-leave-check ; checking
     pass-leave-assumption ; checking
     )
    args))

 (phase-add 'cxx-analyze pass-analyze-ast)

 ;; Defining a basename is quite essential, as it is for instance used
 ;; to add includes within the different concrete compilation units as
 ;; required. Hence we consider it as an important attribute of a
 ;; cxx-unit, rather than seeing it as something that is only required
 ;; during printing.
 (define (ensure-has-basename ast)
   (let ((basename (get-opt-nlist-elem-1 ast 'basename)))
     (unless basename
       (set! basename
             (or (gop-value 'basename)
                 (error "no basename defined")))
       (push-set! ast `(basename ,basename))))
   ast)

 ;; This pass is different in that it returns a list of (ast, table)
 ;; lists, rather than a single pair.
 (define/kw (pass-1n-concretify ast tables
                                #:key
                                separate-h
                                separate-inl
                                harness-h
                                signature-h
                                do-not-edit
                                #:other-keys op)
   (when separate-inl
     (set! separate-h #t))
   (let ((ast-list
          (ast-concretify
           (ensure-has-basename ast)
           (cond
            (separate-inl '(h-file inl-file cpp-file))
            (separate-h '(h-file cpp-file))
            (else '(one-file)))
           #:no-external (gop-value 'no-external)
           #:separate-inl separate-inl
           #:harness-h harness-h
           #:do-not-edit do-not-edit
           #:signature-h signature-h)))
     (map
      (lambda (ast)
        (let ((this-tables tables)
              (filename (fget-opt-nlist-elem-1 ast 'filename))
              (basename (fget-opt-nlist-elem-1 ast 'basename)))
          (when filename
            (set! this-tables
                  (tables-set this-tables 'filename filename)))
          (when basename
            (set! this-tables
                  (tables-set this-tables 'basename basename)))
          (list ast this-tables)))
      ast-list)))

 (phase-add 'cxx-concretify pass-1n-concretify
            #:type '(ast-tables pair-list))

 ;; This only works if we want all the C++ code into a single file.
 (define (pass-11-concretify ast tables)
   (define res (pass-1n-concretify ast tables))
   (first res))

 ;; Can be used to construct passes that accept keyword arguments, but
 ;; ignore them.
 (define (ignore-op pass)
   (lambda (ast-data tables . op)
     (pass ast-data tables)))



 
 (define pass-compile-ast
   (compose-carry
    pass-analyze-ast
    pass-no-emit ; converts to abstract C++
    pass-cxx-names ; converts to abstract C++
    pass-qname ; assigns qualified names to declarations
    pass-11-concretify ; does the sectioning
    pass-doxygen ; produces Doxygen comments
    ;;pass-dump-ast
    pass-adapt ; adapts for pretty printer
    pass-compact ; general purpose
    ))

 (phase-add 'cxx-compile pass-compile-ast)





 (phase-add 'test1
            (compose-carry
             pass-no-emit ; converts to abstract C++
             pass-cxx-names ; converts to abstract C++
             pass-qname ; prepares for sectioning
             ))
 (phase-add 'test2 pass-1n-concretify
            #:type '(ast-tables pair-list))
 (phase-add 'test3
            (compose-carry
             pass-doxygen ; produces Doxygen comments
             ;;pass-dump-ast
             pass-adapt ; adapts for pretty printer
             pass-compact ; general purpose
             ))

 
 
 
 (define to-phase (fix phase-new '(ast-tables ast-tables)))

 ;; This gets quite convoluted just because we want to allow passing
 ;; of options to a particular subphase. So complicated in fact that
 ;; it might be better to pass such options via "tables". Pass
 ;; function keyword arguments might still be useful for phase
 ;; construction time configuration, but their use can get on the way
 ;; of composition, so care is required there.
 (phase-add
  'cxx-compile-new
  (lambda (ast tables . op)
    ((phase-function
      (apply
       phase-compose
       (map
        (lambda (x)
          (if (procedure? x)
              (to-phase x)
              x))
        (list
         pass-analyze-ast
         pass-no-emit ; converts to abstract C++
         pass-cxx-names ; converts to abstract C++
         pass-qname ; prepares for sectioning
         (phase-fix-op (phase-get-reqd-entry 'cxx-concretify) op)
         pass-doxygen ; produces Doxygen comments
         pass-adapt ; adapts for pretty printer
         pass-compact ; general purpose
         ))))
     ast tables))
  #:type '(ast-tables pair-list))

  




 



 
 (define* (pass-identity . args)
   args)

 (phase-add 'identity pass-identity)
 
 (define* (analyze-in-new-env ast)
   (compile-ast ast #:analyze? #t))

 (define* (compile-in-new-env ast)
   (compile-ast ast))

 (define/kw* (compile-ast ast 
			  #:key 
			  analyze?
			  tables)
   (define phases '(cxx-analyze))
   (unless analyze?
     (push-set! phases 'cxx-compile))
   (apply new-compile-ast ast #:tables tables phases))

 ;; Note that any keyword arguments must appear before the phase list.
 (define/kw* (new-compile-ast ast
                              #:key tables
                              #:body phases)
   ((apply make-pass-for-phases phases)
    ast
    (or tables (new-tables))))
 
) ; end module
