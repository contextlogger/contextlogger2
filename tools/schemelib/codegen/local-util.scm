;; xxx the name of this module no longer makes sense

(module
  local-util
  mzscheme

  (require (lib "ast-util.scm" "wg"))
  (require (lib "usual.scm" "wg"))
  (require (lib "compact.scm" "wg"))
  (require (lib "compiler.scm" "wg"))
  (require (lib "file-util.scm" "wg"))
  (require (lib "string-util.scm" "wg"))
  (require (lib "code-print.scm" "wg"))
  (require (lib "node-ctors.scm" "wg"))

  (define compile-phase 
    `(cxx-compile-new #:separate-h #t
                      #:harness-h #t
                      #:do-not-edit #t
                      #:signature-h #t))
  
  (define* (generate-h-and-cpp ast)
    (new-compile-ast
     ast
     compile-phase
     `(cxx-print #:file? #t
                 #:force? #t
                 #:add-eof? #t
                 #:path ,(current-load-relative-directory))))
  
  (define* (dump-h-and-cpp ast)
    (new-compile-ast
     ast
     compile-phase
     `(cxx-print #:banners? #t)))
  
  (define* (dump-analyzed-ast ast)
    (new-compile-ast
     ast
     'cxx-analyze
     'ast-dump))
  
  (define* (dump-compiled-ast ast)
    (new-compile-ast
     ast
     `(cxx-compile-new #:separate-h #t
                       #:harness-h #t)
     'ast-dump))
  
  (define* (sensor-get-bool sensor name)
    (aand* elem (assq name (cdr sensor))
           (true? (cadr elem))))
  
  (define* (sensor-get-opt sensor name)
    (aand* elem (assq name (cdr sensor))
           (cadr elem)))
  
  (define* (sensor-get-list sensor name)
    (aif value (sensor-get-opt sensor name)
         (if (list? value)
             value
             (error-f "~s in ~s does not have a list value" name sensor))
         '()))

  (define* (sensor-get-elem sensor name)
    (aif elem (assq name (cdr sensor))
         elem
         (list name)))
  
  (define* (sensor-get-elem-opt sensor name)
    (assq name (cdr sensor)))
  
  (define* (sensor-get-reqd sensor name)
    (aif elem (assq name (cdr sensor))
         (aif value (cadr elem)
              value
              (error-f "~s in ~s may not have a false value" name sensor))
         (error-f "no ~s in ~s" name sensor)))

  (define* (hash-table-keys h)
    (hash-table-map h (lambda (k v) k)))
  
  (define* (const-ref-to t)
    (ref-to (const t)))
  
  (define* (unless-error c . tb)
    (cif (c== c 'KErrNone) (apply block tb)))

  (define* (when-error c . tb)
    (cif (c!= c 'KErrNone) (apply block tb)))

  (define* (if-error c t e)
    (cif (c!= c 'KErrNone) t e))

  (define* (if-no-error c t e)
    (cif (c== c 'KErrNone) t e))

  ;; This "passes" "arguments" to a class so that they are passed via
  ;; the constructor(s) and then stored as property with an
  ;; initializer. args-spec is of the form '(name type), and an "i" or
  ;; "a" prefix is added as appropriate.
  ;; 
  ;; The idea here: We might want to write something even higher level
  ;; that would take care of this passing of "arguments" to a class
  ;; and recording the references to the instance data. As it is that
  ;; aspect is still repetitive here. It can simply be a wrapper
  ;; function that modifies some of the keyword arguments passed to
  ;; make-ctor; can just add to the end of each relevant list.
  (define/kw* (make-class-args
               args-spec
               #:key
               (arg-list '())
               (init-list '())
               #:other-keys kw)
    (define (add-prefix p s)
      (string-append p (to-string s)))
    (let* ((types (map second args-spec))
           (names (map first args-spec))
           (arg-names (map (fix add-prefix "a") names))
           (init-names (map (fix add-prefix "i") names))
           (new-arg-list
            (map (lambda (n t) (arg (name n) (type t))) arg-names types))
           (new-init-list
            (map (lambda (i a) (ctor-var i (list a))) init-names arg-names))
           (var-decl-list
            (map (lambda (n t) (var private (name n) (type t))) init-names types)))
      (sc
       (apply make-ctor
              #:arg-list new-arg-list
              #:init-list new-init-list
              kw)
       (sc-list var-decl-list))))
  
  (define/kw* (make-ctor
               #:key
               class-name
               newl?
               newlc?
               constructl?
               call-constructl?
               ctor-code
               constructl-code
               (arg-list '())
               (init-list '()))
    (define ctor-args (cons 'args arg-list))
    (define ctor-init-list (cons 'ctor-init-list init-list))
    (define ctor-access (if (or newl? newlc?) 'private 'public))
    (unless class-name
      (error "no class name given"))
    (when constructl?
      (set! call-constructl? #t))

    (let* ((arg-names
            (map
             (lambda (arg) (sensor-get-reqd arg 'name))
             (cdr ctor-args))))
      (sc
       ;; NewLC.
       (ic newlc?
           (func
            (name 'NewLC)
            public static leaving
            ctor-args
            (returns (type (ptr-to class-name)))
            (block
             (var (name 'object) 
                  (type (ptr-to class-name))
                  (init (leaving-new class-name arg-names)))
             (call 'CleanupStack::PushL (list 'object))
             (ic call-constructl?
                 (call-via 'object 'ConstructL))
             (return 'object))))

       ;; NewL.
       (ic newl?
           (func
            (name 'NewL)
            public static leaving
            ctor-args
            (returns (type (ptr-to class-name)))
            (block
             (if newlc?
                 (sc
                  (var (name 'object)
                       (type (ptr-to class-name))
                       (init (call 'NewLC arg-names)))
                  (call 'CleanupStack::Pop)
                  (return 'object))
                 (sc
                  (var (name 'object) 
                       (type (ptr-to class-name))
                       (init (leaving-new class-name arg-names)))
                  (if call-constructl?
                      (sc
                       (call 'CleanupStack::PushL (list 'object))
                       (call-via 'object 'ConstructL)
                       (call 'CleanupStack::Pop)
                       (return 'object))
                      (return 'object)))))))

       ;; Constructor.
       (ctor
        ctor-access not-leaving
        ctor-args
        ctor-init-list
        (block
         (maybe-code ctor-code)))

       ;; ConstructL.
       (ic constructl?
           (func
            (name 'ConstructL)
            ctor-access leaving
            (block
             (maybe-code constructl-code))))
       ))) ;; end make-ctor

  ) ; end module

#|

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
