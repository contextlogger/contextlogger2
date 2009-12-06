#lang scheme

#|

Convenience functions for dealing with scheme/class style OO.

Not sure if we could get access to more of the class-internal.ss
functions by somehow unprotecting the module. Some of the methods
there seem to be provided as protect-out. See
namespace-unprotect-module.

|#

(require (lib "usual-4.ss" "common"))

(define* (has-method? object method-name)
  (method-in-interface? method-name (object-interface object)))

(define-syntax* respond-to?
  (syntax-rules ()
    ((_ obj meth)
     (has-method? obj (quote meth)))))

;; This trick for invoking a method by name (a symbol, not literal)
;; comes from:
;; http://list.cs.brown.edu/pipermail/plt-scheme/2007-February/016557.html
;; Only works for methods without arguments.
(define* (call-method object method-name)
  (define f (make-generic (object-interface object) method-name))
  (send-generic object f))

;; Just about all the field access facilities appear to be macros, and
;; it is hardly obvious how to access a field when its name is in a
;; variable, due to the degree of encapsulation in the class
;; implementation (both the selective exports and the inspection
;; system). This is an attempt to implement something like that.
;; 
;; This implementation works, but only for fields in classes whose
;; inspectors allow it. This should also work where there is
;; inheritance.
;; 
;; Discussion about this topic can be found at:
;; 
;; http://www.cs.brown.edu/pipermail/plt-scheme/2007-March/017073.html
(define* (get-fields/hasheq object)
  ;;(write-nl object)
  (let*-values (((class skipped) (object-info object))
                ((res) (make-hasheq)))
    (let loop ((class class))
      (let*-values (((a cnt names get e super g) (class-info class)))
        ;;(write-nl class)
        ;;(write-nl names)
        ;; Only the first "cnt" of the fields are in this class, while
        ;; the rest are inherited.
        (let ((this-names (drop-right names (- (length names) cnt))))
          ;;(write-nl this-names)
          (let ((i 0))
            (for-each
             (lambda (name)
               (unless (hash-ref res name #f)
                 (let ((val (get object i)))
                   (hash-set! res name val)
                   (set! i (+ i 1)))))
             this-names)))
        (when super (loop super))))
    res))

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
