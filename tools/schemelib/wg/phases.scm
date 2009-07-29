;; 
;; phases.scm
;; 
;; Copyright 2007 Helsinki Institute for Information Technology (HIIT)
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

;; We refer to registered compiler passes as "phases". Routines for
;; maintaining and using the registry are in this model.
(module
  phases
  mzscheme

  (require (lib "usual.scm" "wg"))
  (require (lib "ast-util.scm" "wg"))
  (require (lib "tables.scm" "wg"))

  (define phase-registry (make-hash-table))

  ;; The "type" argument specifies both the input and output interface
  ;; type of the function that implements the pass. This information
  ;; is important for correctly combining passes that are not directly
  ;; compatible.
  (define/kw* (phase-add name function
                         #:key (type '(ast-tables ast-tables)))
    (phase-add-entry name (phase-new type function)))

  (define* (phase-add-entry name entry)
    (hash-table-put! phase-registry name entry))
  
  ;; Returns an entry of the form (type function), or #f.
  (define (phase-get-entry name)
    (hash-table-get phase-registry name))

  (define* (phase-get-reqd-entry name)
    (aif entry (phase-get-entry name)
         entry
         (error "no such phase" name)))

  (define* phase-type first)
  
  (define* phase-function second)

  (define* (phase-get-pass name)
    (phase-function (phase-get-reqd-entry name)))
  
  (define* (phase-new type function)
    (list type function))
  
 ;; Turns an 1-to-1 pass into an n-to-n pass. Tables may be modified,
 ;; but a separate copy of tables is maintained for each of the n
 ;; subpasses. The same set of options is passed to each subpass.
 (define (make-nn-pass pass)
   (lambda (args . op)
     (map
      (lambda (ast-tables)
        (let ((ast (first ast-tables))
              (tables (second ast-tables)))
          (apply pass ast tables op)))
      args)))

  (define* phase-compose
    (case-lambda
      ((p) p)
      ((p1 p2 . rest)
       (let* ((type1 (phase-type p1))
              (type2 (phase-type p2))
              (in-t (first type1))
              (out-t (second type2))
              (t1 (second type1))
              (t2 (first type2))
              (f1 (phase-function p1))
              (f2 (phase-function p2))
              (new-p
               (cond
                ((and (eq? t1 'ast-tables) (eq? t2 'ast-tables))
                 (phase-new (list in-t out-t) (compose-carry f1 f2)))
                ((and (eq? t1 'pair-list) (eq? t2 'pair-list))
                 (phase-new (list in-t out-t) (compose-r f1 f2)))
                ((and (eq? t1 'ast-tables) (eq? t2 'pair-list))
                 (phase-new (list in-t out-t)
                            (lambda (ast tables . op)
                              (alet res (apply f1 ast tables op)
                                    (f1 (list res))))))
                ((and (eq? t1 'pair-list) (eq? t2 'ast-tables))
                 (phase-new (list in-t 'pair-list)
                            (compose-r f1 (make-nn-pass f2))))
                (else
                 (error "cannot compose phases" (list t1 t2))))))
         (apply phase-compose new-p rest)))))

  (define* (phase-fix-op phase-entry phase-op)
    (let ((type (phase-type phase-entry))
          (func (phase-function phase-entry)))
      (phase-new type (fixr func phase-op))))
    
  (define* (make-phase-for-phases . phases)
    (apply
     phase-compose
     (map 
      (lambda (phase-info)
        (when (symbol? phase-info)
          (set! phase-info (list phase-info)))
        (let* ((phase-name (car phase-info))
               (phase-op (cdr phase-info))
               (phase-entry (phase-get-reqd-entry phase-name)))
          (if (null? phase-op)
              phase-entry
              (phase-fix-op phase-entry phase-op))))
      phases)))

  (define* (make-pass-for-phases . phases)
    (phase-function (apply make-phase-for-phases phases)))
  
  ) ; end module
