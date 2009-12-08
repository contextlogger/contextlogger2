#lang scheme

#|

This file defines the API that a component specification must
implement. It largely depends on the build tools used what information
a component specification must contain, and hence what interface it is
expected to implement. Components that are to support multiple
different build tools should implement multiple different interfaces
as appropriate, so that they can present the correct "face" in
tool-specific contexts.

A component specification may be named or anonymous. If a spec is
given as a symbol, an attempt is made to search for a component
specification file by that name from the search path to get the actual
specification. Any non-symbol value must be a specification object.

A component specification may depend on the current variant, and if
so, the variant specification must be provided via
the "current-variant" parameter (defined in variant.ss).

Any an all relative paths in a component specification must be
interpreted relative to the component specification file where
appropriate. For anonymous component specs a default path is used, and
this path may be overridden using parameterize.

|#

(require common/usual-4)
(require common/class)
(require "class-attr.ss")
(require "util.ss")
(require "variant.ss")

;; --------------------------------------------------
;; parameters
;; --------------------------------------------------

;; Component file search path. Paths earlier (on the left) on the
;; search path have priority when there are multiple matches. The
;; function specified by the ignore-comp-file? parameter is used to
;; ignore control files. Only used when resolving a component name to
;; a component description file. For non-system paths, relative paths
;; are generally preferable for portability of generated files.
(define* component-search-path
  (make-parameter (list (build-path "src"))))

;; Relative paths are interpreted relative to this directory by
;; default, unless some other fixpoint is available. If a component
;; has a component specification file, its paths are interpreted
;; relative to that file.
(define* default-path
  (make-parameter (build-path 'same))) ;; (current-directory)

(define (make-re-path-pred re)
  (lambda (p)
    (true? (regexp-match re (path->string p)))))

(define ignore-re #rx"(?:/|^)(?:[.]bzr|[.]git|[.]hg|[.]svn|[.]svk|_darcs|CVS|local|aside|retired)(?:/|$)")

(define* ignore-comp-file?
  (make-parameter (make-re-path-pred ignore-re)))

(define (expand-path p)
  (simplify-path (expand-user-path p) #f))

;; This function is used to "adjust" any parameterized paths.
(define (expand-dir-path p)
  (path->directory-path (expand-path p)))

;; --------------------------------------------------
;; generic component interface
;; --------------------------------------------------

;; A minimal component interface. Required to support component
;; dependency resolution, and querying of information about the
;; component.
;; 
;; If you want to add public attributes to the component, such that
;; those attributes should be emitted, use the attributes as
;; implemented by the class-attr module.
(define* component$
  (interface ()
    ;; List of components this one depends on.
    get-deps ;; list(symbol or component-interface)

    ;; A set of attributes that may be used for book-keeping.
    ;; 
    ;; The 'name value has the name of the component, if any.
    ;; 
    ;; The 'home value has the home path of the component, if any.
    ;; This is as it appears in the component search path.
    get-annots ;; hasheq(symbol, anything)
    ))

(define (comp-set-annot! comp key value)
  (hash-set! (send comp get-annots) key value))

(define (comp-get-annot/opt comp key)
  (hash-ref (send comp get-annots) key #f))

(define-syntax* component-class
  (syntax-rules ()
    ((_ super (iface ...) body ...)
     (class* super (component$ iface ...)
       (inspect #f)
       body ...))
    ))

(define-syntax* define-component
  (syntax-rules ()
    ((_ name super ifaces body ...)
     (define name
       (component-class super ifaces body ...)))))

(define-syntax* define-component*
  (syntax-rules ()
    ((_ name rest ...)
     (begin
       (define-component name rest ...)
       (provide name)))))

(define-component* component% object% ()
  (init-field
   (deps '())
   (annots (make-hasheq)))
  
  (super-new)

  (define/public (get-deps) deps)
  
  (define/public (get-annots) annots)
)

;; --------------------------------------------------
;; component name resolution
;; --------------------------------------------------

(define (basename-by-name name)
  (string->path (string-append (symbol->string name) ".comp.ss")))

(define component-re #rx"(.*)[.]comp[.]ss$")

(define (path=? p1 p2)
  (string=? (path->string p1) (path->string p2)))

(define (path-uniq lst)
  (remove-duplicates lst path=?))

;; First and foremost, component spec files are looked for relative to
;; the directory of the referring file, if any (may be given as #f).
;; This function throws an exception if a result is not found.
(define* (spec-paths-by-name referrer-dir name)
  (let ((sp (component-search-path))
        (basename (basename-by-name name))
        (ignore? (ignore-comp-file?)))
    (when referrer-dir
      (set! sp (cons referrer-dir sp)))
    (set! sp (map expand-dir-path sp))
    (set! sp (path-uniq sp))
    ;;(write-nl sp)
    (let ((res
           (first-true-result
            (lambda (dn)
              (let ((fn (build-path dn basename)))
                ;;(write-nl fn)
                ;;(write-nl (file-exists? fn))
                ;;(write-nl (not (ignore? fn)))
                (and (file-exists? fn)
                     (not (ignore? fn))
                     ;; This path is already normalized due to the
                     ;; way it is constructed.
                     (list dn fn))))
            sp)))
      (unless res
        (error "not found on search path" basename))
      res)))
  
(define* (make-name-resolver)
  ;; We use memoization to make it possible to compare all loaded
  ;; components by reference. And we can probably assume that any
  ;; anonymous components are unique. In any case, no harm if not, as
  ;; this is such an optimization.
  (define cache (make-hash))

  (define (memoize! pn load)
    (aif comp (hash-ref cache pn #f)
         comp
         (alet comp (load)
               (hash-set! cache pn comp)
               comp)))
  
  (define (component-by-name referrer-dir name)
    (let* ((res (spec-paths-by-name referrer-dir name))
           (spec-dir (first res))
           (spec-file (second res)))
      (memoize! spec-file
                (thunk
                 (alet comp ((dynamic-require (path->string spec-file) 'info))
                       (comp-set-annot! comp 'name name)
                       (comp-set-annot! comp 'home spec-dir)
                       (comp-set-annot! comp 'file spec-file)
                       comp)))))

  component-by-name)

;; --------------------------------------------------
;; component dependency resolution
;; --------------------------------------------------

;; Resolves the dependencies of the root component "rcomp"
;; (inclusive).
(define* (resolve-deps rcomp)
  (define resolve (make-name-resolver))
  (define comps (new-Set))

  (define (f parent-home x)
    (define comp (if (symbol? x)
                     (resolve parent-home x)
                     x))
    (define home (comp-get-annot/opt comp 'home))
    ;;(write-nl comp)
    (Set-put! comps comp)
    (let ((deps (send comp get-deps)))
      ;;(write-nl deps)
      (for-each (lambda (dep)
                  (f home dep))
                deps)))
  
  (f #f rcomp)
  (Set-values comps))

;; --------------------------------------------------
;; attribute combination function registry
;; --------------------------------------------------

(define* (string-uniq lst)
  (remove-duplicates lst string=?))

(define* (string-sort lst)
  (sort lst string<?))

(define* (string-uniq-sort lst)
  (string-sort (string-uniq lst)))

(define registry (make-hasheq))

(define* (reg-attr-combinator name f)
  (hash-set! registry name f))

(define* (get-attr-combinator name)
  (hash-ref registry name))

(define* (attr-combine/string-list lst)
  (string-uniq-sort (apply append lst)))

(define* (comps-attr-combinator-list comps)
  (let ((attr-set (new-Set)))
    (for-each
     (lambda (comp)
       (let ((names (object-attr-names comp)))
         (for-each
          (lambda (name) (Set-put! attr-set name))
          names)))
     comps)
    (let ((attr-names (sort (Set-values attr-set) symbol<?)))
      (map
       (lambda (name)
         (list name (get-attr-combinator name)))
       attr-names))))

(define* (comps-combine-attrs comps)
  (map
   (lambda (rec)
     (let ((name (first rec))
           (combine (second rec))
           (res '()))
       (for-each
        (lambda (comp)
          (let/ec esc
            (set! res (cons
                       (object-get-attr comp name esc)
                       res))))
        comps)
       (list name (combine res))
       ))
   (comps-attr-combinator-list comps)))

;; --------------------------------------------------
;; ABLD specific
;; --------------------------------------------------

(define* abld$
  (interface ()
    mmp-libraries.attr
    ))

(define-component* abld-component% component% (abld$)
  (init-field
   (mmp-libs '()))
  
  (super-new)

  (define/public (mmp-libraries.attr) mmp-libs)
  )

(reg-attr-combinator 'mmp-libraries attr-combine/string-list)

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
