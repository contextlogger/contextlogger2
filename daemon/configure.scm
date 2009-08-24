#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (main) (void)" -- ${1+"$@"}
Yes, convoluted, but we want this file to be a module rathar than a script.
|#

#lang scheme

;; This is a script for configuring this project. The idea is simple,
;; you choose a single configuration (from variants/), passing its
;; name on the command line. This script will then proceed to generate
;; a bunch of include files that reflect the configuration. The
;; include files are only touched when they actually change as a
;; result of the change in configuration; this is to allow this script
;; to better work with build tools whose dependency checking is based
;; on timestamps.

(require common/usual-4)
(require konffaile/variant)
(require scheme/cmdline)
(require common/class)

(define configure-script (find-system-path 'run-file))

(define verbose? (make-parameter #t))
(define variant-name (make-parameter #f))

(command-line
 #:once-each
 (("-v" "--verbose") "be verbose"
  (verbose? #t))
 #:args (config_name) (variant-name config_name))

(define-syntax on-fail
  (syntax-rules ()
    ((_ fail-expr expr)
     (with-handlers
         ((exn:fail?
           (lambda (e) fail-expr)))
       expr))))

(define (file-read file)
  (call-with-input-file*
   file
   (lambda (in)
     (port->string in))))
     
;; Checks whether a file either does not exist or has been changed.
(define (file-changed? file s)
  ;; Would there be a good way to write a function for comparing two
  ;; input streams? Then we could handle large files as well. ((nin
  ;; (open-input-string s))) and then compare to file input.
  (on-fail #t (not (equal? (file-read file) s))))

;;(write-nl (file-changed? configure-script (file-read configure-script)))

(define (write-changed-file file s)
  (when (file-changed? file s)
    (call-with-output-file*
     file
     (lambda (out)
       (display s out))
     #:exists 'truncate/replace)
    (display-nl file)))

(define variant-dir (build-path "variants"))

(define (get-variant-basename varname)
  (format "~a.var.ss" varname))

(define (get-variant-file varname)
  (build-path variant-dir (get-variant-basename varname)))

(define variant-symlink-file (get-variant-file "current"))

(define (display-generated-notice pfx)
  (display pfx)
  (display-nl " generated -- do not edit"))

(define (capture-output f)
  (let ((output (open-output-string)))
    (parameterize ((current-output-port output))
      (f))
    (get-output-string output)))

(define-syntax capture
  (syntax-rules ()
    ((_ body ...)
     (capture-output (lambda () body ...)))))

(define (disp . args)
  (display (apply format args)))

(define (disp-nl . args)
  (apply disp args) (newline))

(define (write-variant-symlink varname)
  (define file variant-symlink-file)
  (let ((basename (get-variant-basename varname)))
    (write-changed-file
     file
     (capture
      (display-generated-notice ";;")
      (display-nl "#lang scheme")
      (disp-nl "(require ~s)" basename)
      (disp-nl "(provide (all-from-out ~s))" basename)))))

(define path-censor-re #rx"[-.]")

(define (path-h-ifdefy p)
  (string-append
   "__"
   (string-downcase
    (regexp-replace* path-censor-re (path->string (path-basename p)) "_"))
   "__"))

(define src-dir (build-path "src"))
(define c-file (build-path src-dir "current_config.hrh"))
(define gmake-file (build-path src-dir "current_config.mk"))
(define ruby-file (build-path src-dir "current_config.rb"))

(define ident-censor-re #rx"[-]")

(define (name-to-c sym)
  (string->symbol
   (string-append
    "__"
    (string-upcase
     (regexp-replace* ident-censor-re (symbol->string sym) "_"))
    "__")))

(define (name-to-ruby sym)
  (string->symbol
   (string-append
    "$"
    (string-upcase
     (regexp-replace* ident-censor-re (symbol->string sym) "_"))
    )))

(define (name-to-gmake sym)
  (string->symbol
   (string-append
    (string-upcase
     (regexp-replace* ident-censor-re (symbol->string sym) "_"))
    )))

(define (display-attr/c name value)
  (set! name (name-to-c name))
  (cond
   ((attr-undefined? value)
    (void))
   ((or (attr-defined? value) (eqv? value #t))
    (disp-nl "#define ~a 1" name))
   ((eqv? value #f)
    (disp-nl "#define ~a 0" name))
   ((number? value)
    (disp-nl "#define ~a ~s" name value))
   ((hexnum? value)
    (disp-nl "#define ~a 0x~a"
             name (number->string (hexnum-num value) 16)))
   ((string? value)
    (disp-nl "#define ~a ~s" name value))
   ))

(define (display-attr/ruby name value)
  (set! name (name-to-ruby name))
  (cond
   ((attr-undefined? value)
    (void))
   ((or (attr-defined? value) (eqv? value #t))
    (disp-nl "~a = true" name))
   ((eqv? value #f)
    (disp-nl "~a = false" name))
   ((number? value)
    (disp-nl "~a = ~s" name value))
   ((hexnum? value)
    (disp-nl "~a = 0x~a"
             name (number->string (hexnum-num value) 16)))
   ((string? value)
    (disp-nl "~a = ~s" name value))
   ))

(define (display-attr/gmake name value)
  (set! name (name-to-gmake name))
  (cond
   ((attr-undefined? value)
    (void))
   ((attr-defined? value)
    (disp-nl "~a := 1" name))
   ((eqv? value #t)
    (begin (disp-nl "~a := 1" name)
           (disp-nl "NOT__~a :=" name)))
   ((eqv? value #f)
    (begin (disp-nl "~a :=" name)
           (disp-nl "NOT__~a := 1" name)))
   ((number? value)
    (disp-nl "~a := ~s" name value))
   ((hexnum? value)
    (begin
      (disp-nl "~a__DEC := ~s" name (hexnum-num value))
      (disp-nl "~a__HEX := ~a"
               name (number->string (hexnum-num value) 16))))
   ((string? value)
    (disp-nl "~a := ~a" name value))
   ))

(define (write-c-file attrs)
  (let ((file c-file)
        (harness-name (path-h-ifdefy c-file)))
    (write-changed-file
     file
     (capture
      (display-generated-notice "//")
      (disp-nl "#ifndef ~a" harness-name)
      (disp-nl "#define ~a" harness-name)
      (for-each
       (lambda (entry)
         (let ((name (first entry))
               (value (second entry)))
           (display-attr/c name value)
           ))
       attrs)
      (disp-nl "#endif // ~a" harness-name)
      ))))

(define (write-ruby-file attrs)
  (let ((file ruby-file))
    (write-changed-file
     file
     (capture
      (display-generated-notice "#")
      (for-each
       (lambda (entry)
         (let ((name (first entry))
               (value (second entry)))
           (display-attr/ruby name value)
           ))
       attrs)
      ))))

(define (write-gmake-file attrs)
  (let ((file gmake-file))
    (write-changed-file
     file
     (capture
      (display-generated-notice "#")
      (for-each
       (lambda (entry)
         (let ((name (first entry))
               (value (second entry)))
           (display-attr/gmake name value)
           ))
       attrs)
      ))))

(define* (main)
  (let* ((varname (variant-name))
         (varfile (get-variant-file varname))
         (varinfo-f (dynamic-require (path->string varfile) 'info))
         (varinfo (varinfo-f))
         (set-name (class-field-mutator variant% name)))
    (set-name varinfo (string->symbol varname))

    ;;(write-nl (get-fields/hasheq varinfo))
    ;;(write-nl (interface->method-names (object-interface varinfo)))
    ;;(write-nl (variant-attr-names varinfo))
    ;;(write-nl (variant-attr-values/hasheq varinfo))
    ;;(write-nl (variant-attr-values/sorted varinfo))
    (let ((attrs (variant-attr-values/sorted varinfo)))
      ;;(pretty-nl attrs)
      (write-c-file attrs)
      (write-ruby-file attrs)
      (write-gmake-file attrs))

    (write-variant-symlink varname)))
