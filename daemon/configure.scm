#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (main) (void)" -- ${1+"$@"}
Yes, convoluted, but we want this file to be a module rather than a script.
|#

#lang scheme

;; INTRODUCTION
;; 
;; This is a script for configuring this project. The idea is simple:
;; you choose a single configuration (from variants/), passing its
;; name on the command line. This script will then proceed to generate
;; a bunch of include files that reflect the configuration. The
;; include files are only touched when they actually change as a
;; result of the change in configuration; this is to allow this script
;; to better work with build tools whose dependency checking is based
;; on timestamps.

(require common/usual-4)
(require konffaile/class-attr)
(require konffaile/component)
(require konffaile/variant)
(require konffaile/writer)
(require scheme/cmdline)
(require common/class)

(define configure-script (find-system-path 'run-file))

(define verbose? (make-parameter #t))
(define variant-name (make-parameter #f))

;; --------------------------------------------------
;; variant management
;; --------------------------------------------------

(define variant-dir (build-path "variants"))

(define (get-variant-basename varname)
  (format "~a.var.ss" varname))

(define (get-variant-file varname)
  (build-path variant-dir (get-variant-basename varname)))

(define variant-symlink-file (get-variant-file "current"))

(define (write-variant-symlink varname)
  (define file variant-symlink-file)
  (let ((basename (get-variant-basename varname)))
    (write-scheme-symlink file basename)))

(define src-dir (build-path "src"))

(define c-config-file (build-path src-dir "current_config.hrh"))
(define gmake-config-file (build-path src-dir "current_config.mk"))
(define ruby-config-file (build-path src-dir "current_config.rb"))

(define (write-variant-config varinfo)
  (let ((attrs (object-attr-values/sorted varinfo)))
    ;;(pretty-nl attrs)
    (write-c-file c-config-file attrs)
    (write-ruby-file ruby-config-file attrs)
    (write-gmake-file gmake-config-file attrs)))

;; --------------------------------------------------
;; main
;; --------------------------------------------------

(define* (main)
  (command-line
   #:once-each
   (("-v" "--verbose") "be verbose"
    (verbose? #t)) ;; xxx currently unused
   #:args (config_name) (variant-name config_name))

  (let* ((varname (variant-name))
         (varfile (get-variant-file varname))
         (varinfo-f (dynamic-require (path->string varfile) 'info))
         (varinfo (varinfo-f))
         (set-name (class-field-mutator variant% name)))
    (set-name varinfo (string->symbol varname))

    (write-variant-config varinfo)
    (write-variant-symlink varname)

    (void)))

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
