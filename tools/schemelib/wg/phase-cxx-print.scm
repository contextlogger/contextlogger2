;; 
;; phase-cxx-print.scm
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

(module
  phase-cxx-print
  mzscheme

  (require (lib "usual.scm" "wg"))
  (require (lib "ast-util.scm" "wg"))
  (require (lib "phases.scm" "wg"))
  (require (lib "tables.scm" "wg"))
  (require (prefix pp. "pp-simple.scm"))
  (require (lib "code-print.scm" "wg"))
  (require (lib "settings.scm" "wg"))

  (define/kw* (base-print
               ast
               #:key
               step ;; indentation step in number of characters
               banners? ;; whether to print banners
               file? ;; whether to print into a file
               force? ;; truncate existing file
               basename ;; basename for output file
               ext ;; extension for output file
               port ;; override port to use for output
               filename ;; filename for output file (overrides basename)
               path ;; path for relative files
               banner-name ;; default banner name
               add-eof? ;; append extra newline to non-empty output
               #:allow-other-keys)
    (define (from-basename basename)
      (string-append
       basename
       (if ext ext (error "no extension to attach to basename"))))
    
    (define need-filename? (and file? (not port)))

    (define (to-banner-name filename)
      (if filename
          (call-with-values
              (fix split-path filename)
            (lambda (base last-comp must-be-dir?)
              (when must-be-dir?
                (error "filename indicates a directory"))
              (path->string last-comp)))
          (or banner-name "(output file)")))
    
    (define (call-with-arg arg f)
      (f arg))

    (when (or banners? need-filename?)
      (cond
       (filename void)
       (basename (set! filename (from-basename basename)))
       (else
        (when need-filename?
          (error "cannot determine filename"))))
      (when filename
        (unless (path? filename)
          (set! filename (string->path filename)))
        (unless (absolute-path? filename)
          (if path
              (set! filename (path->complete-path filename path))
              (when need-filename?
                (error "cannot determine output directory" filename))))))

    ((cond
      (port (fix call-with-arg port))
      (file? (lambda (f)
               (call-with-output-file
                   filename f
                   (if force? 'truncate 'error))))
      (else (fix call-with-arg (current-output-port))))
     (lambda (out)
       (let ((pp (pp.new #:step step #:port out)))
         (when banners?
           (print-banner pp (to-banner-name filename)))
         (print-model pp ast)
         (when (and (or banners? add-eof?)
                    (model-matters? ast))
           (pp.nl pp))))))

  (define/kw (pass-cxx-print
              ast
              tables
              #:key
              filename
              basename
              (step 2)
              #:other-keys op)
    (define banner-name "(C++ output file)")
    (apply base-print ast
           #:ext (if (in-c-mode) ".c" ".cpp")
           #:banner-name banner-name
           #:step step
           #:filename (or filename (tables-get-opt tables 'filename))
           #:basename (or basename (tables-get-opt tables 'basename))
           op)
    (list ast tables))
  
  (phase-add 'cxx-print pass-cxx-print)
  
  ) ; end module

#;
(begin
  (require (lib "usual.scm" "wg"))
  (require (lib "phases.scm" "wg"))
  (require phase-cxx-print)

  ;;(write-nl (path->complete-path (string->path "hello.cpp") "/tmp"))

  (define pass
    (make-pass-for-phases
     '(cxx-print #:banners? #t #:basename "hello")))
  (define tables #f)
  
  (pass '(sep nl (text "hello") (text "world")) tables)
     
;;   (backend-print 'cxx '(sep nl (text "hello") (text "world"))
;;                  #:add-eof? #t #:banners? #t #:basename "hello"
;;                  #:filename "hello.cpp" #:path "/tmp"
;;                  #:file? #t)

  ) ; end test code
