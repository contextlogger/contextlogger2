#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (main)"
|#

;; Note that this file can both be required as a module and executed as a program.

#lang scheme

(require (lib "usual-4.ss" "common"))

(require (lib "ast-util.scm" "wg"))
(require (lib "file-util.scm" "wg"))
(require (lib "compact.scm" "wg"))
(require (lib "cxx-syntax.ss" "wg"))
(require (lib "emission.scm" "wg")) ;; var-type-string
(require (lib "settings.scm" "wg")) ;; for in-c-mode
(require (lib "string-util.scm" "wg"))
(require (lib "local-util.scm" "codegen"))

(define (capture-output f)
  (let ((output (open-output-string)))
    (parameterize ((current-output-port output))
      (f))
    (get-output-string output)))

(define (display-nl-bs x)
  (display-nl (string-append x " \\")))

(define (display-f . x)
  (display (apply format x)))

(define (display-f-nl . x)
  (display-nl (apply format x)))

(define (display-f-nl-bs fmt . x)
  (display-nl (apply format (string-append fmt " \\") x)))

;; Scribble might be handy, say for saying something like @t{some code
;; here}, but it does tend to confuse Emacs.
(define (t x) x)
(require scribble/reader)
(use-at-readtable)

;; --------------------------------------------------
;; specify the config parameter list spec here
;; --------------------------------------------------

(define (validate-ascii-ident-string n vn)
  (display-f-nl-bs "if (!is_ascii_ident(~a)) return_with_error(\"value '~a' is not a valid ident string\");" vn n))

;; Poor man's string validation.
(define (validate-non-empty-string n vn)
  (display-f-nl-bs "if (!*~a) return_with_error(\"value '~a' may not be an empty string\");" vn n))

;; Each entry is of the form (name type validator-func), where
;; validator-func may be #f.
(define config-param-list
  `(
    (username string ,validate-ascii-ident-string)
    (upload_url string ,validate-non-empty-string)
    (remokon_host string ,validate-non-empty-string)
    (remokon_port integer #f)
    (remokon_password string ,validate-non-empty-string)
    (jid string ,validate-non-empty-string)
    (iap string ,validate-non-empty-string)
    (log_disk_threshold integer #f)
    ))

;; --------------------------------------------------
;; 
;; --------------------------------------------------

(define (c-return-type t)
  (case-eq t
           ('string (ptr-to 'gchar))
           ('integer 'gint)
           (else (error "unsupported" t))))

(define-syntax for-each-param
  (syntax-rules ()
    ((_ (n t v) body ...)
     (for-each
      (lambda (entry)
        (let-values (((n t v) (apply values entry)))
          body ...))
      config-param-list))))

(define-syntax map-param
  (syntax-rules ()
    ((_ (n t v) body ...)
     (map
      (lambda (entry)
        (let-values (((n t v) (apply values entry)))
          body ...))
      config-param-list))))

(define (make-cleanup-all)
  (define (f)
    (display "#define CLEANUP_ALL {")
    (for-each-param
     (n t v)
     (case-eq t
              ('string
               (begin
                 (display-nl " \\")
                 (display-f "g_free(self->~a);" n)))
              ))
    (display-nl " \\")
    (display "}"))
  (cxx-exported-declarations (capture-output f)))

(define (make-declare-state-all)
  (define (f)
    (display "#define DECLARE_STATE_ALL")
    (for-each-param
     (n t v)
     (display-nl " \\")
     (display-f "~a;" (var-type-string (var (name n) (type (c-return-type t)))))))
  (cxx-exported-declarations (capture-output f)))

#|
  {
    lua_getfield(L, -1, "username");
    if (!lua_isnil(L, -1)) {
      const char* s = lua_tostring(L, -1);
      if (!s) return_with_error("value 'username' is not a string");
      if (!is_ascii_ident(s)) return_with_error("value 'username' is not a valid ident string");
      self->username = strdup(s);
      if (!self->username) return_with_oom;
    }
    lua_pop(L, 1);
  }
|#

;; Note that here we assume that any required #includes are still
;; manually inserted.
(define (make-state-init-all)
  (define (f)
    (display "#define STATE_INIT_ALL")
    (for-each-param
     (n t v)
     (display-nl " \\")
     (display-nl "{ \\")
     (display-f-nl "  lua_getfield(L, -1, \"~a\"); \\" n)
     (display-nl "  if (!lua_isnil(L, -1)) { \\")

     (case-eq
      t

      ('string
       (begin
         (display-nl "const char* _value = lua_tostring(L, -1); \\")
         (display-f-nl "if (!_value) return_with_error(\"value '~a' is not a string\"); \\" n)
         (when v (v n '_value))
         (display-f-nl "self->~a = strdup(_value); \\" n)
         (display-f-nl "if (!self->~a) return_with_oom; \\" n)
         ))

      ('integer
       (begin
         (display-f-nl "if (!lua_isnumber(L, -1)) return_with_error(\"value '~a' is not an integer\"); \\" n)
         ;; This produces 0 if not a number, and truncates if a non-integer number.
         (display-nl "gint _value = lua_tointeger(L, -1); \\")
         (when v (v n '_value))
         (display-f-nl "self->~a = _value; \\" n)
         ))
              
      (else (error "unsupported" t))
      ) ;; end type case
     
     (display-nl "  } \\")
     (display-nl "  lua_pop(L, 1); \\")
     (display "}")))
  (cxx-exported-declarations (capture-output f)))

(define (make-interface-all)
  (sc-list
   (map-param
    (n t v)
    (func (name (format "cf_RcFile_get_~a" n)) 
          cexport ;;extern-c
          (returns (type (c-return-type t)))
          (args (arg (name 'self) (type (ptr-to 'cf_RcFile))))
          (block
           (return (field-via 'self n)))))))

(define program-public
  (cunit
   (basename "cf_rcfile_list")
   (body

    ;; Interface.
    (make-interface-all)

    )))

(define program-private
  (cunit
   (basename "cf_rcfile_list_private")
   (body
    
    ;; State.
    (make-declare-state-all)

    ;; State init and validation.
    (make-state-init-all)
    
    ;; State cleanup.
    (make-cleanup-all)

    )))

(define (generate dump? gen?)
  (let ((do-program
         (lambda (ast)
           ;;(pretty-nl ast)
           (when dump? (dump-h-and-cpp ast))
           (when gen? (generate-h-and-cpp ast))
           ;;(dump-analyzed-ast ast)
           ;;(dump-compiled-ast ast)
           )))
    (parameterize ((in-c-mode #t))
      (do-program program-public)
      (do-program program-private))))
  
(define* (main)
  (let ((dump? #f)
        (gen? #t))
    (generate dump? gen?)
    (void)))

#|

cf_rcfile_list_spec.ss

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
