#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (main)"
|#

#lang scheme

;; xxx Generate separate header for the integer codes only, to allow that to be included without GLib.

(define program-name (find-system-path 'run-file))

(require (lib "usual.scm" "wg"))
(require (lib "ast-util.scm" "wg"))
(require (lib "file-util.scm" "wg"))
(require (lib "compact.scm" "wg"))
(require (lib "node-ctors.scm" "wg"))
(require (lib "settings.scm" "wg"))
(require (lib "string-util.scm" "wg"))
(require (lib "local-util.scm" "codegen"))

;; List of domains. Each will get a dedicated GQuark.
;; 
;; We use these to point out the source code component in which the
;; offending code resides. We also share the same error list in many
;; such components.
(define domain-list
  (list
   "shared"
   "valantine"
   "valazilla"
   "cl2app"
   "symbian" ;; indicates that "code" will be a Symbian error code
   "posix"   ;; indicates that "code" will be a POSIX error code
   "lua"     ;; indicates that "code" will be a Lua C API error code
   "iksemel" ;; indicates that "code" will be an Iksemel C API error code
   "zlib"    ;; indicates that "code" will be a zlib error code
   "qt"
   ))

;; List of error codes. Each will get a dedicated gint.
(define code-list
  '(
    "dummy error"
    "unspecified error"
    "socket create"
    "socket set non blocking"
    "socket set reuse address"
    "socket bind"
    "socket listen"
    "socket select"
    "socket shutdown"
    "socket close"
    "socket accept"
    "c code leave"
    "cleanup stack create"
    "active scheduler create"
    "error in callback"
    "camera"
    "sensor start"
    "sensor no reading"
    "sensor read"
    "directory create"
    "file delete"
    "database open"
    "database close"
    "database command"
    "database state init"
    "time query"
    "timer"
    "no memory"
    "not supported"
    "not found"
    "type error"
    "not connected"
    "no configuration"
    ))

(define (capture-output f)
  (let ((output (open-output-string)))
    (parameterize ((current-output-port output))
      (f))
    (get-output-string output)))

(define (display-cpp-nl s)
  (display s)
  (display " \\")
  (newline))

(define program-1
  (unit
    (basename (path-drop-extension (path-basename program-name) ".scm"))

    (includes
     (system-include "glib.h")
     (local-include "common/utilities.h")
     )
    (body

     ;; We want this sort of thing
     ;;
     ;;   #define domain_x_y (domain_x_y_quark())
     ;;   GQuark domain_x_y_quark(void)
     ;;   {
     ;;     return g_quark_from_static_string("x-y");
     ;;   }
     (sc-list
      (map
       (lambda (s)
         (let* ((comps (string-split-space s))
                (comps-d (cons "domain" comps))
                (comps-dq (push comps "quark"))
                ;; could consider string-upcase here
                (def-name (string-join comps-d "_")) 
                (func-name (string-join comps-dq "_"))
                (domain-name (string-join comps "-")))
           (sc
            (cxx-exported-declarations
             (string-append
              "#define "
              def-name
              " ("
              func-name
              "())"))
            (func (name func-name) export
                  (verbatim-modifier "EXTERN_C")
                  (returns (type 'GQuark))
                  (block
                   (return (call 'g_quark_from_static_string
                                 (list (cstr domain-name)))))))))
       domain-list))

     (sc-list
      (map-with-index
       (lambda (i s)
         (let* ((comps (string-split-space s))
                (comps-c (cons "code" comps))
                (var-name (string-join comps-c "_"))
                (var-value (+ i 1)))
           ;; Using a variable is not appropriate, since in C the variable would be defined in every compilation unit including the header. This behavior differs from that of C++.
           ;;
           ;;(var export (name var-name) (type (const 'gint)) (init var-value))

           (cxx-exported-declarations
            (string-append
             "#define " var-name " " (to-s var-value)))
           ))
       code-list))

     (cxx-exported-declarations
      (capture-output
       (thunk
        (display-cpp-nl "#define preallocate_all_quarks {")
        (for-each
         (lambda (s)
           (let* ((comps (string-split-space s))
                  (domain-name (string-join comps "-")))
             (display-cpp-nl (format "domain_~a;" domain-name))))
         domain-list)
        (display "}")
        )))
     
     ))) ; end program-1

(define* (main)
  (parameterize ((in-c-mode #t))
    ;;(pretty-nl program-1)
    ;;(dump-h-and-cpp program-1)
    (generate-h-and-cpp program-1)
    ;;(dump-analyzed-ast program-1)
    ;;(dump-compiled-ast program-1)
    (void)))

#|

error_list.scm

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
