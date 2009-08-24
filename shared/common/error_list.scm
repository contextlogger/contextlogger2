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
;; such components, of which Valantine is only one.
(define domain-list
  (list
   "shared"
   "valantine"
   "valazilla"
   "cl2app"
   "symbian" ;; indicates that "code" will be a Symbian error code
   "posix"   ;; indicates that "code" will be a POSIX error code
   "lua"     ;; indicates that "code" will be a Lua C API error code
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
    ))

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
     
     ))) ; end program-1

(define* (main)
  (parameterize ((in-c-mode #t))
    ;;(pretty-nl program-1)
    ;;(dump-h-and-cpp program-1)
    (generate-h-and-cpp program-1)
    ;;(dump-analyzed-ast program-1)
    ;;(dump-compiled-ast program-1)
    (void)))
