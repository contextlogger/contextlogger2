#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (main)"
|#

#lang scheme

;; CTimer objects are quite useful, and I believe we shall want a
;; language construct for defining subclasses. Or we might want to
;; create a concrete subclass of CTimer that is fairly general
;; purpose, and just use that one across the board. Indeed, here we
;; define just such a general purpose CTimer class.

(require (lib "usual-4.ss" "common"))
(require (lib "ast-util.scm" "wg"))
(require (lib "compact.scm" "wg"))
(require (lib "file-util.scm" "wg"))
(require (lib "node-ctors.scm" "wg"))
(require (lib "local-util.scm" "codegen"))
(require (lib "active-object.scm" "codegen"))

(define program-name (find-system-path 'run-file))
(define program-basename (path-drop-extension (path-basename program-name) ".scm"))

(define program-1
  (unit
    (basename program-basename)

    (includes (system-include "e32base.h"))

    (body
     (cxx-iface-line "class CTimerAo;")
     
     (class
       (name 'MTimerObserver)
       export
       (doc "A callback interface for CTimerAo.")
       (body
        (func
         (name 'HandleTimerEvent)
         (args
          (arg (name 'aOrig) (type (ptr-to 'CTimerAo)))
          (arg (name 'aError) (type 'TInt)))
         public pure virtual)))

     ;; We might want a construct for defining the construction
     ;; methods all in one go. Some kind of a sexp-based
     ;; specification could be given as an argument to it, for
     ;; parameterization.
     (let* ((class-name 'CTimerAo)
            (class-args
             (args (arg (name 'aInterface) (type (ref-to 'MTimerObserver)))
                   (arg (name 'aPriority) (type 'TInt))))
            (args-names
             (map
              ;; We might want to consider using the views library for
              ;; some extra abstraction when dealing with our exposed
              ;; AST nodes.
              (lambda (arg) (fget-reqd-nlist-elem-1 arg 'name))
              (cdr class-args))))
       (class
         (name class-name)
         (bases 'CTimer)
         export
         (doc "A fairly generic, concrete CTimer subclass that delivers timer events via a callback interface.")
         (body

          (func
           (name 'NewLC)
           public static leaving
           class-args
           (returns (type (ptr-to class-name)))
           (block
            (var (name 'object)
                 (type (ptr-to class-name))
                 (init (leaving-new class-name args-names)))
            (call 'CleanupStack::PushL (list 'object))
            (call-via 'object 'ConstructL)
            (return 'object)))

          (func
           (name 'NewL)
           public static leaving
           class-args
           (returns (type (ptr-to class-name)))
           (block
            (var (name 'object)
                 (type (ptr-to class-name))
                 (init (call 'NewLC args-names)))
            (call 'CleanupStack::Pop)
            (return 'object)))

          (ctor
           private
           class-args
           (ctor-init-list
            (ctor-super 'CTimer '(aPriority))
            (ctor-var 'iInterface '(aInterface)))
           (block
            ;; Apparently the superclass constructor does not do this.
            ao-scheduler-add-this))

          (func
           (name 'RunL)
           private virtual leaving
           (block
            (call-on 'iInterface 'HandleTimerEvent
                     (list 'this (call-on 'iStatus 'Int)))))

          ;; Must implement RunError if HandleTimerEvent may leave. As
          ;; per the current naming, we do not allow that. We could
          ;; have another variant of this class allowing for it, I
          ;; guess, and letting a RunError handler also be defined in
          ;; the interface.

          (var
           (name 'iInterface)
           (type (ref-to 'MTimerObserver))
           private)
          )))
     ))) ; end program-1

(define* (main)
  ;;(pretty-nl program-1)
  (generate-h-and-cpp program-1)
  ;;(dump-h-and-cpp program-1)
  ;;(dump-analyzed-ast program-1)
  ;;(dump-compiled-ast program-1)
  (void))
