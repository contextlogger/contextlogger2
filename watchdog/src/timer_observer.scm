#!/bin/sh
#| # -*- scheme -*-
exec mzscheme --name "$0" --eval "(require scheme (lib \"usual-4.ss\" \"common\") (file \"$0\")) (current-load-relative-directory (path-dirname (path->complete-path \"$0\"))) (main)"
|#

#lang scheme

;;
;; Copyright 2008 Helsinki Institute for Information Technology (HIIT)
;; and Tero Hasu <tero.hasu@hut.fi>. All rights reserved.
;;
;; This license applies:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; Alternatively, this license applies:
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

(require (lib "usual.scm" "wg"))
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
     (class
       (name 'MTimerObserver)
       export
       (doc "A callback interface for CTimerAo.")
       (body
        (func
         (name 'HandleTimerEvent)
         (args (arg (name 'aError) (type 'TInt)))
         public pure virtual)))

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
                     (list (call-on 'iStatus 'Int)))))

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
