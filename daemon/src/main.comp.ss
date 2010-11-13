#lang scheme

(require (lib "usual-4.ss" "common"))
(require konffaile/class-attr)
(require konffaile/component)
(require konffaile/variant)

(define (string-split-space s)
  (regexp-split #rx"[ \t]+" s))

(define* (info)
  (define var (current-variant))

  (define (attr x) (object-get-attr var x))

  (define flatten-compact (compose compact flatten))

  (define platform (send var platform))
  
  (define on-symbian (eq? platform 'symbian))
  
  (define (symbian-list . x)
    (if on-symbian (flatten-compact x) '()))

  (define (compact-list . x) (compact x))

  (new abld-component%
       (deps (compact-list
              (and on-symbian
                   (attr 'smsevent-enabled)
                   'epoc-smsevent)
              ))
       (mmp-libs
        (symbian-list
         "apgrfx.lib"
         "apparc.lib"
         "avkon.lib"
         "bafl.lib"
         "bluetooth.lib"
         "btmanclient.lib"
         "charconv.lib"
         "commdb.lib"
         "commonui.lib" ;; CErrorUI
         "cone.lib"
         "efsrv.lib"
         "eikcoctl.lib"
         "eikcore.lib"
         "esock.lib"
         "etel3rdparty.lib" ;; CTelephony
         "http.lib"
         "inetprotutil.lib" ;; URI parsers
         "insock.lib" ;; Internet protocol support for esock
         "lbs.lib"
         "ws32.lib"
         ))
       ))

#|

main.comp.ss

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
