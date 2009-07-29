;; 
;; settings.scm
;; 
;; Copyright 2006 Helsinki Institute for Information Technology (HIIT)
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
 settings
 mzscheme 

 (require (lib "usual.scm" "wg"))

 (define global-op #f)

 ;; Initializes the settings from the environment, if this has not
 ;; already been done. A potential alternative source we could use
 ;; would be yielded by current-command-line-arguments.
 (define (ensure-gop)
   (unless 
    global-op
    (let ((ev (getenv "WG_SCHEME_SETTINGS")))
      (if ev
	  (set! global-op (eval (read (open-input-string ev))))
	  (set! global-op '())))))

 (define* (gop-add-entry key value)
   (ensure-gop)
   (set! global-op (append global-op (list (list key value)))))

 (define* (gop-entry key)
   (ensure-gop)
   (assq key global-op))

 (define* (gop-has-entry? key)
   (true? (gop-entry key)))

;  (define* (gop-value key)
;    (let ((entry (gop-entry key)))
;      (if entry
; 	 (cadr entry)
; 	 #f)))

 (define* gop-value
   (let ((f
	  (lambda (key defval)
	    (let ((entry (gop-entry key)))
	      (if entry
		  (cadr entry)
		  defval)))))
     (case-lambda
      ((key) (f key #f))
      ((key defval) (f key defval)))))

 (define* (gop-not-value key)
   (not (gop-value key)))

 (define* (gop-true? key)
   (true? (gop-value key #f)))

 (define* (gop-false? key)
   (false? (gop-value key #f)))

 (define* (gop-entries)
   (ensure-gop)
   global-op)

 ;; Some environments are not set in environment variables, but rather
 ;; in the Scheme dynamic environment. Only the ones with fairly global
 ;; effect are defined here.
 
 (define* in-c-mode (make-parameter #f))

 )
