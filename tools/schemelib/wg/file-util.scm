;; 
;; file-util.scm
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
  file-util
  mzscheme

 (require "usual.scm")
 (require "string-util.scm")
 ;;(require (lib "file.ss"))
 
 ;; "Transfers" data from an input stream into an output stream.
 ;; 
 ;; Code from Swindle served as an example for this.
 (define* (read-write in out)
  (let* ((buffer (make-bytes 4096)))
    (let loop ()
      (let ((num-bytes (read-bytes-avail! buffer in)))
        (unless (eof-object? num-bytes)
          (write-bytes buffer out 0 num-bytes)
          (loop))))))
 
 (define* (read-string-from-file file)
   (let* ((out (open-output-string)))
     (call-with-input-file
         file
       (lambda (in)
         (read-write in out)))
     (get-output-string out)))

 ;; There are strings and byte strings, and we do not want to make a
 ;; policy decision here as to which string encoding we are using.
 ;; Using (lib "file.ss") here does not help us much since it deals
 ;; with byte strings, and (require (planet "file.ss" ("ryanc"
 ;; "scripting.plt" 1 1))) does fix the encoding choice to UTF-8.
 ;; So we provide what utilities we can without policy decisions.

 (define* (path-drop-extension path ext)
   (alet s (path->string path)
         (if (string-ends-with? s ext)
             (string->path (string-drop-from-end
                            (string-length ext) s))
             path)))
 
 (define* (path-replace-extension path old-ext new-ext)
   (let* ((extless (path-drop-extension path old-ext))
          (s (path->string extless))
          (exted (string-append s new-ext)))
     (string->path exted)))
 
 ) ; end module

#;
(begin
  (require (lib "usual.scm" "wg"))
  (require file-util)

  (let ((pn (string->path "file.scm")))
    (write-nl (path-drop-extension pn ".scm"))
    (write-nl (path-drop-extension pn ".rb"))
    (write-nl (path-replace-extension pn ".scm" ".rb"))
    (write-nl (path-replace-extension pn ".cpp" ".rb")))
  
  ;;(write-nl (read-string-from-file "/tmp/test1"))
  )
