#lang scheme

#|

For development and testing.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* klass%
   (variant-class
    symbian-variant%
    (super-new)
    
    (define/override (binary-type) 'daemon)
    (define/override (s60-vernum.attr) 31)
    (define/override (kit-name) 's60_31)
    
    (define/override (have-sqlite3.attr) #t)
    
    (define/override (appfocus-enabled.attr) #t)
    (define/override (appmessage-enabled.attr) #t)
    (define/override (btprox-enabled.attr) #f)
    (define/override (callstatus-enabled.attr) #t)
    (define/override (cellid-enabled.attr) #f)
    (define/override (gps-enabled.attr) #f)
    (define/override (inactivity-enabled.attr) #t)
    (define/override (indicator-enabled.attr) #t)
    (define/override (keypress-enabled.attr) #t)
    (define/override (profile-enabled.attr) #t)
    (define/override (smsevent-enabled.attr) #t)
    (define/override (weburl-enabled.attr) #t)
        
    (define/override (feature-remokon.attr) #f)

    (define/override (upload-time-expr.attr) "never")

    (define/override (username.attr) "developer")

    )) ;; end class

(define* (info)
  (new klass%)) 
