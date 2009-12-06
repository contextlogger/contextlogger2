#lang scheme

(require (lib "usual-4.ss" "common"))
(require konffaile/class-attr)
(require konffaile/component)
(require konffaile/variant)

(define* (info)
  (define var (current-variant))
  (define (attr x) (object-get-attr var x))
  (new abld-component%
       (mmp-libs
        (list (if (attr 'have-profileengine-lib)
                  "profileengine.lib"
                  "profileeng.lib" ;; SDK Plugins
                  )))))
