#lang scheme

(require (lib "usual-4.ss" "common"))
(require konffaile/component)
(require konffaile/variant)

(define* (info)
  (define var (current-variant))
  (new abld-component%
       (mmp-libs
        (if (and (eq? 'symbian (send var platform))
                 (send var do-logging.attr))
            '(
              ;; With logging enabled, there is a dependency on the
              ;; (deprecated) file logger API. Without logging enabled
              ;; errors are still displayed on the console (if you
              ;; have one and have time to read it).
              "flogger.lib"
              )
            '()))))

#|

;; Test program.

(require (lib "usual-4.ss" "common"))
(require konffaile/variant)
(require konffaile/component)

(current-variant
 (new
  (class object%
    (inspect #f)
    (super-new)
    (define/public (platform) 'symbian)
    (define/public (do-logging.attr) #t))))

(component-search-path (list (build-path 'same)))

;;(spec-paths-by-name #f 'common/logging)

(define resolve (make-name-resolver))

;;(resolve #f 'common/logging)

(resolve-deps 'common/logging)

|#

