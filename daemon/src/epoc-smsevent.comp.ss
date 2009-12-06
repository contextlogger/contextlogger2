#lang scheme

(require (lib "usual-4.ss" "common"))
(require konffaile/component)
(require konffaile/variant)

(define* (info)
  (define var (current-variant))
  (new abld-component%
       (mmp-libs
        '(
          "gsmu.lib" ;; CSmsPDU
          "msgs.lib" ;; CMsvSession
          "smcm.lib" ;; CSmsClientMtm
          ))))
