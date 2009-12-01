#lang scheme

#|

This one could be useful for photo tagging after a trip abroad. It
avoids the expense of uploads and remote control, but records
location. To help save battery, it has a GUI for easy ON/OFF.

|#

(require "base.ss")
(require common/usual-4)
(require konffaile/variant)

(define* (info)
  (new
   (variant-class
    devel-variant%
    (super-new)
    (define/override (feature-uploader.attr)
      #f)
    (define/override (feature-remokon.attr)
      #f)
    )
   (sensor-list '(cellid gps))))
