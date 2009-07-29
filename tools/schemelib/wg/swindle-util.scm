;; Provides a selection of useful Swindle utilities.

(module
 swindle-util
 mzscheme

 (require (lib "misc.ss" "swindle"))

 (provide 
  define*
  define-syntax*
  make-provide-syntax
  symbol-append
  negate
  thunk
  while
  until
  dotimes
  dolist
  no-errors
  no-errors*
  ))
