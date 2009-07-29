;; 
;; tables.scm
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

;; We refer to context or control data passed between passes as
;; "tables". This module defines routines for managing such data.
(module
 tables
 mzscheme

 (require (lib "usual.scm" "wg"))
 (require (prefix h. (lib "hash.scm" "wg")))
 (require (prefix e. (lib "environment.scm" "wg")))
 (require (lib "ast-util.scm" "wg"))
 (require (lib "declarations.scm" "wg"))

 (define* (new-tables)
   (h.new))

 (define* (carry-tables f)
   (lambda (ast tables)
     (list (f ast) tables)))

 (define* tables-set h.store)
 
 (define* tables-get-reqd h.fetch-reqd)

 (define* tables-get-opt h.fetch)

 (define* (get-g-table tables)
   (h.fetch tables 'g-table (e.new)))

 (define* (get-d-table tables)
   (h.fetch tables 'd-table (h.new)))

 (define* (set-g-table tables value)
   (h.store tables 'g-table value))

 (define* (set-d-table tables value)
   (h.store tables 'd-table value))

 (define* (set-d-g-table tables d-table g-table)
   (set! tables (set-d-table tables d-table))
   (set! tables (set-g-table tables g-table))
   tables)

 ;; Takes the element "elem", extracts a "name" from it, assigns a
 ;; declaration ID to the element, adds the declaration to d-table,
 ;; adds the ID to the element, and returns (new d-table, new elem).
 (define* (d-table-add-id d-table elem)
   (define nid (get-opt-nlist-elem-1 elem 'decl-id))
   (unless nid
     (let* ((name (get-reqd-nlist-elem-1 elem 'name)))
       (set! nid (d-table-next-id d-table name))
       (set! elem (push elem `(decl-id ,nid)))))
   (list (h.store d-table nid elem)
         elem))

 ;; Like d-table-add-id, but also adds a name to the provided global
 ;; name table.
 (define/kw* (tables-add-id-and-name d-table g-table elem #:key is-func)
   (let* ((name (get-reqd-nlist-memb-1 elem 'name))
	  (nid (d-table-next-id d-table name))
	  (nelem (push elem `(decl-id ,nid))))
     (list (h.store d-table nid nelem)
	   (e.add-binding g-table name nid #:is-func is-func)
	   nelem)))

 (define* (make-td-texpr d-table)
   (lambda (id)
     (alet decl (get-decl-by-id d-table id)
	   (and (eq? (car decl) 'cxx-typedef)
		(fget-reqd-nlist-elem-1 decl 'type)))))

 (define* (tables-make-td-texpr tables)
   (make-td-texpr (get-d-table tables)))

) ; end module
