;; 
;; graph.scm
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
 graph
 mzscheme

 (require (lib "oo.scm" "wg"))
 (require (lib "usual.scm" "wg"))

 (define (make-inode id)
   (let ((visited #f)
	 (explored #f)
	 (out-inodes '()))

     (lambda (method . args)
       (cond

	((eq? method 'id)
	 id)

	((eq? method 'visited?) 
	 visited)

	((eq? method 'mark-visited)
	 (set! visited #t))

	((eq? method 'explored?) 
	 explored)

	((eq? method 'mark-explored)
	 (set! explored #t))

	((eq? method 'add-edge)
	 (push-set! out-inodes (first args)))

	((eq? method 'edges)
	 out-inodes)

	(else
	 (error "no method" method))))))

 ;; All that this really does is constructs the vertices with
 ;; make-inode and sets them up with edges between them, and then
 ;; provides a convenient facility for looking up a node by its ID.
 (define (make-igraph edges)
   (let ((inodes '()))

     (define (get-inode-by-id id)
       (let ((entry (assq id inodes)))
	 (if entry (cadr entry) #f)))

     (define (add-node id)
       (aif inode (get-inode-by-id id)
	    inode
	    (begin
	      (set! inode (make-inode id))
	      (push-set! inodes (list id inode))
	      inode)))

     (for-each
      (lambda (edge)
	(let ((snode (add-node (car edge)))
	      (enode (add-node (cadr edge))))
	  (snode 'add-edge enode)))
      edges)

     (define-methods args
       (get-inode-by-id
	(get-inode-by-id (first args)))
       (get-inodes 
	inodes))))

 ;; This library deals with directed graphs only.
 ;;
 ;; This is how the input graph should be represented. We may use a
 ;; different format internally for efficiency, though.
 ;; 
 ;; type Node = Symbol
 ;; 
 ;; type Edge = (Symbol Symbol)
 ;; 
 ;; type Graph = [Edge]

 ;; This algorithm comes from somewhere on the net, forget where: "If
 ;; you reach a node that has been visited but not fully explored, the
 ;; edge that got you there is part of a cycle."
 ;; 
 ;; We ignore any nodes that cannot be reached from the start node.
 (define* (graph-cyclic? graph start-node)
   (define i-g (make-igraph graph))
   (define i-sn (i-g 'get-inode-by-id start-node))
   (unless i-sn
	   (error "graph has no node" start-node))
   (let/ec
    esc
    (letrec
	((trav
	  (lambda (i-n)
	    (if (i-n 'visited?)
		(unless (i-n 'explored?)
			(esc #t))
		(begin
		  (i-n 'mark-visited)
		  (let ((edges (i-n 'edges)))
		    (for-each
		     trav
		     edges))
		  (i-n 'mark-explored))))))
      (trav i-sn)
      (esc #f))))

) ; end module

#;
(begin
  (require graph)
  (require "util.scm")

  #;
  (let* ((igraph (make-igraph '((a b) (b c))))
	 (inode-a (igraph 'get-inode-by-id 'a)))
    (write-nl inode-a)
    (write-nl (map
	       (lambda (inode)
		 (inode 'id))
	       (inode-a 'edges)))
    (write-nl (igraph 'get-inodes)))

  (for-each
   (lambda (graph)
     (write-nl (list graph
		     (graph-cyclic? graph 's))))
   (list
    '((s b) (b c)) ;; acyclic
    '((s b) (b s)) ;; cyclic
    '((s a) (a b) (a c) (c a)) ;; cyclic
    '((s s)) ;; cyclic
    '((s a) (s b) (a c) (b c)) ;; acyclic
    )))
