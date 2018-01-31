;;; A simple graph database in Scheme
;;; Copyright Thomas Haug 2018


;; define-struct (R5RS) --> define-record-type (in R6RS)
;; for the time being don#t use that

#lang r6rs
(import (rnrs base)
        (rnrs hashtables))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; beginregion graph engine related procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; graph storage engine related procedures

;; structure:
;; a graph storage engine is a list of following entries
;;   name (string)
;;   procedure to get next vertex ID
;;   procedure to get next edge ID
;;   procedure to vertices
;;   procedure to acess edges

;; alternative design 1: 
;; alternative design 2: storage egnine defines procdures and there is a dispatch procedure)

;; choosen design: A graph database is a list with the following content
;;(name next-vertex-id! next-edge-id! (add-vertex) (add-edge))
;; where name            is a symbol
;;       next-vertex-id! is a procedure returns the next vertrex id. It has no arguments
;;       next-edge-id!   is a procedure returns the next vertrex id. It has no arguments
;;       (list of vertex procedure)
;;       (list of edge procedure)

(define (engine-next-vertex-id engine)
  ((cadr engine)))

(define (engine-next-edge-id engine)
  ((caddr engine)))

(define (engine-add-vertex! engine vertex)
  ((car (engine-vertex-procedures engine)) vertex))

(define (engine-vertex-count engine)
  ((cadr (engine-vertex-procedures engine))))

(define (engine-vertices engine)
  ((caddr (engine-vertex-procedures engine))))

;; convenience procedure to retrieve the list of available vertex procedures of the graph engine
(define (engine-vertex-procedures engine)
  (cadddr engine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; endregion graph engine related procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; simple in-memory engine implementation

;; current hack global variable
;; move into engine  --> done ;-)
;;(define *CUR_ID* 0)

;;(define (next-id!) 
;;  (set! *CUR_ID* (+ *CUR_ID* 1))
;;    *CUR_ID*)

(define (create-in-memory-engine name)
  (let* ((id 0)
        (next-id! (lambda () (begin (set! id (+ id 1)) id)))
        (vertices '())
        (add-vertex (lambda (vertex) (set! vertices (cons vertex vertices))))
        (count-vertices (lambda () (length vertices)))
        (get-vertices (lambda () vertices))   ;; is this correct?
        )
    (list name next-id! next-id! (list add-vertex count-vertices get-vertices) '()))
)


(define (create-graph name engine)
 (list name engine '() '())
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; beginregion VERTEX related procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a Vertex with a label
;; the vertex has no properties 
(define (engine-create-vertex engine label)
  (cond ((and (symbol? label))
      (list (engine-next-vertex-id engine) label (make-eq-hashtable 0)) 
      )))

(define (vertex-id vertex)
  (car vertex))

;; return the name of a vertex
(define (vertex-label vertex)
  (cadr vertex))

;; return the properties of the vertex
(define (vertex-properties vertex)
  (caddr vertex))

;; add or replace a property of the given vertex
(define (vertex-set-property! vertex name value)
  (hashtable-set! (vertex-properties vertex) name value))

;;; endregion VERTEX related procedures


;;; region sample code


; (define g (create-in-memory-engine 'Test))
; (define v (engine-create-vertex g 'TestVertex))
; (engine-vertex-count g)
; (engine-add-vertex! g v)
; (engine-vertex-count g)
; (engine-vertices g)
;;;