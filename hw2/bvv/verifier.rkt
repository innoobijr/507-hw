#lang racket
; TODO UNCOMMENT THIS.
(require
  (only-in "bv.rkt" define-fragment fragment-ast)
  (only-in rosette bv)
  "solver.rkt")
;(require "bv.rkt" "solver.rkt")

(provide verify)

; keep track of recurrences in variable definition and append count to ids to disambiguate.
(define occurrence (make-hash))
(define (with_occurrence tok)
  (format "~a_~a" tok (hash-ref occurrence tok 0)))

;(define (of_ids id0 id1) (format "~a_~a" id0 id1))
(define (of_ids id0 id1) (with_occurrence (format "~a_~a" id0 id1)))

(define (of_const const) (regexp-replace #rx#"\\(bv " (regexp-replace #rx#" 32\\)" (format "~a" (bv const 32)) "") ""))

(define (of_expr id expr)
  (match expr
    
    ; if-statement
    [`(if ,expr ,expr_true ,expr_false) (list 'ite (of_expr id expr) (of_expr id expr_true) (of_expr id expr_false))]
    
    ; unary-op
    [(list unary-op expr) (list unary-op (of_expr id expr))]

    ; binary-op
    [(list binary-op expr_lhs expr_rhs) (list binary-op (of_expr id expr_lhs) (of_expr id expr_rhs))]

    ; nary-op
    [(list nary-op exprs ...) (list nary-op (map (lambda (expr) (of_expr id expr)) exprs))]

    ; id
    [id_other #:when (not (number? expr)) (of_ids id id_other)]
    
    ; const
    [const (of_const const)]

))

(define (get_expr_kind expr)
  (match expr
    [`(bvule ,_ ...) 'Bool]
    [`(bvult ,_ ...) 'Bool]
    [`(bvuge ,_ ...) 'Bool]
    [`(bvugt ,_ ...) 'Bool]
    [`(bvsle ,_ ...) 'Bool]
    [`(bvslt ,_ ...) 'Bool]
    [`(bvsge ,_ ...) 'Bool]
    [`(bvsgt ,_ ...) 'Bool]
    [`(= ,_ ...) `Bool]
    [`(if ,expr ,expr_true ,expr_false) (get_expr_kind expr_true)]
    [_ '(_ BitVec 32)]
    ))

(define (of_stmt id stmt)
  (match stmt
    ; define id expr
    [`(define ,id_other ,expr)
     ; This is the first occurrence of variable. Set occurrence to 0.
     (hash-set! occurrence (format "~a_~a" id id_other) 0)
     (list 'define-fun (of_ids id id_other) '() (get_expr_kind expr) (of_expr id expr))]
    
    ; set! id expr
    [`(set! ,id_other ,expr)
     
     ; evauluate rhs side before updating occurrence and then do lhs.
     (define rhs (of_expr id expr))
     
     ; this is effectively a redefinition. Bump the occurrence number so that this variable is disambiguated.
     (hash-update! occurrence (format "~a_~a" id id_other) (lambda (val) (+ val 1)))
     (list 'define-fun (of_ids id id_other) '() (get_expr_kind expr) rhs)]))

; assert equivalence between 'return' exprs of the two functions.
(define (of_rets ret1 ret2)
  (match (list ret1 ret2)
    [(list `(return ,expr1) `(return ,expr2))
     (list `assert  (list 'not (list '= "#x00000000" (list `bvxor (of_expr "A" expr1) (of_expr "B" expr2)))))]))

; assert equivalences between two equal-length lists of args. (a0 a1 a2) (b0 b1 b2) -> ((assert = a0 b0) ...)
(define (of_argss id1 id2 ids1_inner ids2_inner)
  (match (list ids1_inner ids2_inner)
    [(list (list id1_inner ids1_inner ...) (list id2_inner ids2_inner ...))
     (append
      (list (list `assert (list `= (of_ids id1 id1_inner) (of_ids id2 id2_inner))))
      (of_argss id1 id2 ids1_inner ids2_inner))]
    [_ `()])) 

(define (of_progs prog1 prog2)
  (match (list (fragment-ast prog1) (fragment-ast prog2))
    [(list
      `(define-fragment (,id1 ,args1 ...) ,stmts1 ... ,ret1)
      `(define-fragment (,id2 ,args2 ...) ,stmts2 ... ,ret2))
     (append

      ; declare inputs for original functions.
      (map (lambda (id_other) (list 'declare-const (of_ids "A" id_other) '(_ BitVec 32))) args1)
      (map (lambda (id_other) (list 'declare-const (of_ids "B" id_other) '(_ BitVec 32))) args2)

      ; assert equivalence between positional args.
      (of_argss "A" "B" args1 args2)
      
      (map (lambda (stmt) (of_stmt "A" stmt)) stmts1)
      (map (lambda (stmt) (of_stmt "B" stmt)) stmts2)
 
      ; assert a final equality between ret1 and ret2
      (list (of_rets ret1 ret2))
     )]))

(define (pp_encoding lines) (map (lambda (line) (printf "~a\n" line)) lines))

; The verifier takes as input two BV fragments
; (see examples.rkt and bv.rkt) and produces one 
; of two outputs: (1) 'EQUIVALENT if the fragments are 
; semantically equivalent; or (2) an input, represented 
; as a list of values, on which the fragments produce 
; different outputs.  The inputs to a fragment are always 
; integer values.
;
; The verifier performs the equivalence check by 
; producing a QF_BV formula that is unsatisfiable iff 
; the fragments are equivalent.  See solver.rkt.
;
; (-> fragment? fragment? (or/c 'EQUIVALENT (listof integer?))))
(define (verify f1 f2)
  (hash-clear! occurrence)
  (define encoding (of_progs f1 f2))
  ;(pp_encoding encoding)
  (match (solve encoding)
    [#f `EQUIVALENT]
    ; Solve returns variables set for both f1 and f2. Half this list is redundant.
    [mapping (list-tail (hash-values mapping) (/ (hash-count mapping) 2))]))
  