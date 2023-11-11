#lang racket
; TODO UNCOMMENT THIS.
;(require (only-in "bv.rkt" fragment-ast) "solver.rkt")
(require "bv.rkt" "solver.rkt")

(provide verify)


(define-fragment (bar x y)
  (define a (bvlshr x 1))
  (define b (bvlshr x 1))
  (return (bvadd a b)))

(define (of_ids id0 id1) (format "~a_~a" id0 id1))

(define-fragment (baz x y)
  (return x))

(define (of_const const)
  (match const
    [true const]
    [false const]
    [_ const]))

(define (of_expr id expr)
  (match expr
    ; id TODO this is bugged. id's exist elsewhere.
    [`(define ,id_inner) (list "foo" (of_ids id id_inner))]

    ; if-statement
    [`(if ,expr ,expr_true ,expr_false) (list 'if (of_expr id expr) (of_expr id expr_true) (of_expr id expr_false))]
    
    ; unary-op
    [(list unary-op expr) (list unary-op (of_expr id expr))]

    ; binary-op
    [(list binary-op expr_lhs expr_rhs)(list binary-op (of_expr id expr_lhs) (of_expr id expr_rhs))]

    ; nary-op
    [(list nary-op exprs ...) (list nary-op (map (lambda (expr) (of_expr id expr)) exprs))]

    ; arg
    [id_other #:when (not (number? expr)) (of_ids id id_other)]
    
    ; const
    [const (of_const const)]

))

(define (of_stmt id stmt)
  (match stmt
    [`(define ,id_other ,expr) (list 'define-fun (of_ids id id_other) '() '(_ BitVec 32) (of_expr id expr))]))

(define (of_rets id1 id2 ret1 ret2)
  (match (list ret1 ret2)
    [(list `(return ,expr1) `(return ,expr2))
  (list `assert '= (list (of_expr id1 ret1) (of_expr id2 ret2)))]))

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
      (map (lambda (id_other) (list 'declare-const (of_ids id1 id_other) '(_ BitVec 32))) args1)
      (map (lambda (id_other) (list 'declare-const (of_ids id2 id_other) '(_ BitVec 32))) args2)

      ; assert equivalence between positional args.
      (of_argss id1 id2 args1 args2)
      
      (map (lambda (stmt) (of_stmt id1 stmt)) stmts1)
      (map (lambda (stmt) (of_stmt id2 stmt)) stmts2)
      ; zip the args1 and args2 together with assert =
      ; assert a final equality between ret1 and ret2

      (list (of_rets id1 id2 ret1 ret2))

     )]))

(define (pp_lines lines) (map (lambda (line) (printf "~a\n" line)) lines))
(pp_lines (of_progs bar baz))
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
  (solve (of_progs f1 f2)))

;(verify bar baz)
;(printf "Bar ~a\n" (unwrap-prog foo))
;(verify bar baz)
;(solve '((declare-const a Int)))
;(solve "(declare-const a Int)")