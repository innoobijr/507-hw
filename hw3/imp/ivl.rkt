#lang rosette

(provide cut unroll)

; Takes as input the AST of an IMP program that is annotated
; with requires / ensures clauses and loop invariants, and
; returns the corresponding program in the IMP intermediate
; verification language (IVL). IVL programs contain no annotations
; and no loops, but may include havoc, assert, and assume statements.
; This procedure transforms annotated IMP programs to IVL by cutting
; loops, as shown in lecture. The procedure also transforms the 
; requires / ensures clauses into suitable IVL statements
; in the body of the generated program.
(define (cut ast)
  (match ast
     [`(procedure ,id , arg :, ret (#:requires ,P) (#:ensures ,Q) ,S)
       (list
	'procedure id arg ': ret
	(list `begin (list `assume P) (cut S) (list `assert Q)))]
     [`(begin) ast]
     [`(begin, S1, S2 ...)
       (let ([one (cut S1)]
	     [two ( map (lambda (s) 
			  (cut s))
			S2)])
	 (cons `begin (cons one (foldr append (list) two))))]

     [`(if, C, S1, S2) 
       (list (list
	 `if C 
	  (cut S1)
	  (cut S2)))
	  ]
     [`(while , C (#:invariant, I), S) 
       (list
	 (list `assert I)
	 (list `havoc `x)
	 (list `havoc `y)
	 (list `assume I)
	 (list
	   `if C 
	   (list `begin (cut S) (list `assert I) (list `assume #f))
	   (list `skip)
	   )
	)]
     [`(skip) ast]
     [`(abort) ast]
     [`(assert, C) (list ast)]
     [`(assume, C) (list ast)]
     [`(havoc, C) (list ast)]
     [`(:=, x, E) ast]
     ))

; Takes as input the AST of an IMP program that is annotated
; with requires / ensures clauses, and returns a finitized
; version of this program in IVL that unrolls all loops k >= 0 times.
; Specifically, the resulting IVL program can perform up to
; k iterations of every loop in the original IMP program, ensuring
; that there are no inputs on which the transformed program fails and
; the original one does not. The procedure also transforms the 
; requires / ensures clauses into suitable IVL statements
; in the body of the generated program.

(define (unroll ast k)
  (match ast
     [`(procedure ,id , arg :, ret (#:requires ,P) (#:ensures ,Q) ,S)
       (list
	'procedure id arg ': ret
	(list `begin (list `assume P) (unroll S k) (list `assert Q)))]
     [`(begin) ast]
     [`(begin, S1, S2 ...)
       (let ([one (unroll S1 k)]
	     [two ( map (lambda (s) 
			  (unroll s k))
			S2)])
	 (cons `begin (cons one (foldr append (list) two))))]
     [`(if, C, S1, S2) 
       (list (list
	 `if C 
	  (unroll S1 k)
	  (unroll S2 k)))
	  ]
     [`(while , C (#:invariant, I), S) 
	(for/list ([i (range k)])
		(list
		   `if C 
		   (unroll S k)
		   (list `skip)
		   ))]
     [`(skip) ast]
     [`(abort) ast]
     [`(assert, C) (list ast)]
     [`(assume, C) (list ast)]
     [`(havoc, C) (list ast)]
     [`(:=, x, E) ast]
     ))