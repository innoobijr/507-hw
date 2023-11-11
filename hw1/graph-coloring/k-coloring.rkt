#lang racket

(require "solver.rkt" "graph.rkt")

(provide
 k-coloring      ; (->* (graph/c natural-number/c) [(is-a? solver%)] (or/c #f coloring/c))
 valid-coloring? ; (-> graph/c coloring/c boolean?)
)

; Returns true iff the given coloring is correct for the specified graph.
(define (valid-coloring? graph coloring)
  (and (coloring/c coloring)
       (= (color-size coloring) (node-count graph))
       (for*/and ([(e n) (in-indexed graph)] [child e])
         (not (= (color-ref coloring n) (color-ref coloring child))))))

(define (generate-vars k v)
  (range 1 (+ (* k v) 1)))

(define (generate-aux-ivars k v)
  (range (+ 1 (* k v)) (+ 1 (* 2 ( * k v)))))


(define (exactly-once-int symbols auxs size k)
   (append* (list )(for/list ([i (range size)])
    (cond
      [(zero? (modulo i k)) (list (list (-(list-ref auxs i)) (list-ref symbols i) (list-ref symbols (+ i 1)))
                       (list (-(list-ref symbols i)) (-(list-ref symbols (+ i 1)))))]
      [(eq? (- k 1) (modulo i k )) (list (list (list-ref auxs (- i 1)))) ]
      [else (list (list (-(list-ref auxs i)) (list-ref auxs (- i 1)) (list-ref symbols (+ i 1)))
                       (list (-(list-ref auxs (- i 1))) (-(list-ref symbols (+ i 1)))))]
      ))))

(define (make-constraint symbols fst snd k)
  (for/list ([i (range k)])
    (list (-(list-ref symbols (+ fst i))) (-(list-ref symbols (+ snd i))))
  ))

(define (third-cl-int symbols edges size colors)
  (append* (list ) (for/list ([i edges])
    (make-constraint symbols (* (car i) colors) (* (cdr i) colors) colors)
    )))

(define (generate-ladder-encoding graph symbols auxiliary color)
  (F graph symbols auxiliary color))


(define/contract (stream-to-list str)
  (-> stream? list?)
  (stream->list str))

; get edges
(define (get-edges graph)
  (stream-to-list (edges graph)))

; get edge count
; (edge-count)

; get nodes
(define (get-nodes graph)
  (stream-to-list (nodes graph)))

(define (F graph si ai k)
    (append (exactly-once-int si ai (* (node-count graph) k) k) (third-cl-int si (get-edges graph) (* (node-count graph) k) k)))

;
(define (execute-get-coloring graph k)
  (cond
    [(> k 1) (let* ([e (get-edges graph)]
        [a (generate-aux-ivars k (node-count graph))]
        [s (generate-vars k (node-count graph))]
        [F (generate-ladder-encoding graph s a k)]
        [sol (solve F)])
         (extract-coloring sol k  (node-count graph))
               )]))
;
(define (extract-coloring interpretation k v)
  (cond
    [(eq? interpretation #f) #f]
    [else (map (lambda (x) (modulo x k)) (filter positive? (take interpretation (* k v))))]
    ))
; Returns a coloring/c if the given graph can
; be colored with k colors.  Otherwise returns #f.
(define (k-coloring graph k)
  (execute-get-coloring graph k)
  )
