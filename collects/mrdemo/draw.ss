
(unless (defined? 'draw-snip%)
  (load-relative "graph.ss"))

(define (draw:go)
  (define f (make-object mred:pasteboard-frame%))
  (define s (make-object draw-snip% 100 100))
  (define e (send f get-edit))
  (define s2 (make-object graph-snip% (list '(lambda (x) (* x x)))))
  (send f show #t)
  (send e insert s 10 10)
  (send e insert s2 120 120))
