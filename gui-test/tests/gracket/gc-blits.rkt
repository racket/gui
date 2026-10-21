#lang racket/base
(require racket/class
	 racket/gui/base)

(define w 50)
(define h 50)
(define on (make-bitmap w h))
(define off (make-bitmap w h))
(send (send on make-dc) draw-rectangle 5 5 (- w 10) (- h 10))

(define final
  (for/fold ([prev #f]) ([i (in-range 10)])
    (define f (new frame% [label "GC Blit"] [width 100] [height 100]))
    (define c (new canvas% [parent f]))
    (register-collecting-blit c 0 0 w h on off)
    (send f show #t)
    (collect-garbage)
    (yield (system-idle-evt))
    (when prev
      (unregister-collecting-blit (cdr prev))
      (send (car prev) show #f))
    (cons f c)))

(unregister-collecting-blit (cdr final))
(send (car final) show #f)
