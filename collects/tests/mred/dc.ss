
(load-relative "loadtest.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               DC Tests                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mdc (make-object bitmap-dc%))
(define bm (make-object bitmap% 10 10))
(define bm2 (make-object bitmap% 10 10))

(test #t 'is-color? (send bm is-color?))

(define (bad m . args)
  (with-handlers ([exn:application:mismatch?
		   (lambda (x)
		     (test '("ok")
			   `(send <bad-dc> ,m ...)
			   (regexp-match "ok" (exn-message x))))])
    (send-generic mdc (make-generic (object-interface mdc) m) . args)
    (error 'bad-dc "~a shouldn't succeed" `(send <bad-dc> ,m ...))))

(define (test-all mdc try)
  (try 'clear)
  (try 'draw-arc 0 0 10 10 0.1 0.2)
  (try 'draw-bitmap bm2 0 0)
  (try 'draw-bitmap-section bm2 0 0 0 0 5 5)
  (try 'draw-ellipse 0 0 10 10)
  (try 'draw-line 0 0 10 10)
  (try 'draw-lines (list (make-object point% 0 0) (make-object point% 10 10)))
  (try 'draw-point 5 5)
  (try 'draw-polygon (list (make-object point% 0 0) (make-object point% 10 10) (make-object point% 5 10)))
  (try 'draw-rectangle 0 0 10 10)
  (try 'draw-rounded-rectangle 0 0 10 10)
  (try 'draw-spline 0 0 10 10 5 10)
  (try 'draw-text "Hello" 0 0)

  (try 'start-doc "Ok")
  (try 'start-page)
  (try 'end-page)
  (try 'end-doc)

  (try 'get-background)
  (try 'get-brush)
  (try 'get-clipping-region)
  (try 'get-font)
  (try 'get-pen)
  (try 'get-size)
  (try 'get-text-background)
  (try 'get-text-foreground)
  (try 'get-text-mode)

  (try 'set-background (make-object color% "Yellow"))
  (try 'set-brush (make-object brush% "Yellow" 'solid))
  (try 'set-clipping-rect 0 0 10 10)
  (try 'set-clipping-region (make-object region% mdc))
  (try 'set-font (make-object font% 12 'default 'normal 'normal))
  (try 'set-origin 0 0)
  (try 'set-pen (make-object pen% "Yellow" 1 'solid))
  (try 'set-scale 2 2)
  (try 'set-text-background (make-object color% "Yellow"))
  (try 'set-text-foreground (make-object color% "Yellow"))
  (try 'set-text-mode 'transparent)
  (try 'try-color (make-object color% "Yellow") (make-object color%)))

(st #f mdc ok?)
(test-all mdc bad)

(send mdc set-bitmap bm)
(test-all mdc (lambda (m . args)
		(send-generic mdc (make-generic (object-interface mdc) m) . args)))

