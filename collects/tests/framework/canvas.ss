(define (test-creation class name)
  (test
   name
   (lambda (x) #t)
   `(let ([f (make-object frame:basic%)]
	  [c (make-object ,class f)])
      (send f show #t))))

(test-creation 'canvas:wide-snip-mixin-creation '(canvas:wide-snip-mixin editor-canvas%))
(test-creation 'canvas:wide-snip%-creation 'canvas:wide-snip%)
