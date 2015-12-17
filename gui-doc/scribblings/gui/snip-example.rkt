#lang racket/base
(require racket/class
         racket/snip
         racket/format)

(provide circle-snip%
         (rename-out [circle-snip-class snip-class]))

(define circle-snip%
  (class snip%
    (inherit set-snipclass
             get-flags set-flags
             get-admin)
    (init-field [size 20.0])
    
    (super-new)
    (set-snipclass circle-snip-class)
    (send (get-the-snip-class-list) add circle-snip-class)
    (set-flags (cons 'handles-events (get-flags)))
    
    (define/override (get-extent dc x y	 	 	 	 
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (maybe-set-box! w (+ 2.0 size))
      (maybe-set-box! h (+ 2.0 size))
      (maybe-set-box! descent 1.0)
      (maybe-set-box! space 1.0)
      (maybe-set-box! lspace 1.0)
      (maybe-set-box! rspace 1.0))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-ellipse (+ x 1.0) (+ y 1.0) size size))
    
    (define/override (copy)
      (new circle-snip% [size size]))
    
    (define/override (write f)
      (send f put size))
    
    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down?)
        (set! size (+ 1.0 size))
        (define admin (get-admin))
        (when admin
          (send admin resized this #t))))))

(define circle-snip-class%
  (class snip-class%
    (inherit set-classname)
    
    (super-new)
    (set-classname (~s '((lib "main.rkt" "circle-snip")
                         (lib "wxme-circle-snip.rkt" "circle-snip"))))
    
    (define/override (read f)
      (define size-b (box 0.0))
      (send f get size-b)
      (new circle-snip% [size (unbox size-b)]))))

(define circle-snip-class (new circle-snip-class%))
