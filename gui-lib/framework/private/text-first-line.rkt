#lang racket/base
(require racket/unit
         racket/class
         mred/mred-sig
         "text-sig.rkt"
         "interfaces.rkt"
         "../preferences.rkt")
(provide text-first-line@)

(define-unit text-first-line@
  (import mred^
          text-line-numbers^)
  (export text-first-line^)
  
  (define first-line<%>
    (interface ()
      highlight-first-line
      get-first-line-height
      first-line-currently-drawn-specially?
      is-special-first-line?))

  (define dark-first-line-color (make-object color% 50 0 50))

  (define first-line-mixin
    (mixin ((class->interface text%)) (first-line<%>)
      (inherit get-text paragraph-end-position get-admin invalidate-bitmap-cache position-location
               scroll-to local-to-global get-dc get-padding)
      (define bx (box 0))
      (define by (box 0))
      (define bw (box 0))
    
      (define fancy-first-line? #f)
    
      (define first-line "")
      (define end-of-first-line 0)
      (define first-line-is-lang? #f)

      (define/public-final (highlight-first-line on?)
        (unless (equal? fancy-first-line? on?)
          (set! fancy-first-line? on?)
          (invalidate-bitmap-cache)
          (let ([canvas (send this get-canvas)])
            (when canvas
              (send canvas refresh)))))
    
      (define/public-final (get-first-line-height)
        (let-values ([(_1 h _2 _3) (send (get-dc) get-text-extent first-line (get-font))])
          h))
    
      (define/public-final (first-line-currently-drawn-specially?)
        (and (show-first-line?)
             (let ([admin (get-admin)])
               (and admin
                    (begin
                      (send admin get-view #f by #f #f #f)
                      (not (= (unbox by) 0)))))))
           
      (define/public (is-special-first-line? l) #f)
    
      (define/private (show-first-line?)
        (and fancy-first-line? first-line-is-lang?))
    
      (define/private (update-first-line)
        (set! end-of-first-line (paragraph-end-position 0))
        (set! first-line (get-text 0 end-of-first-line))
        (set! first-line-is-lang? (is-special-first-line? first-line)))
    
      (define/augment (after-insert start len)
        (when (<= start end-of-first-line)
          (update-first-line))
        (inner (void) after-insert start len))
      (define/augment (after-delete start len)
        (when (<= start end-of-first-line)
          (update-first-line))
        (inner (void) after-delete start len))
    
      (define/override (scroll-editor-to localx localy width height refresh? bias)
        (let ([admin (get-admin)])
          (cond
            [(not admin)
             #f]
            [(show-first-line?)
             (let ([h (get-first-line-height)])
               (set-box! by localy)
               (local-to-global #f by)
               (cond
                 [(<= (unbox by) h)
                  ;; the max is relevant when we're already scrolled to the top.
                  (super scroll-editor-to localx (max 0 (- localy h)) width height refresh? bias)]
                 [else
                  (super scroll-editor-to localx localy width height refresh? bias)]))]
            [else
             (super scroll-editor-to localx localy width height refresh? bias)])))
    
      (define/override (on-event event)
        (cond
          [(or (send event moving?)
               (send event leaving?)
               (send event entering?))
           (super on-event event)]
          [else
           (let ([y (send event get-y)]
                 [h (get-first-line-height)]
                 [admin (get-admin)])
             (unless admin (send admin get-view #f by #f #f #f))
             (cond
               [(and admin
                     (< y h)
                     (not (= (unbox by) 0)))
                (send admin scroll-to (send event get-x) 0 0 0 #t)
                (super on-event event)]
               [else         
                (super on-event event)]))]))

      (define to-invalidate #f)
      (define/override (on-scroll-to)
        (super on-scroll-to)
        (set! to-invalidate (get-region-to-draw)))
      (define/override (after-scroll-to)
        (super after-scroll-to)
        (define (maybe-invalidate)
          (when to-invalidate
            (invalidate-bitmap-cache
             (list-ref to-invalidate 0)
             (list-ref to-invalidate 1)
             (list-ref to-invalidate 2)
             (list-ref to-invalidate 3))
            (set! to-invalidate #f)))
        (maybe-invalidate)
        (set! to-invalidate (get-region-to-draw))
        (maybe-invalidate))
      (define/private (get-region-to-draw)
        (cond
          [(show-first-line?)
           (define admin (get-admin))
           (cond
             [admin
              (send admin get-view bx by bw #f #f)
              (define first-line (get-text 0 (paragraph-end-position 0)))
              (define-values (tw th _1 _2) (send (get-dc) get-text-extent first-line (get-font)))
              (list (unbox bx) (unbox by) (unbox bw) (+ th extra-fade-space))]
             [else #f])]
          [else #f]))

      (define extra-fade-space 11)
    
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (unless before?
          (when (show-first-line?) 
            (define admin (get-admin))
            (when admin
              (send admin get-view bx by bw #f #f)
              (define y-coord (unbox by))
              (unless (= y-coord 0)
                (define draw-first-line-number?
                  (and (is-a? this line-numbers<%>)
                       (send this showing-line-numbers?)))
                (define first-line (get-text 0 (paragraph-end-position 0)))
                (define old-pen (send dc get-pen))
                (define old-brush (send dc get-brush))
                (define old-smoothing (send dc get-smoothing))
                (define old-α (send dc get-alpha))
                (define old-font (send dc get-font))
                (define old-text-foreground (send dc get-text-foreground))
                (define old-text-mode (send dc get-text-mode))
                (define w-o-b? (preferences:get 'framework:white-on-black?))
                (send dc set-font (get-font))
                (send dc set-smoothing 'aligned)
                (send dc set-text-mode 'transparent)
                (define-values (tw th _1 _2) (send dc get-text-extent first-line))
                (define line-height (+ y-coord dy th 1))
                (define line-left (+ (unbox bx) dx))
                (define line-right (+ (unbox bx) dx (unbox bw)))

                (if w-o-b?
                    (send dc set-pen "white" 1 'solid)
                    (send dc set-pen "black" 1 'solid))
                (send dc draw-line line-left line-height line-right line-height)
              
                (when (eq? (send dc get-smoothing) 'aligned)
                  (define start (if w-o-b? 6/10 3/10))
                  (define end 0)
                  (define steps (- extra-fade-space 1))
                  (send dc set-pen 
                        dark-first-line-color
                        1
                        'solid)
                  (let loop ([i steps])
                    (unless (zero? i)
                      (define alpha-value (+ start (* (- end start) (/ i steps))))
                      (send dc set-alpha alpha-value)
                      (send dc draw-line 
                            line-left
                            (+ line-height i)
                            line-right
                            (+ line-height i))
                      (loop (- i 1)))))
              
                (send dc set-alpha 1)
                (send dc set-pen "gray" 1 'transparent)
                (send dc set-brush (if w-o-b? "black" "white") 'solid)
                (send dc draw-rectangle (+ (unbox bx) dx) (+ y-coord dy) (unbox bw) (+ th 1))
                (send dc set-text-foreground
                      (send the-color-database find-color
                            (if w-o-b? "white" "black")))
                (define x-start
                  (cond
                    [draw-first-line-number?
                     (send this do-draw-single-line dc dx dy 0 y-coord #f #f)
                     (send dc set-pen (if w-o-b? "white" "black") 1 'solid)
                     (send this draw-separator dc y-coord (+ y-coord line-height) dx dy)
                     (define-values (padding-left _1 _2 _3) (get-padding))
                     padding-left]
                    [else 0]))
                (send dc draw-text first-line (+ x-start (+ (unbox bx) dx)) (+ y-coord dy))
              
                (send dc set-text-foreground old-text-foreground)
                (send dc set-text-mode old-text-mode)
                (send dc set-font old-font)
                (send dc set-pen old-pen)
                (send dc set-brush old-brush)
                (send dc set-alpha old-α)
                (send dc set-smoothing old-smoothing)))))
        (super on-paint before? dc left top right bottom dx dy draw-caret))
    
      (inherit get-style-list)
      (define/private (get-font)
        (define style-list (get-style-list))
        (define std (or (send style-list find-named-style "Standard")
                        (send style-list basic-style)))
        (send std get-font))
    
      (super-new))))
