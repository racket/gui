#lang racket/base
(require racket/unit
         racket/class
         mred/mred-sig
         "text-sig.rkt"
         "../preferences.rkt")
(provide text-column-guide@)

(define-unit text-column-guide@
  (import mred^)
  (export text-column-guide^)
  
  (define column-guide<%> (interface ((class->interface text%))))
  (define column-guide-mixin-pen-size 2)
  (define column-guide-mixin 
    (mixin ((class->interface text%)) (column-guide<%>)
      (inherit get-style-list invalidate-bitmap-cache get-dc
               begin-edit-sequence end-edit-sequence
               get-extent get-padding)
      (define char-width #f)
      (define pen #f)
      ;; these two functions are defined as private fields
      ;; because they are weakly held callbacks
      (define (bw-cb p v)
        (set! pen 
              (send the-pen-list find-or-create-pen
                    (if v
                        (make-object color% 225 225 51)
                        (make-object color% 204 204 51))
                    (* column-guide-mixin-pen-size 2)
                    'solid)))
      (define (cw-cb p v)
        (define new-cw (and (car v) (cadr v)))
        (unless (equal? new-cw char-width)
          (define (inv cw)
            (define x-pos (get-x-spot cw))
            (when x-pos
              (invalidate-bitmap-cache 
               (- x-pos (send pen get-width))
               0
               (+ x-pos (send pen get-width))
               'end)))
          (define old-char-w char-width)
          (set! char-width new-cw)
          (begin-edit-sequence #t #f)
          (inv old-char-w)
          (inv char-width)
          (end-edit-sequence)))
      
      (super-new)
    
      (preferences:add-callback 'framework:white-on-black? bw-cb #t)
      (bw-cb 'ignored-arg (preferences:get 'framework:white-on-black?))
    
      (preferences:add-callback 'framework:column-guide-width cw-cb #t)
      (cw-cb 'ignored-arg (preferences:get 'framework:column-guide-width))
    
      (define aw (box 0.0))
      (define ah (box 0.0))
      (define old-draw-the-line? #f)
      (define left-padding 0)
    
      (define/augment (on-change)
        (inner (void) on-change)
        (define old-aw (unbox aw))
        (define old-ah (unbox ah))
        (get-extent aw ah)
        (define new-draw-the-line? (draw-the-line?))
        (define-values (left top right bottom) (get-padding))
        (unless (and (= old-aw (unbox aw)) 
                     (= old-ah (unbox ah))
                     (= left left-padding)
                     (equal? new-draw-the-line? old-draw-the-line?))
          (set! old-draw-the-line? new-draw-the-line?)
          (set! left-padding left)
          (invalidate-bitmap-cache 0.0 0.0 'display-end 'display-end)))
    
      ;; pre: aw initialized to current value
      (define/private (draw-the-line?)
        (define x-pos (get-x-spot char-width))
        (and x-pos
             (< x-pos (- (unbox aw) 3))))
    
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (when char-width
          (when before?
            (define x-pos (get-x-spot char-width))
            (when x-pos
              (define old-pen (send dc get-pen))
              (send dc set-pen pen)
              (when (draw-the-line?)
                (send dc draw-line
                      (+ dx x-pos)
                      (+ dy top column-guide-mixin-pen-size)
                      (+ dx x-pos)
                      (+ dy (min (unbox ah) bottom) (- column-guide-mixin-pen-size))))
              (send dc set-pen old-pen)))))

      (define/private (get-x-spot char-width)
        (let/ec return
          (unless char-width (return #f))
          (define dc (get-dc))
          (unless dc (return #f))
          (define style (or (send (get-style-list) find-named-style "Standard")
                            (send (get-style-list) find-named-style "Basic")))
          (unless style (return #f))
          (define fnt (send style get-font))
          (define-values (xw _1 _2 _3) (send dc get-text-extent "x" fnt))
          (+ left-padding (* xw char-width)))))))

