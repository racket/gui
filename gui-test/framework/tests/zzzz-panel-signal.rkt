#lang racket/base
(require racket/class
         racket/gui/base
         framework
         rackunit)

(define (run-tests)
  (define semaphore (make-semaphore 0))
  (define semaphore-frame%
    (class frame%
      (define/augment (on-close) (semaphore-post semaphore))
      (super-new)))
  (define f (make-object semaphore-frame% "Single Panel Test"))
  (define navy-blue-brush (send the-brush-list find-or-create-brush "navy" 'solid))
  (define light-blue-brush (send the-brush-list find-or-create-brush "lightblue" 'solid))
  (define grid-canvas%
    (class canvas%
      (init-field lines)
      (init label)
      (inherit get-dc get-client-size)
      (override on-paint)
      (define (on-paint)
        (define-values (width height) (get-client-size))
        (define dc (get-dc))
        (define single-width (/ width lines))
        (define single-height (/ height lines))
        (send dc set-pen "black" 1 'transparent)
        (for* ([i (in-range lines)]
               [j (in-range lines)])
          (send dc set-brush
                (if (= 0 (modulo (+ i j) 2))
                    navy-blue-brush light-blue-brush))
          (send dc draw-rectangle
                (* single-width i)
                (* single-height j)
                single-width
                single-height)))
      (super-new)
      ;; soon to be obsolete, hopefully.
      (inherit set-label)
      (set-label label)
      (inherit min-width min-height)
      (min-width 50)
      (min-height 50)))
  (define border-panel (make-object horizontal-panel% f '(border)))
  (define single-panel (make-object panel:single% border-panel))
  (define children
    (list
     (new grid-canvas% (lines 3) (parent single-panel) (label "Small") (stretchable-width #f) (stretchable-height #f))
     (new grid-canvas% (lines 3) (parent single-panel) (label "Wide") (stretchable-width #t) (stretchable-height #f))
     (new grid-canvas% (lines 3) (parent single-panel) (label "Tall") (stretchable-width #f) (stretchable-height #t))
     (new grid-canvas% (lines 3) (parent single-panel) (label "Wide and Tall") (stretchable-width #t) (stretchable-height #t))))
  (define active-child (car children))
  (define radios (make-object horizontal-panel% f))
  (define (make-radio label choices callback)
    (define panel (make-object vertical-panel% radios '(border)))
    (define message (make-object message% label panel))
    (define radio (make-object radio-box% #f choices panel (λ (radio _) (callback radio))))
    (define button (make-object button%
                     "Cycle" panel
                     (λ (_1 _2)
                       (define before (send radio get-selection))
                       (define tot (send radio get-number))
                       (for ([n (in-range tot)])
                         (send radio set-selection n)
                         (callback radio)
                         (sleep/yield 1))
                       (send radio set-selection before)
                       (callback radio))))
    radio)
  (define radio
    (make-radio
     "Active Child"
     (map (λ (x) (send x get-label)) children)
     (λ (radio)
       (for ([c (in-list children)])
         (when (string=? (send radio get-item-label (send radio get-selection))
                         (send c get-label))
           (set! active-child c)
           (send single-panel active-child active-child))))))
  (define vertical-alignment 'center)
  (define horizontal-alignment 'center)
  (define (update-alignment)
    (send single-panel set-alignment horizontal-alignment vertical-alignment))
  (define horiz
    (make-radio
     "Horizontal Alignment"
     (list "left" "center" "right")
     (λ (radio)
       (set! horizontal-alignment (string->symbol (send radio get-item-label (send radio get-selection))))
       (update-alignment))))
  (define vert
    (make-radio
     "Vertical Alignment"
     (list "top" "center" "bottom")
     (λ (radio)
       (set! vertical-alignment (string->symbol (send radio get-item-label (send radio get-selection))))
       (update-alignment))))
  (define buttons (make-object horizontal-panel% f))
  (define result 'failed)
  (define failed (make-object button% "Failed" buttons (λ (_1 _2) (semaphore-post semaphore))))
  (define passed (make-object button% "Passed" buttons (λ (_1 _2) (set! result 'passed) (semaphore-post semaphore))))
  (send border-panel min-width 100)
  (send border-panel min-height 100)
  (send vert set-selection 1)
  (send horiz set-selection 1)
  (send buttons stretchable-height #f)
  (send buttons set-alignment 'right 'center)
  (send radios stretchable-height #f)
  (send f show #t)
  (void (yield semaphore))
  (send f show #f)
  (check-equal? result 'passed))

(module+ test
  (unless (getenv "PLTDRDR")
    (run-tests)))
