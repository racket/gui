#lang racket/base
(require racket/class
         racket/gui/base)
(provide arrow-toggle-snip%)

;; arrow-toggle-snip% represents a togglable state, displayed as a right-facing
;; arrow (off or "closed") or a downward-facing arrow (on or "open").

;; The size of the arrow is determined by the style (and font) applied to the
;; snip. The arrow is drawn inscribed in a square resting on the baseline, but
;; the snip reports its size (usually) as the same as a capital "X"; this means
;; that the snip should look good next to text (in the same style) no matter
;; whether base-aligned or top-aligned.

;; ------------------------------------------------------------
;; Paths and gradients, in terms of 100x100 box

(define (down-arrow-path scale yb)
  (define (tx x) (* scale x))
  (define (ty y) (+ yb (* scale y)))
  (define p (new dc-path%))
  (send* p
    [move-to (tx 10) (ty 40)]
    [line-to (tx 90) (ty 40)]
    [line-to (tx 50) (ty 75)]
    [line-to (tx 10) (ty 40)]
    [close])
  p)

(define (down-arrow-gradient scale xb yb)
  (define (tx x) (+ xb (* scale x)))
  (define (ty y) (+ yb (* scale y)))
  (new linear-gradient%
       [x0 (tx 50)] [y0 (ty 40)]
       [x1 (tx 50)] [y1 (ty 75)]
       [stops
        (list (list 0 (make-object color% 240 240 255))
              (list 1 (make-object color% 128 128 255)))]))

(define (right-arrow-path scale yb)
  (define (tx x) (* scale x))
  (define (ty y) (+ yb (* scale y)))
  (define p (new dc-path%))
  (send* p
    [move-to (tx 40) (ty 10)]
    [line-to (tx 75) (ty 50)]
    [line-to (tx 40) (ty 90)]
    [line-to (tx 40) (ty 10)]
    [close])
  p)

(define (right-arrow-gradient scale xb yb)
  (define (tx x) (+ xb (* scale x)))
  (define (ty y) (+ yb (* scale y)))
  (new linear-gradient%
       [x0 (tx 40)] [y0 (ty 50)]
       [x1 (tx 75)] [y1 (ty 50)]
       [stops
        (list (list 0 (make-object color% 240 240 255))
              (list 1 (make-object color% 128 128 255)))]))

;; ------------------------------------------------------------

(define arrow-toggle-snip%
  (class snip%
    (inherit get-admin get-flags get-style set-flags set-count)
    (init [open? #f])
    (init-field [callback void]
                [size #f])
    (field [state (if open? 'down 'up)])  ; (U 'up 'down 'up-click 'down-click)
    (super-new)
    (set-count 1)
    (set-flags (cons 'handles-events (get-flags)))

    (define/override (copy)
      (new arrow-toggle-snip% (callback callback) (open? state) (size size)))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define old-brush (send dc get-brush))
      (define old-pen (send dc get-pen))
      (define old-smoothing (send dc get-smoothing))
      (define-values (size dy*) (get-target-size* dc))
      (define scale-factor (/ size 100))
      (define arrow-path
        (case state
          [(up up-click) (right-arrow-path scale-factor dy*)]
          [(down down-click) (down-arrow-path scale-factor dy*)]))
      (define arrow-gradient
        (case state
          [(up up-click) (right-arrow-gradient scale-factor x (+ dy* y))]
          [(down down-click) (down-arrow-gradient scale-factor x (+ dy* y))]))
      ;; Draw arrow
      (send* dc
        [set-pen "black" 0 'solid]
        [set-brush (new brush% [gradient arrow-gradient])]
        [set-smoothing 'aligned]
        [draw-path arrow-path x y])
      ;; Restore
      (send* dc
        [set-brush old-brush]
        [set-pen old-pen]))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (define-values (size dy) (get-target-size* dc))
      (set-box/f! descent 0)
      (set-box/f! space 0)
      (set-box/f! lspace 0)
      (set-box/f! rspace 0)
      (set-box/f! w size)
      (set-box/f! h (+ size dy)))

    ;; get-target-size* : -> (values Real Real)
    ;; Returns size of drawn square and dy to drop to baseline so whole
    ;; snip takes up same space as "X". (This is a hack because baseline
    ;; alignment would cause problems elsewhere.)
    (define/private (get-target-size* dc)
      (define-values (xw xh xd xa)
        (send dc get-text-extent "X" (send (get-style) get-font)))
      (let ([size (or size (* xh 0.6))])
        (values size (max 0 (- xh xd size)))))

    (define/override (on-event dc x y editorx editory evt)
      (define-values (arrow-snip-width dh) (get-target-size* dc))
      (define arrow-snip-height (+ arrow-snip-width dh))
      (let ([snip-evt-x (- (send evt get-x) x)]
            [snip-evt-y (- (send evt get-y) y)])
        (cond
          [(send evt button-down? 'left)
           (set-state (case state
                        [(up) 'up-click]
                        [else 'down-click]))]
          [(send evt button-up? 'left)
           (cond [(and (<= 0 snip-evt-x arrow-snip-width)
                       (<= 0 snip-evt-y arrow-snip-height))
                  (set-state (case state
                               [(down down-click) 'up]
                               [else 'down]))]
                 [else
                  (set-state (case state
                               [(down down-click) 'down]
                               [else 'up]))])]
          [(and (send evt get-left-down)
                (send evt dragging?))
           (cond [(and (<= 0 snip-evt-x arrow-snip-width)
                       (<= 0 snip-evt-y arrow-snip-height))
                  (set-state (case state
                               [(down down-click) 'down-click]
                               [else 'up-click]))]
                 [else
                  (set-state (case state
                               [(down down-click) 'down]
                               [else 'up]))])]))
      (super on-event dc x y editorx editory evt))

    (define/public (get-toggle-state)
      (case state [(down down-click) #t] [else #f]))

    (define/public (set-toggle-state new-state)
      (set-state (if new-state 'down 'up)))

    (define/private (set-state new-state)
      (unless (eq? state new-state)
        (define old-toggled? (get-toggle-state))
        (set! state new-state)
        (let ([admin (get-admin)])
          (when admin
            (define-values (size dy) (get-target-size* (send admin get-dc)))
            (send admin needs-update this 0 0 size (+ size dy))))
        (define new-toggled? (get-toggle-state))
        (unless (equal? new-toggled? old-toggled?)
          (callback new-toggled?))))

    (define/override (adjust-cursor dc x y editorx editory event)
      arrow-snip-cursor)
    ))

(define (set-box/f! b v) (when (box? b) (set-box! b v)))

(define arrow-snip-cursor (make-object cursor% 'arrow))
