#lang racket/base
(require racket/class
         racket/gui/base
         mrlib/include-bitmap)
(provide arrow-toggle-snip%)

(define arrow-toggle-snip%
  (class snip%
    (inherit get-admin get-flags set-flags set-count)
    (init-field [open? #f]
                [on-up void]
                [on-down void])
    (field [state (if open? 'down 'up)])  ; (U 'up 'down 'up-click 'down-click)
    (super-new)
    (set-count 1)
    (set-flags (cons 'handles-events (get-flags)))

    (define/override (copy)
      (new arrow-toggle-snip% (on-up on-up) (on-down on-down) (state state)))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define bitmap
        (case state
          [(up) up-bitmap]
          [(down) down-bitmap]
          [(up-click) up-click-bitmap]
          [(down-click) down-click-bitmap]))
      (cond [(send bitmap ok?)
             (send dc draw-bitmap bitmap x y)]
            [(send dc draw-rectangle x y 10 10)
             (send dc drawline x y 10 10)]))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (set-box/f! descent 0)
      (set-box/f! space 0)
      (set-box/f! lspace 0)
      (set-box/f! rspace 0)
      (set-box/f! w arrow-snip-width)
      (set-box/f! h arrow-snip-height))

    (define/override (on-event dc x y editorx editory evt)
      (let ([snip-evt-x (- (send evt get-x) x)]
            [snip-evt-y (- (send evt get-y) y)])
        (cond
         [(send evt button-down? 'left)
          (set-state (case state
                       [(up) 'up-click]
                       [(down) 'down-click]
                       [else 'down-click]))]
         [(and (send evt button-up? 'left)
               (<= 0 snip-evt-x arrow-snip-width)
               (<= 0 snip-evt-y arrow-snip-height))
          (case state
            [(up up-click) (set-state 'down) (on-down)]
            [(down down-click) (set-state 'up) (on-up)]
            [else (set-state 'down)])]
         [(send evt button-up? 'left)
          (set-state (case state
                       [(up up-click) 'up]
                       [(down down-click) 'down]
                       [else 'up]))]
         [(and (send evt get-left-down)
               (send evt dragging?)
               (<= 0 snip-evt-x arrow-snip-width)
               (<= 0 snip-evt-y arrow-snip-height))
          (set-state (case state
                       [(up up-click) 'up-click]
                       [(down down-click) 'down-click]
                       [else 'up-click]))]
         [(and (send evt get-left-down)
               (send evt dragging?))
          (set-state (case state
                       [(up up-click) 'up]
                       [(down down-click) 'down]
                       [else 'up-click]))]
         [else
          (super on-event dc x y editorx editory evt)])))

    (define/public (get-open)
      (case state [(down down-click) #t] [else #f]))

    (define/public (set-open new-state)
      (set-state (if new-state 'down 'up)))

    (define/private (set-state new-state)
      (unless (eq? state new-state)
        (set! state new-state)
        (let ([admin (get-admin)])
          (when admin
            (send admin needs-update this 0 0 arrow-snip-width arrow-snip-height)))))

    (define/override (adjust-cursor dc x y editorx editory event)
      arrow-snip-cursor)
    ))

(define (set-box/f! b v) (when (box? b) (set-box! b v)))
(define down-bitmap (include-bitmap (lib "icons/turn-down.png") 'png))
(define up-bitmap (include-bitmap (lib "icons/turn-up.png") 'png))
(define down-click-bitmap (include-bitmap (lib "icons/turn-down-click.png") 'png))
(define up-click-bitmap (include-bitmap (lib "icons/turn-up-click.png") 'png))

(define arrow-snip-height
  (max 10
       (send up-bitmap get-height)
       (send down-bitmap get-height)
       (send up-click-bitmap get-height)
       (send down-click-bitmap get-height)))
(define arrow-snip-width
  (max 10
       (send up-bitmap get-width)
       (send down-bitmap get-width)
       (send up-click-bitmap get-width)
       (send down-click-bitmap get-width)))

(define arrow-snip-cursor (make-object cursor% 'arrow))
