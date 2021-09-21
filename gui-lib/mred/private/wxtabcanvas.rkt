#lang racket/base
(require racket/class
         racket/draw
         racket/match
         "wx.rkt"
         "gdi.rkt"
         "wx/common/event.rkt"
         "wx/common/queue.rkt"
         "wxcanvas.rkt"
         (prefix-in compute: "panel-wob.rkt")
         "misc.rkt")

(provide wx-tab-canvas%)

(module+ test (require rackunit))

(define wx-tab-canvas%
  (class* wx-canvas% (wx/client-adjacent<%>)
    (init choices)
    (init-field style font on-close-request on-new-request on-reorder)
    (init-rest init-args)
    (apply super-make-object init-args)

    (define callback void)
    (define/public (set-callback proc) (set! callback proc))

    (define/private (can-reorder?) (member 'can-reorder style))
    (define/private (can-close?) (member 'can-close style))
    (define/private (has-new-button?) (member 'new-button style))

    ;; ----------------------------------------

    (define sibling-client #f)
    (define/public (get-sibling-client) sibling-client)
    (define/public (set-sibling-client c) (set! sibling-client c))

    (inherit refresh
             get-dc
             [do-get-client-size get-client-size]
             set-min-width set-min-height stretchable-in-y)

    (define/private (get-client-size)
      (define x (box 0))
      (define y (box 0))
      (do-get-client-size x y)
      (values (unbox x) (unbox y)))
    
    ;; ----------
    ;; internal state variables
    
    ;; (or/c #f (integer-in 0 (hash-count items)))
    ;; the currently selected tab
    (define selection #f)
    (define/private (set-the-selection s)
      (unless (equal? selection s)
        (set! selection s)
        (refresh)))
    
    ;; hash[natural -o> string]
    ;; indicates the strings on each of the tab items
    (define items (for/hash ([i (in-naturals)]
                             [s (in-list choices)])
                    (values i s)))

    (define/private (number-of-items) (hash-count items))
    (define/private (get-item i) (hash-ref items i))
    (define/private (set-item i v)
      (unless (equal? (hash-ref items i #f) v)
        (set! items (hash-set items i v))
        (show-or-hide-scroll-thumb)
        (update-mouse-over-drawing-state)
        (set-clicked-in #f #f #f #f)
        (refresh)))
    (define/private (set-items is)
      (define new-items
        (for/hash ([i (in-naturals)]
                   [c (in-list is)])
          (values i c)))
      (unless (equal? new-items items)
        (set! items new-items)
        (show-or-hide-scroll-thumb)
        (update-mouse-over-drawing-state)
        (set-clicked-in #f #f #f #f)
        (refresh)))
    (define/private (delete-item n)
      (set! items (delete-item/hash items n))
      (show-or-hide-scroll-thumb)
      (update-mouse-over-drawing-state)
      (set-clicked-in #f #f #f #f)
      (refresh))

    (define/private (reorder-items! former-indices)
      (set! items (for/hash ([old (in-list former-indices)]
                             [i (in-naturals)])
                    (values i (hash-ref items old)))))

    ;; (or/c #f natural?)
    ;; if #f there are no scroll thumbs,
    ;; if a natural, it is the offset
    ;; that we've scrolled over
    (define scroll-offset #f)

    ;; -> boolean
    ;; #t indicates that the scrollbar actually changed
    (define (set-scroll-offset nv)
      (define nv-constrained (and nv (ensure-in-bounds 0 nv (scroll-offset-rightmost))))
      (cond
        [(equal? nv-constrained scroll-offset) #f]
        [else
         (set! scroll-offset nv-constrained)
         (update-mouse-over-drawing-state)
         (refresh)
         #t]))
    
    ;; #t if we are between mouse enter and leave events, #f otherwise
    (define mouse-entered? #f)
    (define/private (set-mouse-entered? nv)
      (unless (equal? mouse-entered? nv)
        (set! mouse-entered? nv)
        (refresh)))
    
    ;; (or/c #f (integer-in 0 (number-of-items)))
    ;; indicates which of the tabs the mouse is currently over
    (define mouse-over #f)
    
    ;; boolean
    ;; when `mouse-over` isn't #f, if this is #t then
    ;; the mouse isn't just over the tab, it is also inside
    ;; the close `x` for that one
    (define mouse-over-close? #f)

    ;; (or/c #f 'left 'right)
    (define mouse-over-thumb #f)

    ;; boolean
    (define mouse-over-new-button? #f)

    (define/private (set-mouse-over new-mouse-over
                                    new-mouse-over-close?
                                    new-mouse-over-thumb
                                    new-mouse-over-new-button?)
      (unless (and (equal? mouse-over new-mouse-over)
                   (equal? mouse-over-close? new-mouse-over-close?)
                   (equal? mouse-over-thumb new-mouse-over-thumb)
                   (equal? mouse-over-new-button? new-mouse-over-new-button?))
        (set! mouse-over new-mouse-over)
        (set! mouse-over-close? new-mouse-over-close?)
        (set! mouse-over-thumb new-mouse-over-thumb)
        (set! mouse-over-new-button? new-mouse-over-new-button?)
        (refresh)))

    ;; (or/c #f (integer-in 0 (number-of-items)))
    ;; indicates which item was clicked in
    ;; (either becuase it is being dragged or for the close button)
    (define clicked-in #f)

    ;; (or/c #f natural?)
    ;; if a natural? then
    ;;  - indicates the offset from the start where the
    ;;    clicked-in tab was first clicked in
    ;; if #f then
    ;;  - close button was clicked in the `clicked-in` tab
    ;; this is meaningful only if clicked-in is not #f
    (define clicked-in-offset #f)

    ;; (or/c 'left 'right #f)
    ;; when not #f, the thumb-timer is running
    ;; when not #f, scroll-offset should also not be #f
    (define clicked-thumb #f)

    ; boolean
    (define clicked-new-button #f)

    (define/private (set-clicked-in new-clicked-in new-clicked-in-offset new-clicked-thumb new-clicked-new-button)
      (unless (and (equal? clicked-in new-clicked-in)
                   (equal? clicked-in-offset new-clicked-in-offset)
                   (equal? clicked-thumb new-clicked-thumb)
                   (equal? clicked-new-button new-clicked-new-button))
        (set! clicked-in new-clicked-in)
        (set! clicked-in-offset new-clicked-in-offset)
        (set! clicked-thumb new-clicked-thumb)
        (set! clicked-new-button new-clicked-new-button)
        (refresh)
        (maybe-start/stop-thumb-timer)))
    
    ;; the current coordinates of the mouse
    (define mouse-x #f)
    (define mouse-y #f)
  
    ;; ----------
    ;; public api
    
    (define/public (append choice)
      (set-item (number-of-items) choice)
      (refresh))
    (define/public (delete n)
      (unless (< n (number-of-items))
        (raise-argument-error 'delete `(integer-in 0 ,(number-of-items)) n))
      (delete-item n)
      (refresh))
      
    (define/public (get-item-label n) (get-item n))
    (define/public (get-number) (number-of-items))
    (define/public (get-selection) selection)
    (define/public (set new-choices)
      (set-items new-choices)
      (refresh))
    (define/public (set-label n label) (set-item n label))
    (define/public (set-selection n) (set-the-selection n))
        
    ;; ----------
    ;; drawing

    (define orig-ascent 0)

    (define/override (on-paint)
      (enable-cache)
      (define dc (get-dc))
      (send dc set-smoothing 'smoothed)
      (send dc set-font font)
      (send dc set-text-foreground
            (if (white-on-black-panel-scheme?)
                "white"
                "black"))

      ;; 1. draw the items that aren't being dragged
      (define-values (cw ch) (get-client-size))
      (define tw (width-of-tab))
      (for ([i (in-range (number-of-items))])
        (define skip-this-drawing-because-it-is-moving?
          (and (equal? i clicked-in)
               (number? clicked-in-offset)))
        (unless skip-this-drawing-because-it-is-moving?
          (define ith-offset (find-ith-offset i))
          (define sp (natural-left-position (+ i ith-offset)))
          (unless (< (+ sp tw) 0) ;; entirely to the left of being visible
            (unless (< cw sp)     ;; entirely to the right of being visible
              (draw-ith-item i sp)))))

      (when (has-new-button?)
        (define sp (natural-left-position (number-of-items)))
        (unless (< (+ sp tw) 0) ;; entirely to the left of being visible
          (unless (< cw sp)     ;; entirely to the right of being visible
            (draw-new-button (number-of-items) sp))))

      ;; 2.
      (draw-lines-between-items)

      ;; 3. draw the one that is being dragged (so it shows up on top)
      (when (and clicked-in clicked-in-offset)
        (draw-ith-item clicked-in
                       (get-left-edge-of-moving-tab)))

      ;; 4.
      (when scroll-offset (draw-scroll-thumbs))

      (disable-cache))

    (define/private (draw-new-button i x-start)
      (define dc (get-dc))
      (define new-icon-start (+ x-start new-button-margin))
      (define-values (cw ch) (get-client-size))
      (define cx (+ new-icon-start (/ size-of-new-icon-circle 2)))
      (define cy (/ ch 2))

      (define text-and-close-foreground-color (text-and-close-icon-bright-color))
      
      (define new-circle-color
        (cond
          [(and clicked-new-button mouse-over-new-button?)
           (mouse-down-over-close-circle-color)]
          [clicked-new-button
           (selected-tab-color)]
          [mouse-over-new-button?
           (mouse-over-close-circle-color)]
          [else (natural-tab-color)]))
        
      (when new-circle-color
        (send dc set-brush new-circle-color 'solid)
        (send dc set-pen "black" 1 'transparent)
        (send dc draw-ellipse
              (- cx (/ size-of-new-icon-circle 2))
              (- cy (/ size-of-new-icon-circle 2))
              size-of-new-icon-circle
              size-of-new-icon-circle))
      
      (send dc set-pen text-and-close-foreground-color 1 'solid)
      (send dc draw-line
            (- cx (/ size-of-new-icon-x 2))
            cy
            (+ cx (/ size-of-new-icon-x 2))
            cy)
      (send dc draw-line
            cx
            (+ cy (/ size-of-new-icon-x 2))
            cx
            (- cy (/ size-of-new-icon-x 2)))
      (void))
    
    (define/private (draw-scroll-thumbs)
      (define dc (get-dc))
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush (scrollthumb-background-color) 'solid)
      (define-values (cw ch) (get-client-size))
      (define-values (sw sh) (get-scroll-thumb-size))
      (send dc draw-rectangle 0 0 sw sh)
      (send dc draw-rectangle (- cw sw) 0 sw sh)
      (define w-ti 1/4)
      (define h-ti 1/6)
      (define points (list (cons (* (- 1 w-ti) sw) (* h-ti ch))
                           (cons (* (- 1 w-ti) sw) (* (- 1 h-ti) ch))
                           (cons (* w-ti sw) (* ch 1/2))))
      (send dc set-brush
            (cond
              [(= scroll-offset 0)
               (scrollthumb-all-the-way-over)]
              [(equal? clicked-thumb 'left)
               (scrollthumb-clicked-foreground-color)]
              [(equal? mouse-over-thumb 'left)
               (scrollthumb-over-foreground-color)]
              [else
               (scrollthumb-foreground-color)])
            'solid)
      (send dc draw-polygon points)
      (send dc set-brush
            (cond
              [(= scroll-offset (scroll-offset-rightmost))
               (scrollthumb-all-the-way-over)]
              [(equal? clicked-thumb 'right)
               (scrollthumb-clicked-foreground-color)]
              [(equal? mouse-over-thumb 'right)
               (scrollthumb-over-foreground-color)]
              [else
               (scrollthumb-foreground-color)])
            'solid)
      (send dc draw-polygon (for/list ([point (in-list points)])
                              (cons (- cw (car point))
                                    (cdr point)))))
    
    (define/private (draw-lines-between-items)
      (define dc (get-dc))
      (send dc set-pen (text-and-close-icon-dim-color) 1 'solid)
      (define-values (cw ch) (get-client-size))
      (for ([i (in-range 1 (number-of-items))])
        (define x (natural-left-position i))
        (send dc draw-line x top-item-margin x (- ch bottom-item-margin))))

    (define/private (draw-ith-item i x-start)
      (define tab-background-color
        (cond
          [(equal? selection i)
           (selected-tab-color)]
          [(or (equal? mouse-over i) (equal? clicked-in i))
           (mouse-over-tab-color)]
          [else
           (natural-tab-color)]))
      (define text-and-close-foreground-color
        (cond
          [(equal? selection i) (text-and-close-icon-bright-color)]
          [else (text-and-close-icon-dim-color)]))
      (define close-circle-color
        (cond
          [(and (equal? clicked-in i) (not clicked-in-offset))
           (if mouse-over-close?
               (mouse-down-over-close-circle-color)
               (mouse-over-close-circle-color))]
          [(and (equal? mouse-over i) mouse-over-close?)
           (mouse-over-close-circle-color)]
          [else tab-background-color]))
      (draw-ith-item/colors i x-start
                            tab-background-color
                            text-and-close-foreground-color
                            close-circle-color))
    
    (define/private (draw-ith-item/colors i x-start
                                          tab-background-color
                                          text-and-close-foreground-color
                                          close-circle-color)
      (define dc (get-dc))
      (define lab (get-item i))
      (define lab-space (- (width-of-tab)
                           horizontal-item-margin
                           horizontal-item-margin
                           (if (can-close?) size-of-close-icon-circle 0)))
      (define-values (cw ch) (get-client-size))

      (send dc set-brush tab-background-color 'solid)
      (send dc set-pen "black" 1 'transparent)
      (send dc draw-rectangle x-start 0 (width-of-tab) ch)

      (send dc set-clipping-rect
            (+ x-start horizontal-item-margin)
            0
            (max 0 lab-space)
            (max 0 ch))
      (send dc set-text-foreground text-and-close-foreground-color)
      (define-values (tw th td ta) (send dc get-text-extent lab))
      (send dc draw-text lab
            (+ x-start horizontal-item-margin)
            (+ top-item-margin (- orig-ascent (- th td)))
            #t)
      (send dc set-clipping-region #f)
      (maybe-draw-fade-at-edge lab lab-space x-start tab-background-color)
      (when (can-close?)
        (draw-close-icon x-start
                         tab-background-color
                         text-and-close-foreground-color
                         close-circle-color)))
    
    (define/private (maybe-draw-fade-at-edge lab lab-space x-start tab-background-color)
      (define dc (get-dc))
      (define-values (cw ch) (get-client-size))
      (define-values (tw th td ta) (send dc get-text-extent lab))
      (when (tw . >= . lab-space)
        #;(assert (lab-space . >= . end-of-label-horizontal-gradient-amount))
        ;; this assert should always be true because the minimum size of
        ;; a tab label should always include a label that is at least as
        ;; big as `end-of-label-horizontal-gradient-amount`
        (define right-edge-of-label
          (+ x-start horizontal-item-margin lab-space))
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (define gradient-stops
          (list (list 0 (make-transparent tab-background-color))
                (list 1 tab-background-color)))
        (send dc set-brush
              (new brush%
                   [gradient
                    (new linear-gradient%
                         [x0 (- right-edge-of-label end-of-label-horizontal-gradient-amount)]
                         [y0 0]
                         [x1 right-edge-of-label]
                         [y1 0]
                         [stops gradient-stops])]))
        (send dc set-pen "black" 1 'transparent)
        (send dc draw-rectangle
              (- right-edge-of-label end-of-label-horizontal-gradient-amount)
              0
              end-of-label-horizontal-gradient-amount
              ch)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)))

    ;; pre: (can-close?) = #t
    (define/private (draw-close-icon x-start
                                     tab-background-color
                                     text-and-close-foreground-color
                                     close-circle-color)
      (define dc (get-dc))
      (define close-icon-start (+ x-start (get-start-of-cross-x-offset)))
      (define-values (cw ch) (get-client-size))
      (define cx (+ close-icon-start (/ size-of-close-icon-circle 2)))
      (define cy (/ ch 2))
      (when close-circle-color
        (send dc set-brush close-circle-color 'solid)
        (send dc set-pen "black" 1 'transparent)
        (send dc draw-ellipse
              (- cx (/ size-of-close-icon-circle 2))
              (- cy (/ size-of-close-icon-circle 2))
              size-of-close-icon-circle
              size-of-close-icon-circle))
      (send dc set-pen text-and-close-foreground-color 1 'solid)
      (send dc draw-line
            (- cx (/ size-of-close-icon-x 2))
            (- cy (/ size-of-close-icon-x 2))
            (+ cx (/ size-of-close-icon-x 2))
            (+ cy (/ size-of-close-icon-x 2)))
      (send dc draw-line
            (- cx (/ size-of-close-icon-x 2))
            (+ cy (/ size-of-close-icon-x 2))
            (+ cx (/ size-of-close-icon-x 2))
            (- cy (/ size-of-close-icon-x 2)))
      (void))

    ;; -------
    ;; mouse movement

    (define/override (on-event evt)
      (define leaving? (send evt leaving?))
      (define entering? (send evt entering?))
      (define left-down (send evt get-left-down))
      (define button-down?-left (send evt button-down? 'left))
      (define time-stamp (send evt get-time-stamp))
      (define dragging? (send evt dragging?))
      (define button-up?-left (send evt button-up? 'left))
      (define last-mouse-x mouse-x)
      (set! mouse-x (send evt get-x))
      (set! mouse-y (send evt get-y))

      (define the-callback void)

      (define-values (cw ch) (get-client-size))
      (cond
        [(or (and leaving?
                  (not left-down))
             (and button-up?-left
                  (or (not (<= 0 mouse-x cw))
                      (not (<= 0 mouse-y ch)))))
         ;; this cannot just be `leaving?` because the mouse being
         ;; down grabs all events to the canvas. So: if the
         ;; button is down we don't believe the leaving event.
         ;; BUT when the mouse is eventually released we do need
         ;; to consider the mouse as having left the window, so we
         ;; use the `x` and `y` coordinates to determine if we're
         ;; outside the window when we do see the up event
         (set-mouse-over #f #f #f #f)
         (set-mouse-entered? #f)
         (set-clicked-in #f #f #f #f)]
        [entering?
         (set-mouse-entered? #t)])

      (when mouse-entered?
        (define-values (mouse-over-tab
                        mx-offset-in-tab
                        mouse-over-close?
                        mouse-over-thumb
                        mouse-over-new-button?)
          (mouse->info mouse-x mouse-y))
        (cond
          [button-down?-left
           (when (and mouse-over-tab (not mouse-over-close?))
             (set-the-selection mouse-over-tab)
             (set! the-callback
                   (λ ()
                     (callback this
                               (new control-event%
                                    [event-type 'tab-panel]
                                    [time-stamp time-stamp])))))
           (cond
             [(can-reorder?)
              (set-clicked-in mouse-over-tab
                              (and (not mouse-over-close?) mx-offset-in-tab)
                              mouse-over-thumb
                              mouse-over-new-button?)]
             [else
              (set-clicked-in (and mouse-over-close? mouse-over-tab)
                              #f
                              mouse-over-thumb
                              mouse-over-new-button?)])
           (set-mouse-over mouse-over-tab mouse-over-close? mouse-over-thumb mouse-over-new-button?)]
          [(and left-down dragging?)
           ;; maybe this next line needs to refresh only when
           ;; we are dragging a tab, not all the time?
           (unless (equal? last-mouse-x mouse-x) (refresh))
           (cond
             [mouse-over-thumb
              (set-clicked-in #f #f mouse-over-thumb #f)]
             [else
              (set-mouse-over #f
                              (and mouse-over-close?
                                   (equal? clicked-in mouse-over-tab))
                              #f
                              mouse-over-new-button?)])]
          [(and button-up?-left clicked-in)
           (cond
             [clicked-in-offset
              (define n (number-of-items))
              (define to-tab (or mouse-over-tab (if (mouse-x . <= . 0)
                                                    0
                                                    (sub1 n))))
              (define former-indices (reordered-list n clicked-in to-tab))
              (when former-indices
                (reorder-items! former-indices)
                (set! the-callback
                      (λ ()
                        (on-reorder former-indices))))]
             [else
              (when (and mouse-over-close?
                         (equal? clicked-in mouse-over-tab))
                (define index clicked-in)
                (set! the-callback
                      (λ ()
                        (on-close-request index))))])
           (set-clicked-in #f #f #f #f)
           (set-mouse-over mouse-over-tab mouse-over-close? mouse-over-thumb mouse-over-new-button?)]
          [button-up?-left
           (when mouse-over-new-button?
             (set! the-callback
                   (λ ()
                     (on-new-request))))
           (set-clicked-in #f #f #f #f)
           (set-mouse-over mouse-over-tab mouse-over-close? mouse-over-thumb mouse-over-new-button?)]
          [else
           (set-mouse-over mouse-over-tab mouse-over-close? mouse-over-thumb mouse-over-new-button?)]))


      (the-callback))

    (define/private (update-mouse-over-drawing-state)
      (cond
        [(and mouse-x mouse-y mouse-entered?)
         (define-values (mouse-over-tab
                         mx-offset-in-tab
                         mouse-over-close?
                         mouse-over-thumb
                         mouse-over-new-button?)
           (mouse->info mouse-x mouse-y))
         (set-mouse-over mouse-over-tab mouse-over-close? mouse-over-thumb mouse-over-new-button?)]
        [else
         (set-mouse-over #f #f #f #f)]))

    ;; -----
    ;; scrolling-related event handling
    
    (define/override (on-size)
      (show-or-hide-scroll-thumb))

    (define/override (on-char evt)
      (case (send evt get-key-code)
        [(wheel-left) (scroll-with-low-priority-event -1)]
        [(wheel-right) (scroll-with-low-priority-event 1)]))

    (define pending-scroll-amount #f)
    (define/private (scroll-with-low-priority-event amount)
      (cond
        [pending-scroll-amount
         (set! pending-scroll-amount (+ amount pending-scroll-amount))]
        [else
         (set! pending-scroll-amount amount)
         (queue-callback
          (λ ()
            (when scroll-offset
              (set-scroll-offset (+ scroll-offset pending-scroll-amount)))
            (set! pending-scroll-amount #f))
          #f)]))

    ;; called when something that might cause scrollbars to appear or disappear
    (define/private (show-or-hide-scroll-thumb)
      (define-values (cw ch) (get-client-size))
      (define need-scrollbars? (cw . < . (min-size-of-all-tabs-together)))
      (cond
        [(and need-scrollbars? (not scroll-offset))
         (set-scroll-offset 0)]
        [(and (not need-scrollbars?) scroll-offset)
         (set-scroll-offset #f)]))

    (define thumb-timer-start-seconds #f)
    (define thumb-timer
      (new timer%
           [notify-callback
            (λ ()
              (cond
                [clicked-thumb
                 (define number-of-seconds-since-click
                   (/ (- (current-inexact-milliseconds) thumb-timer-start-seconds)
                      1000))
                 (define rounded-up-to-nearest-1/2
                   (/ (inexact->exact (ceiling (* number-of-seconds-since-click 2))) 2))
                 (define thumb-speed (+ 1 rounded-up-to-nearest-1/2)) ;; go a little faster
                 (define moved?
                   (set-scroll-offset (if (equal? clicked-thumb 'left)
                                          (- scroll-offset rounded-up-to-nearest-1/2)
                                          (+ scroll-offset rounded-up-to-nearest-1/2))))
                 (cond
                   [moved?
                    (send thumb-timer start thumb-timer-interval #t)]
                   [else
                    (set! thumb-timer-start-seconds #f)])]
                [else
                 (set! thumb-timer-start-seconds #f)]))]))

    (define/private (maybe-start/stop-thumb-timer)
      (cond
        [clicked-thumb
         (set! thumb-timer-start-seconds (current-inexact-milliseconds))
         (send thumb-timer start thumb-timer-interval #t)]
        [else
         (set! thumb-timer-start-seconds #f)
         (send thumb-timer stop)]))
    
    ;; -----
    ;; sizes and positions

    ;; returns the position in the coordinates that
    ;; we should use to draw into the canvas (so
    ;; taking into account the scroll position)
    (define/private (natural-left-position i)
      (define-values (sw sh) (get-scroll-thumb-size))
      (+ (if scroll-offset (+ sw (- scroll-offset)) 0)
         (* i (width-of-tab))))

    ;; determines the delta (0, -1, +1) for the `ith` tab
    ;; due to some other tab being dragged around
    ;; pre: i ≠ clicked-in
    (define/private (find-ith-offset i)
      (cond
        [(and clicked-in clicked-in-offset)
         (define i-left (natural-left-position i))
         (define i-right (+ i-left (width-of-tab)))
         (define i-middle (/ (+ i-left i-right) 2))
         (define left-edge-of-moving-tab (get-left-edge-of-moving-tab))
         (define right-edge-of-moving-tab (+ left-edge-of-moving-tab (width-of-tab)))
         (cond
           [(< i clicked-in)
            (if (left-edge-of-moving-tab . < . i-middle)
                +1
                0)]
           [(< clicked-in i)
            (if (right-edge-of-moving-tab . > . i-middle)
                -1
                0)]
           [else 0])]
        [else 0]))

    (define/private (get-left-edge-of-moving-tab)
      (ensure-in-bounds (natural-left-position 0)
                        (- mouse-x clicked-in-offset)
                        (natural-left-position (- (number-of-items) 1))))

    (define/private (enable-cache)
      (thread-cell-set! wob (compute:white-on-black-panel-scheme?))
      (set! the-width-of-tab (compute-width-of-tab)))
    (define/private (disable-cache)
      (thread-cell-set! wob 'compute-it)
      (set! the-width-of-tab 'compute-it))
    (define the-width-of-tab 'compute-it)
    (define/private (width-of-tab)
      (match the-width-of-tab
        ['compute-it
         (compute-width-of-tab)]
        [(? number? n) n]))
    (define (new-button-width)
      (if (has-new-button?)
          (+ new-button-margin
             size-of-new-icon-circle
             new-button-margin)
          0))
    (define/private (compute-width-of-tab)
      (define-values (cw ch) (get-client-size))
      (define dc (get-dc))
      (define shrinking-required-size (- (/ cw (number-of-items))
                                         (/ (new-button-width) (number-of-items))))

      ;; this is the maximum size that a tab will ever be
      (define unconstrained-tab-size (* (send (send dc get-font) get-point-size) 12))
      (max (min shrinking-required-size
                unconstrained-tab-size)
           (get-min-tab-width)))

    (define/private (white-on-black-panel-scheme?) #f)

    ;; also include the size of the new button if present
    (define/private (min-size-of-all-tabs-together)
      (+ (* (number-of-items) (get-min-tab-width))
         (new-button-width)))

    (define/private (scroll-offset-rightmost)
      (define-values (cw ch) (get-client-size))
      (define-values (sw sh) (get-scroll-thumb-size))
      (define min (min-size-of-all-tabs-together))
      (- min (- cw sw sw)))

    (define/private (get-scroll-thumb-size)
      (define-values (cw ch) (get-client-size))
      (define sw (* ch 2/3))
      (values sw ch))

    (define/private (get-min-width)
      (define-values (sw sh) (get-scroll-thumb-size))
      (define min-number-of-visible-items 2)
      (+ (* min-number-of-visible-items
            (get-min-tab-width))
         sw   ;; left scrollbar
         sw)) ;; right scrollbar

    ;; -> exact natural
    (define/private (get-min-tab-width)
      (define dc (get-dc))
      (define-values (tw th td ta) (send dc get-text-extent "w"))
      ;; width of an item showing only the letter `w`
      (+ horizontal-item-margin
         (max (inexact->exact (ceiling tw))
              end-of-label-horizontal-gradient-amount)
         horizontal-item-margin
         (if (can-close?) size-of-close-icon-circle 0)))

    ;; returns the position where the close x starts, relative
    ;; to the position of the start of the tab itself
    (define/private (get-start-of-cross-x-offset)
      (- (width-of-tab)
         horizontal-item-margin
         (if (can-close?) size-of-close-icon-circle 0)))

    (define/private (mouse->info mx-in-canvas-coordinates my)
      (define-values (sw sh) (get-scroll-thumb-size))
      ;; this `mx` is in coordinates such that 0 is the left
      ;; edge of the tabs. The `mx-in-canvas-coordinates` is
      ;; such that the left edge is the left edge of the window
      ;; (even including the scroll thumbs)
      (define mx (if scroll-offset
                     (+ mx-in-canvas-coordinates scroll-offset (- sw))
                     mx-in-canvas-coordinates))
      (define-values (cw ch) (get-client-size))
      (define tab-candidate-i (floor (/ mx (width-of-tab))))
      (cond
        [(and scroll-offset (<= 0 mx-in-canvas-coordinates sw))
         (values #f #f #f 'left #f)]
        [(and scroll-offset (<= (- cw sw) mx-in-canvas-coordinates cw))
         (values #f #f #f 'right #f)]
        [(<= 0 tab-candidate-i (- (number-of-items) 1))
         (define mx-offset-in-tab (- mx-in-canvas-coordinates (natural-left-position tab-candidate-i)))
         (define start-of-cross (get-start-of-cross-x-offset))
         (define-values (cw ch) (get-client-size))
         (define in-close-x
           (and (can-close?)
                (<= start-of-cross
                    mx-offset-in-tab
                    (+ start-of-cross size-of-close-icon-circle))))
         (define in-close-y
           (and (can-close?)
                (<= (- (/ ch 2) size-of-close-icon-circle)
                    my
                    (+ (/ ch 2) size-of-close-icon-circle))))
         (values tab-candidate-i mx-offset-in-tab (and in-close-x in-close-y) #f #f)]
        [(and (has-new-button?)
              (not clicked-in)
              (= tab-candidate-i (number-of-items))
              (>= mx (+ (* tab-candidate-i (width-of-tab))
                        new-button-margin))
              (<= mx (+ (* tab-candidate-i (width-of-tab))
                        new-button-margin
                        size-of-new-icon-circle))
              ; need to check the height as well for the case where the mouse button was held down
              ; oven the new button and re-enters the widget
              (>= my (/ (- ch size-of-new-icon-circle) 2))
              (<= my (- ch (/ (- ch size-of-new-icon-circle) 2))))
         (values #f #f #f #f #t)]
        [else
         (values #f #f #f #f #f)]))

    (let ()
      (define dc (get-dc))
      (send dc set-smoothing 'smoothed)
      (send dc set-font font)
      (define-values (tw th td ta) (send dc get-text-extent "Xy"))
      (set! orig-ascent (- th td))
      (set-min-width (get-min-width))
      (set-min-height (max (+ top-item-margin
                              (ceiling (inexact->exact th))
                              bottom-item-margin)
                           (if (can-close?) size-of-close-icon-circle 0))))

    (stretchable-in-y #f)))

;; -----
;; size constants

;; space around text in each item horizontally
(define horizontal-item-margin 10)
(define top-item-margin 5)
(define bottom-item-margin 3)
(define new-button-margin (/ horizontal-item-margin 2))

(define end-of-label-horizontal-gradient-amount 16)

(define size-of-close-icon-x 6)
(define size-of-close-icon-circle 12)

(define size-of-new-icon-x 8)
(define size-of-new-icon-circle 16)

;; in msec
(define thumb-timer-interval 30)

;; in pixels (need to be able to speed this up, so this is wrong)
(define thumb-speed 2)

;; ------
;; color constants
(define shade-delta 16)
(define shade-start 20)
(define colors (make-hash))
(define (get-a-color shade-count dark?)
  (unless (hash-ref colors shade-count #f)
    (define offset (+ shade-start (* shade-delta shade-count)))
    (define 255-of (- 255 offset))
    (hash-set! colors
               shade-count
               (cons (make-object color% offset offset offset)
                     (make-object color% 255-of 255-of 255-of))))
  (define pr (hash-ref colors shade-count))
  (if dark? (car pr) (cdr pr)))

(define wob (make-thread-cell 'compute))
(define (white-on-black-panel-scheme?)
  (define v (thread-cell-ref wob))
  (cond
    [(boolean? v) v]
    [else (compute:white-on-black-panel-scheme?)]))

(define (natural-tab-color) (get-a-color 1 (white-on-black-panel-scheme?)))
(define (mouse-over-tab-color) (get-a-color 2 (white-on-black-panel-scheme?)))
(define (selected-tab-color) (get-a-color 3 (white-on-black-panel-scheme?)))
(define (text-and-close-icon-dim-color) (get-a-color 3 (not (white-on-black-panel-scheme?))))
(define (text-and-close-icon-bright-color) (get-a-color 1 (not (white-on-black-panel-scheme?))))
(define (mouse-over-close-circle-color) (get-a-color 6 (white-on-black-panel-scheme?)))
(define (mouse-down-over-close-circle-color) (get-a-color 8 (white-on-black-panel-scheme?)))
(define (scrollthumb-background-color) (get-a-color 4 (white-on-black-panel-scheme?)))
(define (scrollthumb-foreground-color) (get-a-color 9 (white-on-black-panel-scheme?)))
(define (scrollthumb-over-foreground-color) (get-a-color 11 (white-on-black-panel-scheme?)))
(define (scrollthumb-clicked-foreground-color) (get-a-color 13 (white-on-black-panel-scheme?)))
(define (scrollthumb-all-the-way-over) (get-a-color 1 (white-on-black-panel-scheme?)))

(define transparent-cache (make-hasheq))
(define (make-transparent color)
  (hash-ref! transparent-cache
             color
             (λ ()
               (make-object color%
                 (send color red)
                 (send color green)
                 (send color blue)
                 0))))

(define (ensure-in-bounds low x high)
  (max (min x high) low))
(module+ test
  (check-equal? (ensure-in-bounds 0 1 10) 1)
  (check-equal? (ensure-in-bounds 0 8 10) 8)
  (check-equal? (ensure-in-bounds 0 -1 10) 0)
  (check-equal? (ensure-in-bounds 0 11 10) 10))

(define (delete-item/hash items n)
  (define shifted-items
    (for/fold ([items items]) ([i (in-range (+ n 1) (hash-count items))])
      (hash-set items (- i 1) (hash-ref items i))))
  (hash-remove shifted-items (- (hash-count items) 1)))

(module+ test
  (let ()
    (define ht (hash 0 "a"))
    (define new-ht (delete-item/hash ht 0))
    (check-equal? new-ht (hash)))

  (let ()
    (define ht (hash 0 "a"
                     1 "b"
                     2 "c"
                     3 "d"))
    (define new-ht (delete-item/hash ht 0))
    (check-equal? new-ht (hash 0 "b"
                               1 "c"
                               2 "d")))

  (let ()
    (define ht (hash 0 "a"
                      1 "b"
                      2 "c"
                      3 "d"))
    (define new-ht (delete-item/hash ht 2))
    (check-equal? new-ht (hash 0 "a"
                               1 "b"
                               2 "d")))

  (let ()
    (define ht (hash 0 "a"
                     1 "b"
                     2 "c"
                     3 "d"))
    (define new-ht (delete-item/hash ht 3))
    (check-equal? new-ht (hash 0 "a"
                               1 "b"
                               2 "c"))))

;; computes mapping of new index to old index when
;; clicked-in is dragged to mouse-over
;; returns #f if nothing would change
(define (reordered-list number-of-items clicked-in mouse-over)
  (cond
    [(= clicked-in mouse-over) #f]
    [else
     (for/list ([i (in-range number-of-items)])
       (cond
         [(or (i . < . (min clicked-in mouse-over))
              (i . > . (max clicked-in mouse-over)))
          i]
         [(= i mouse-over) clicked-in]
         [(clicked-in . < . mouse-over) (add1 i)]
         [else (sub1 i)]))]))

(module+ test
  (check-equal? (reordered-list 1 0 0) #f)
  (check-equal? (reordered-list 2 1 0) '(1 0))
  (check-equal? (reordered-list 5 2 3) '(0 1 3 2 4))
  (check-equal? (reordered-list 5 3 2) '(0 1 3 2 4))
  (check-equal? (reordered-list 6 2 5) '(0 1 3 4 5 2))
  (check-equal? (reordered-list 6 5 1) '(0 5 1 2 3 4))
  (check-equal? (reordered-list 6 2 2) #f))
