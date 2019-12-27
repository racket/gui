#lang racket/base
(require "text-sig.rkt"
         "sig.rkt"
         "../preferences.rkt"
         "interfaces.rkt"
         racket/unit
         racket/class
         racket/math
         mred/mred-sig)

(provide text-line-numbers@)

(define-unit text-line-numbers@
  (import mred^
          [prefix editor: framework:editor^]
          [prefix color-prefs: framework:color-prefs^]
          )
  (export text-line-numbers^)

  (define line-numbers<%>
    (interface ()
      show-line-numbers!
      showing-line-numbers?
      set-line-numbers-color))

  ;; draws line numbers on the left hand side of a text% object
  (define line-numbers-mixin
    (mixin ((class->interface text%) editor:standard-style-list<%>) (line-numbers<%>)
      (inherit begin-edit-sequence
               end-edit-sequence
               in-edit-sequence?
               get-visible-line-range
               get-visible-position-range
               last-line
               line-location
               line-paragraph
               line-start-position
               line-end-position
               get-view-size
               set-padding
               get-padding
               get-start-position
               get-end-position
               position-paragraph
               position-line
               position-location
               paragraph-start-position
               invalidate-bitmap-cache
               get-dc)

      (init-field [line-numbers-color #f])
      (init-field [show-line-numbers? #t])
      ;; whether the numbers are aligned on the left or right
      ;; only two values should be 'left or 'right
      (init-field [alignment 'right])

      (define need-to-setup-padding? #f)
    
      (define/private (number-space)
        (number->string (max (* 10 (add1 (last-line))) 100)))
      ;; add an extra 0 so it looks nice
      (define/private (number-space+1) (string-append (number-space) "0"))

      (define/private (setup-padding)
        (cond
          [(showing-line-numbers?)
           (send padding-dc set-font (get-style-font))
           (define-values (padding-left padding-top padding-right padding-bottom) (get-padding))
           (define new-padding (text-width padding-dc (number-space+1)))
           (set-padding new-padding 0 0 0)
           (unless (= padding-left new-padding)
             (invalidate-bitmap-cache))]
          [else 
           (set-padding 0 0 0 0)]))
    
      ;; call this method with #t or #f to turn on/off line numbers
      (define/public (show-line-numbers! what)
        (set! show-line-numbers? what)
        (setup-padding))

      (define/public (showing-line-numbers?)
        show-line-numbers?)

      (define/public (set-line-numbers-color color)
        (define new-line-numbers-color
          (cond
            [(string? color) (send the-color-database find-color color)]
            [(is-a? color color%) color]
            [else
             (raise-argument-error 'line-numbers-mixin::set-line-numbers-color
                                   (format "~s" '(or/c string? (is-a?/c color%)))
                                   color)]))
        (set! line-numbers-color new-line-numbers-color))

      (define notify-registered-in-list #f)

      (define style-change-notify
        (lambda (style) (unless style (setup-padding))))

      (define/private (get-style)
        (let* ([style-list (editor:get-standard-style-list)]
               [std (or (send style-list
                              find-named-style
                              (editor:get-default-color-style-name))
                        (send style-list find-named-style "Standard")
                        (send style-list basic-style))])
          ;; If the style changes, we should re-check the width of
          ;; drawn line numbers:
          (unless (eq? notify-registered-in-list style-list)
            ;; `notify-on-change' holds the given function weakly:
            (send style-list notify-on-change style-change-notify)
            ;; Avoid registering multiple notifications:
            (set! notify-registered-in-list style-list))
          std))

      (define/private (get-style-foreground)
        (send (get-style) get-foreground))

      (define/private (get-style-font)
        (send (get-style) get-font))

      (define/private (save-dc-state dc)
        (saved-dc-state (send dc get-smoothing)
                        (send dc get-pen)
                        (send dc get-brush)
                        (send dc get-font)
                        (send dc get-text-foreground)
                        (send dc get-text-mode)))

      (define/private (restore-dc-state dc dc-state)
        (send dc set-smoothing (saved-dc-state-smoothing dc-state))
        (send dc set-pen (saved-dc-state-pen dc-state))
        (send dc set-brush (saved-dc-state-brush dc-state))
        (send dc set-font (saved-dc-state-font dc-state))
        (send dc set-text-foreground (saved-dc-state-text-foreground-color dc-state))
        (send dc set-text-mode (saved-dc-state-text-mode dc-state)))

      (define/private (get-foreground)
        (or line-numbers-color (get-style-foreground)))
        
      ;; set the dc stuff to values we want
      (define/private (setup-dc dc)
        (send dc set-smoothing 'aligned)
        (send dc set-text-mode 'transparent)
        (send dc set-font (get-style-font))
        (send dc set-text-foreground (get-foreground)))

      (define/private (lighter-color color)
        (define (integer number)
          (inexact->exact (round number)))
        ;; hue 0-360
        ;; saturation 0-1
        ;; lightness 0-1
        ;; returns rgb as float values with ranges 0-1
        (define (hsl->rgb hue saturation lightness)
          (define (helper x a b)
            (define x* (cond
                         [(< x 0) (+ x 1)]
                         [(> x 1) (- x 1)]
                         [else x]))
            (cond
              [(< (* x 6) 1) (+ b (* 6 (- a b) x))]
              [(< (* x 6) 3) a]
              [(< (* x 6) 4) (+ b (* (- a b) (- 4 (* 6 x))))]
              [else b]))

          (define h (/ hue 360))
          (define a (if (< lightness 0.5)
                        (+ lightness (* lightness saturation))
                        (- (+ lightness saturation) (* lightness saturation))))
          (define b (- (* lightness 2) a))
          (define red (helper (+ h (/ 1.0 3)) a b))
          (define green (helper h a b))
          (define blue (helper (- h (/ 1.0 3)) a b))
          (values red green blue))
        
        ;; red 0-255
        ;; green 0-255
        ;; blue 0-255
        (define (rgb->hsl red green blue)
          (define-values (a b c d)
            (if (> red green)
                (if (> red blue)
                    (if (> green blue)
                        (values red (- green blue) blue 0)
                        (values red (- green blue) green 0))
                    (values blue (- red green) green 4))
                (if (> red blue)
                    (values green (- blue red) blue 2)
                    (if (> green blue)
                        (values green (- blue red) red 2)
                        (values blue (- red green) red 4)))))
          (define hue (if (= a c) 0
                          (let ([x (* 60 (+ d (/ b (- a c))))])
                            (if (< x 0) (+ x 360) x))))
          (define saturation (cond
                               [(= a c) 0]
                               [(< (+ a c) 1) (/ (- a c) (+ a c))]
                               [else (/ (- a c) (- 2 a c))]))
          (define lightness (/ (+ a c) 2))
          (values hue saturation lightness))
        (define-values (hue saturation lightness)
          (rgb->hsl (send color red)
                    (send color green)
                    (send color blue)))
        (define-values (red green blue)
          (hsl->rgb hue saturation (+ 0.5 lightness)))
        (make-object color% (min 255 (integer (* 255 red)))
          (min 255 (integer (* 255 green)))
          (min 255 (integer (* 255 blue)))))

      ;; adjust space so that we are always at the left-most position where
      ;; drawing looks right
      (define/private (left-space dc dx)
        (define left (box 0))
        (define top (box 0))
        (define width (box 0))
        (define height (box 0))
        (send (send this get-admin) get-view left top width height)
        (+ (unbox left) dx))

      (define/augment (after-insert start length)
        (inner (void) after-insert start length)
        ; in case the max line number changed:
        (if (in-edit-sequence?)
            (set! need-to-setup-padding? #t)
            (setup-padding)))

      (define/augment (after-delete start length)
        (inner (void) after-delete start length)
        ; in case the max line number changed:
        (if (in-edit-sequence?)
            (set! need-to-setup-padding? #t)
            (setup-padding)))

      (define/augment (after-edit-sequence)
        (when need-to-setup-padding?
          (set! need-to-setup-padding? #f)
          (setup-padding))
        (inner (void) after-edit-sequence))
    
      (define/private (draw-numbers dc left top right bottom dx dy start-line end-line)
        (unless ((+ left dx) . > . (line-x-coordinate dc dx))
          (define last-paragraph #f)
          (define insertion-para
            (let ([sp (get-start-position)])
              (if (= sp (get-end-position))
                  (position-paragraph sp)
                  #f)))
          (for ([line (in-range start-line end-line)])
            (define y (line-location line))
            (define yb (line-location line #f))
            (define this-paragraph (line-paragraph line))
            (when (and (y . <= . bottom) (yb . >= . top))
              (do-draw-single-line dc dx dy line y last-paragraph 
                                   (and insertion-para
                                        (= insertion-para this-paragraph))))
            (set! last-paragraph this-paragraph))))
    
      (define/public (do-draw-single-line dc dx dy line y last-paragraph is-insertion-line?)
        (define single-space (text-width dc "0"))
        (define-values (single-w single-h _1 _2) (send dc get-text-extent "0"))
        (define view (number->string (add1 (line-paragraph line))))
        (define ls (left-space dc dx))
        (define right-space (text-width dc (number-space)))
        (define final-x
          (+ ls
             (case alignment
               [(left) 0]
               [(right) (- right-space (text-width dc view) single-space)]
               [else 0])))
        (define final-y (+ dy y))
        (cond
          [is-insertion-line?
           (send dc set-pen "black" 1 'transparent)
           (send dc set-brush 
                 (if (get-highlight-text-color)
                     (get-highlight-background-color)
                     (color-prefs:lookup-in-color-scheme
                      'framework:line-numbers-current-line-number-background))
                 'solid)
         
           (send dc draw-rectangle ls final-y (- right-space single-w) single-h)
           (send dc draw-arc 
                 (- (+ ls (- right-space single-w)) single-w) final-y
                 (* 2 single-w) single-h
                 (* pi 3/2) (* pi 1/2))
         
           (define text-fg (send dc get-text-foreground))
           (send dc set-text-foreground (if (get-highlight-text-color)
                                            text-fg
                                            (color-prefs:lookup-in-color-scheme
                                             'framework:line-numbers-current-line-number-foreground)))
           (send dc draw-text view final-x final-y)
           (send dc set-text-foreground text-fg)]
          [(and last-paragraph (= last-paragraph (line-paragraph line)))
           (define text-fg (send dc get-text-foreground))
           (send dc set-text-foreground
                 (color-prefs:lookup-in-color-scheme 'framework:line-numbers-when-word-wrapping))
           (send dc draw-text view final-x final-y)
           (send dc set-text-foreground text-fg)]
          [else
           (define text-fg (send dc get-text-foreground))
           (send dc set-text-foreground
                 (color-prefs:lookup-in-color-scheme 'framework:line-numbers))
           (send dc draw-text view final-x final-y)
           (send dc set-text-foreground text-fg)]))

      ;; draw the line between the line numbers and the actual text
      (define/public (draw-separator dc top bottom dx dy)
        (define line-x (line-x-coordinate dc dx))
        (define line-y1 (+ dy top))
        (define line-y2 (+ dy bottom))
        (send dc set-pen (get-foreground) 1 'solid)
        (send dc draw-line line-x line-y1
              line-x line-y2))
    
      (define/private (line-x-coordinate dc dx)
        (define x (text-width dc (number-space)))
        (+ (left-space dc dx) x))

      ;; `line-numbers-space' will get mutated in the `on-paint' method
      ;; (define line-numbers-space 0)

      (define/private (draw-line-numbers dc left top right bottom dx dy)
        (define saved-dc (save-dc-state dc))
        (setup-dc dc)
        (define start-line (box 0))
        (define end-line (box 0))
        (get-visible-line-range start-line end-line #f)

        (draw-numbers dc left top right bottom dx dy (unbox start-line) (add1 (unbox end-line)))
        (draw-separator dc top bottom dx dy)
        (restore-dc-state dc saved-dc))

      (define/private (text-width dc stuff)
        (define-values (font-width font-height baseline space)
          (send dc get-text-extent stuff (get-style-font)))
        font-width)

      (define/private (text-height dc stuff)
        (define-values (font-width height baseline space)
          (send dc get-text-extent stuff (get-style-font)))
        height)

      (define old-clipping #f)
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (when show-line-numbers?
          (cond
            [before?
             (define left-most (left-space dc dx))
             (set! old-clipping (send dc get-clipping-region))
             (define saved-dc (save-dc-state dc))
             (setup-dc dc)
             (define clipped (make-object region% dc))
             (define copy (make-object region% dc))
             (if old-clipping
                 (send copy union old-clipping)
                 (let ([all (make-object region% dc)])
                   (send all set-rectangle
                         (+ dx left) (+ dy top)
                         (- right left) (- bottom top))
                   (send copy union all)))
             (send clipped set-rectangle
                   0 (+ dy top)
                   (text-width dc (number-space+1))
                   (- bottom top))
             (restore-dc-state dc saved-dc)
             (send copy subtract clipped)
             (send dc set-clipping-region copy)]
            [else
             (send dc set-clipping-region old-clipping)
             (draw-line-numbers dc left top right bottom dx dy)]))
        (super on-paint before? dc left top right bottom dx dy draw-caret))

      (define old-position #f)
      (define/augment (after-set-position)
        (cond
          [(and old-position
                (= (get-start-position)
                   (get-end-position))
                (= (position-line old-position)
                   (position-line (get-start-position))))
           ;; when the line stays the same, don't invalidate anything
           (set! old-position (get-start-position))]
          [else
           (define old-position-before old-position)
           (set! old-position (and (= (get-start-position)
                                      (get-end-position))
                                   (get-start-position)))
           (define single-edit-sequence?
             (and old-position-before 
                  old-position
                  (<= (abs (- (position-paragraph old-position-before)
                              (position-paragraph old-position)))
                      1)))
           (when single-edit-sequence? (begin-edit-sequence #f #f))
           (when old-position-before (invalidate-at-position old-position-before))
           (when old-position (invalidate-at-position old-position))
           (when single-edit-sequence? (end-edit-sequence))])
        (inner (void) after-set-position))
    
      (define/private (invalidate-at-position pos)
        (when (showing-line-numbers?)
          (define dc (get-dc))
          (when dc
            (begin-edit-sequence #f #f)
            (define bx (box 0))
            (define by (box 0))
            (define tw (text-width dc (number-space+1)))
            (define th (text-height dc "0"))
            (define start-para (position-paragraph pos))
            (define start-line (position-line (paragraph-start-position start-para)))
            (let loop ([line start-line])
              (define para (position-paragraph (line-start-position line)))
              (when (= start-para para)
                (position-location (line-start-position line) bx by)
                (invalidate-bitmap-cache (- (unbox bx) tw)
                                         (unbox by) 
                                         tw
                                         th)
                (unless (= line (last-line))
                  (loop (+ line 1)))))
            (end-edit-sequence))))
    
      (super-new)
      (setup-padding)))

  (define-struct saved-dc-state (smoothing pen brush font text-foreground-color text-mode))
  (define padding-dc (new bitmap-dc% [bitmap (make-screen-bitmap 1 1)])))
