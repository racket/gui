#lang racket/base
(provide text-inline-overview@)

(require racket/unit
         mred/mred-sig
         racket/class
         "../preferences.rkt"
         "text-sig.rkt"
         "sig.rkt"
         (prefix-in unsafe: (only-in racket/draw/private/bitmap make-bitmap)))

(define-local-member-name
  get-primary-bmp
  get-secondary-bmp
  maybe-queue-do-a-little-work?
  do-a-little-work
  do-all-of-the-work
  up-to-date?
  get-invalid-start
  get-invalid-end)

(define maximum-bitmap-width 200)

(define-unit text-inline-overview@
  (import mred^
          [prefix color-prefs: framework:color-prefs^]
          [prefix text: text-max-width-paragraph^])
  (export text-inline-overview^)
  (init-depend text-max-width-paragraph^)

  (define transparent-color (make-object color% 255 255 255 0))
  (define extra-blue-parts-margin 10)
  (define arrow-cursor (make-object cursor% 'arrow))

  (define inline-overview<%> (interface ((class->interface text%))
                               get-inline-overview-enabled?
                               set-inline-overview-enabled?
                               is-inline-overview-work-pending?
                               ))

  (define (inline-overview-mixin super%)
    (inline-overview-mpw-mixin
     (text:max-width-paragraph-mixin super%)))

  (define inline-overview-mpw-mixin
    (mixin ((class->interface text%) text:max-width-paragraph<%>) (inline-overview<%>)
      (define is-do-a-little-work-enqueued? #f)
      (define invalid-start #f)
      (define invalid-end #f)
      (define primary-bmp #f)
      (define secondary-bmp #f)
      (define bmp-width 0)
      (define enabled? #f)
      ;; known-blank : nat
      ;; the lines after and including known-blank are known to be blank in the bitmap
      (define known-blank +inf.0)

      (define/public (is-inline-overview-work-pending?) is-do-a-little-work-enqueued?)

      (define/public (get-inline-overview-enabled?) enabled?)
      (define/public (set-inline-overview-enabled? _e?)
        (define e? (and _e? #t))
        (unless (equal? e? enabled?)
          (set! enabled? e?)
          (cond
            [enabled?
             (reset-entire-overview)]
            [else
             (when (get-admin) (invalidate-entire-overview-region #f))
             (set! bmp-width 0)
             (set! scratch-string #f)
             (set! primary-bmp #f)
             (set! secondary-bmp #f)
             (set! known-blank +inf.0)])))

      (define/private (reset-entire-overview)
        (define h (last-paragraph))
        (define previous-bmp-width bmp-width)
        (update-bmp-width)
        (define to-create-h (+ h 20))
        (unless (and primary-bmp
                     (= (send primary-bmp get-width) bmp-width)
                     (= (send primary-bmp get-height) to-create-h))
          (set! primary-bmp (unsafe:make-bitmap bmp-width to-create-h))
          (set! secondary-bmp (unsafe:make-bitmap bmp-width to-create-h))
          (set! known-blank 0))
        (when (and (> previous-bmp-width bmp-width) (get-admin))
          (invalidate-entire-overview-region
           #t
           #:extra-left-width
           (- previous-bmp-width bmp-width)))
        (union-invalid 0 h)
        (maybe-queue-do-a-little-work?))

      (define/public (get-primary-bmp) primary-bmp)
      (define/public (get-secondary-bmp) secondary-bmp)
    
      (super-new)

      (define loading-file? #f)
      (define/augment (on-load-file filename format)
        (set! loading-file? #t)
        (inner (void) on-load-file filename format))
      (define/augment (after-load-file success?)
        (inner (void) after-load-file success?)
        (set! loading-file? #f)
        (reset-entire-overview))

      (define/augment (after-max-width-paragraph-change)
        (when enabled?
          ;; this guard on `enabled?` ensures that we don't ask for
          ;; the width of the widest line (which can be slow) when
          ;; the bitmap overview is not shown
          (when (bitmap-too-big?)
            (reset-entire-overview)))
        (inner (void) after-max-width-paragraph-change))
    
      (define/augment (after-insert start len)
        (inner (void) after-insert start len)
        (when (and enabled? (not loading-file?))
          (set! width-could-have-changed-since-last-do-a-little-work? #t)
          (define ps (position-paragraph start))
          (define pe (position-paragraph (+ start len)))
          (cond
            [(= ps pe)
             ;; edit is just on a single line,
             ;; so we mark that line as invalid
             (union-invalid ps pe)]
            [else
             ;; the insertion spans multiple lines, so we
             ;; copy the known good stuff to the secondary
             ;; bitmap and swap the bitmaps and just give
             ;; up on tracking which parts are blank
             (set! known-blank +inf.0)
             (define current-h (send primary-bmp get-height))
             (define w (send primary-bmp get-width))
             (define h
               (cond
                 [(>= (last-paragraph) current-h)
                  (if (< (* 2 current-h) (last-paragraph))
                      (+ 20 (last-paragraph))
                      (* 2 current-h))]
                 [else current-h]))
             (define make-new-bitmaps? (not (= current-h h)))
             (define bmp-to-draw-into
               (if make-new-bitmaps?
                   (unsafe:make-bitmap bmp-width h)
                   secondary-bmp))
             (define bdc (new bitmap-dc% [bitmap bmp-to-draw-into]))
             (unless make-new-bitmaps? (send bdc erase))
             (send bdc draw-bitmap-section primary-bmp 0 0 0 0 w ps)
             (send bdc draw-bitmap-section primary-bmp 0 pe 0 ps w (- current-h ps))
             (when make-new-bitmaps?
               (set! secondary-bmp bmp-to-draw-into)
               (set! primary-bmp (unsafe:make-bitmap bmp-width h))
               (set! known-blank 0))
             (swap-bitmaps)
             (union-invalid ps pe)])
          (maybe-queue-do-a-little-work?)))
    
      (define/augment (on-delete start len)
        (inner (void) on-delete start len)
        (when (and enabled? (not loading-file?))
          (set! width-could-have-changed-since-last-do-a-little-work? #t)
          (define ps (position-paragraph start))
          (define pe (position-paragraph (+ start len)))
          (cond
            [(= ps pe)
             (union-invalid ps ps)]
            [else
             (set! known-blank +inf.0)
             (define h (send secondary-bmp get-height))
             (define w (send secondary-bmp get-width))
             (define invalid-region-size (- (+ (last-paragraph) 1) pe))

             (define bdc (new bitmap-dc% [bitmap secondary-bmp]))
             (send bdc erase)
         
             ; copy stuff before start of invalid region to other bitmap
             (send bdc draw-bitmap-section primary-bmp 0 0 0 0 w ps)
         
             ; copy stuff after end of invalid region to other bitmap
             (send bdc draw-bitmap-section primary-bmp
                   0 ps
                   0 pe
                   w invalid-region-size)
             (send bdc set-pen "black" 1 'transparent)
             (send bdc set-brush transparent-color 'solid)
             (send bdc draw-rectangle
                   0
                   (- (last-paragraph) 1)
                   w
                   (- h ps (- invalid-region-size pe)))
         
             (swap-bitmaps)
         
             (union-invalid ps ps)])
          (maybe-queue-do-a-little-work?)))

      (define/augment (after-change-style start len) 
        (inner (void) after-change-style start len)
        (when enabled?
          (define ps (position-paragraph start))
          (define pe (position-paragraph (+ start len)))
          (union-invalid ps pe)
          (maybe-queue-do-a-little-work?)))

      (define style-has-changed-callback-pending? #f)
      (define/override (style-has-changed s)
        (unless style-has-changed-callback-pending?
          (set! style-has-changed-callback-pending? #t)
          (queue-callback
           (λ ()
             (set! style-has-changed-callback-pending? #f)
             (reset-entire-overview))
           #f))
        (super style-has-changed s))

      (define last-time-on-paint-called #f)
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (when (and enabled? before? (get-admin))
          (define-values (view-height
                          bitmap-first-visible-paragraph
                          top-paragraph
                          bot-paragraph
                          bitmap-x-coordinate
                          bitmap-y-coordinate)
            (get-bitmap-placement-info))

          (when (or (<= left bitmap-x-coordinate right)
                    (<= left (+ bitmap-x-coordinate bmp-width) right))
            (define visible-height (- bot-paragraph top-paragraph))
            (define old-pen (send dc get-pen))
            (define old-brush (send dc get-brush))
            (send dc set-pen "black" 1 'transparent)
            (send dc set-brush
                  (color-prefs:lookup-in-color-scheme 'framework:program-contour-current-location-bar)
                  'solid)
            (send dc draw-rectangle
                  (- (+ dx bitmap-x-coordinate) extra-blue-parts-margin)
                  (+ dy
                     bitmap-y-coordinate
                     (- top-paragraph bitmap-first-visible-paragraph))
                  (+ extra-blue-parts-margin (send primary-bmp get-width))
                  visible-height)
            (define top-region-to-skip
              (if (<= bitmap-y-coordinate top (+ bitmap-y-coordinate view-height))
                  (- top bitmap-y-coordinate)
                  0))
            (define bottom-region-to-skip
              (if (<= bitmap-y-coordinate bottom (+ bitmap-y-coordinate view-height))
                  (- (+ bitmap-y-coordinate view-height) bottom)
                  0))
            (send dc draw-bitmap-section primary-bmp
                  (+ dx bitmap-x-coordinate)
                  (+ dy bitmap-y-coordinate top-region-to-skip)
                  0
                  (+ bitmap-first-visible-paragraph top-region-to-skip)
                  (send primary-bmp get-width)
                  (- view-height
                     top-region-to-skip
                     bottom-region-to-skip))

            (send dc set-brush old-brush)
            (send dc set-pen old-pen))))

      (define/override (after-scroll-to)
        (when enabled?
          ;; we a scroll happens, we need to redraw
          ;; the the entire overview region, as scrolling
          ;; invalidates only the newly exposed region
          (when (get-admin)
            (invalidate-entire-overview-region #f))))

      (define/private (invalidate-entire-overview-region just-union? #:extra-left-width [extra-left-width 0])
        (define-values (view-height
                        bitmap-first-visible-paragraph
                        top-paragraph
                        bot-paragraph
                        bitmap-x-coordinate
                        bitmap-y-coordinate)
          (get-bitmap-placement-info))
        (define x (- bitmap-x-coordinate extra-blue-parts-margin extra-left-width))
        (define w (+ bmp-width extra-left-width extra-blue-parts-margin))
        (cond
          [just-union?
           (union-region-to-invalidate x
                                       bitmap-y-coordinate
                                       w
                                       view-height)]
          [else
           (invalidate-bitmap-cache x
                                    bitmap-y-coordinate
                                    w
                                    view-height)]))
    
      ;; pre: admin is not #f
      (define/private (get-bitmap-placement-info)
        ;; bitmap-first-visible-paragraph is which paragraph of the first
        ;;   line of the bitmap that should be visible at the top of the screen.
        ;; top-paragraph is the paragraph of the main body of text that's
        ;;   at the top of the window
        (define-values (x y view-width view-height) (get-view-info))
        (define view-right (+ x view-width))

        ;; in editor coordinates, the location where the upper-left part of the
        ;; drawn part of the bitmap goes (the drawing uses draw-bitmap-section
        ;; so this won't be where the upper-left corner of the
        ;; content of the bitmap goes)
        (define bitmap-x-coordinate (- view-right (send primary-bmp get-width)))
        (define bitmap-y-coordinate y)

        (define top-paragraph (xy-to-paragraph x y))
        (define bot-paragraph (xy-to-paragraph x (+ y view-height)))
        (define bitmap-first-visible-paragraph
          (get-bitmap-first-visible-paragraph view-height top-paragraph))
        (values view-height
                bitmap-first-visible-paragraph
                top-paragraph
                bot-paragraph
                bitmap-x-coordinate
                bitmap-y-coordinate))

      ;; pre: admin is not #f
      (define/private (get-view-info)
        (define xb (box 0))
        (define yb (box 0))
        (define wb (box 0))
        (define hb (box 0))
        (define admin (get-admin))
        (send admin get-view xb yb wb hb)
        (values (unbox xb) (unbox yb) (unbox wb) (unbox hb)))
    
      (define/private (get-bitmap-first-visible-paragraph view-height top-paragraph)
        (cond
          [(<= (last-paragraph) view-height) 0]
          ;top
          [(> (- (ceiling (/ view-height 2)) top-paragraph) 0) 0]
          [(< (- (last-paragraph) top-paragraph) (ceiling (/ view-height 2))) 
           (- (last-paragraph) view-height)]
          ; subtract half the size of the editor from where we want to
          ; draw the blue box to find the start of the bitmap
          [else 
           (- top-paragraph (/ view-height 2))]))
    
      (define/override (on-default-event event)
        (cond
          [(and enabled? (get-admin))
           (cond
             [(send event button-up? 'left)
              (define-values (mx my) (dc-location-to-editor-location
                                      (send event get-x)
                                      (send event get-y)))
              (define-values (view-height
                              bitmap-first-visible-paragraph
                              top-paragraph
                              bot-paragraph
                              bitmap-x-coordinate
                              bitmap-y-coordinate)
                (get-bitmap-placement-info))
              (cond
                [(mouse-event-in-range? event mx my
                                        bitmap-x-coordinate bitmap-y-coordinate
                                        bot-paragraph bitmap-first-visible-paragraph)
                 (define p (+ (paragraph-start-position
                               (inexact->exact
                                (ceiling (+ (- my bitmap-y-coordinate)
                                            bitmap-first-visible-paragraph))))
                              (inexact->exact
                               (ceiling
                                (- mx bitmap-x-coordinate)))))
                 (define by (box 0))
                 (position-location p #f by)
                 (begin-edit-sequence)
                 (set-position p p)
                 (scroll-editor-to 0 (- (unbox by) (/ view-height 2))
                                   0 view-height
                                   #t 'none)
                 (end-edit-sequence)]
                [else (super on-default-event event)])]
             [else (super on-default-event event)])]
          [else (super on-default-event event)]))

      (define/override (adjust-cursor event)
        (cond
          [(and enabled? (get-admin))
           (define-values (mx my) (dc-location-to-editor-location
                                   (send event get-x)
                                   (send event get-y)))
           (define-values (view-height
                           bitmap-first-visible-paragraph
                           top-paragraph
                           bot-paragraph
                           bitmap-x-coordinate
                           bitmap-y-coordinate)
             (get-bitmap-placement-info))
           (cond
             [(mouse-event-in-range? event mx my
                                     bitmap-x-coordinate bitmap-y-coordinate
                                     bot-paragraph bitmap-first-visible-paragraph)
              arrow-cursor]
             [else (super adjust-cursor event)])]
          [else (super adjust-cursor event)]))

      (define/private (mouse-event-in-range? event mx my
                                             bitmap-x-coordinate bitmap-y-coordinate
                                             bot-paragraph bitmap-first-visible-paragraph)
        (and (<= (- bitmap-x-coordinate extra-blue-parts-margin)
                 mx
                 (+ bitmap-x-coordinate (send primary-bmp get-width)))
             (<= bitmap-y-coordinate
                 my
                 (+ bitmap-y-coordinate
                    (- (last-paragraph) bitmap-first-visible-paragraph)))))
      

      (inherit invalidate-bitmap-cache
               split-snip get-snip-position
               paragraph-start-position
               dc-location-to-editor-location
               paragraph-end-position
               position-paragraph
               position-location
               last-paragraph
               get-character
               insert
               delete
               get-text
               find-snip
               get-canvas
               get-admin
               find-position
               set-position
               scroll-editor-to
               begin-edit-sequence
               end-edit-sequence
               get-max-width-paragraph)
    
      (define/private (xy-to-paragraph x y)
        (position-paragraph (find-position x y)))
    
      (define/private (swap-bitmaps)
        (define temp primary-bmp)      
        (set! primary-bmp secondary-bmp)
        (set! secondary-bmp temp))
    
      (define/private (update-invalid-start nstart)
        (set! invalid-start nstart))
    
      (define/private (union-invalid start end)
        (set! invalid-start 
              (if invalid-start
                  (min start invalid-start)
                  start))
        (set! invalid-end 
              (if invalid-end
                  (max end invalid-end)
                  end)))
      (define/private (clear-invalid)
        (set! invalid-start #f)
        (set! invalid-end #f))

      (define/private (bitmap-too-big?)
        (define mwp (get-max-width-paragraph))
        (define text-width (- (paragraph-end-position mwp)
                              (paragraph-start-position mwp)))
        (< (+ text-width 50) bmp-width))

      (define/private (update-bmp-width)
        (define mwp (get-max-width-paragraph))
        ;; use `max` to ensure that we always have a non-empty bitmap
        (define text-width (max 1 (- (paragraph-end-position mwp)
                                     (paragraph-start-position mwp))))
        (unless (<= text-width bmp-width (+ text-width 40))
          (set! bmp-width (min maximum-bitmap-width (+ 20 text-width))))
        (when (or (not scratch-string)
                  (< (string-length scratch-string) bmp-width))
          (set! scratch-string (make-string bmp-width))))
    
      (define/public (maybe-queue-do-a-little-work?)
        (let loop ()
          (cond
            [(or (up-to-date?) is-do-a-little-work-enqueued?)
             (void)]
            [else
             (set! is-do-a-little-work-enqueued? #t)
             (queue-callback
              (λ ()
                (set! is-do-a-little-work-enqueued? #f)
                (when enabled?
                  (do-a-little-work)
                  (loop)))
              #f)])))

      (define/public (do-all-of-the-work)
        (let loop ()
          (unless (up-to-date?)
            (do-a-little-work)
            (loop))))
      (define width-could-have-changed-since-last-do-a-little-work? #f)
      (define/public (do-a-little-work)
        (cond
          [(up-to-date?)
           (void)]
          [(or invalid-start invalid-end)
           (define start-time (current-inexact-milliseconds))
           (define bmp-width-changed?
             (cond
               [width-could-have-changed-since-last-do-a-little-work?
                (set! width-could-have-changed-since-last-do-a-little-work? #f)
                (define previous-bmp-width bmp-width)
                (update-bmp-width)
                (when (and (get-admin) (> previous-bmp-width bmp-width))
                  ;; if the bitmap gets narrower,
                  ;; the invalidate-entire-overview-region
                  ;; below won't invalidate a big enough region
                  (invalidate-entire-overview-region #t #:extra-left-width (- previous-bmp-width bmp-width)))
                (not (= previous-bmp-width bmp-width))]
               [else #f]))
           (when bmp-width-changed?
             (define new-primary-bmp (unsafe:make-bitmap bmp-width (send primary-bmp get-height)))
             (define new-secondary-bmp (unsafe:make-bitmap bmp-width (send primary-bmp get-height)))
             (define bdc (new bitmap-dc% [bitmap primary-bmp]))
             (send bdc set-bitmap new-primary-bmp)
             (send bdc erase)
             (send bdc draw-bitmap primary-bmp 0 0)
             (set! primary-bmp new-primary-bmp)
             (set! secondary-bmp new-secondary-bmp)
             (set! known-blank 0))
           (define start-of-updated-lines invalid-start)
           (define end-of-updated-lines
             (let loop ([line-line-last-snip #f]
                        [y invalid-start])
               (define relevant-portion-known-blank? (>= y known-blank))
               (cond
                 [(= y invalid-end)
                  (update-one-line y line-line-last-snip relevant-portion-known-blank?)
                  (clear-invalid)
                  y]
                 [else
                  (define this-line-last-snip
                    (update-one-line y line-line-last-snip relevant-portion-known-blank?))
                  (cond
                    [(< (+ start-time 10) (current-inexact-milliseconds))
                     (update-invalid-start (+ 1 y))
                     y]
                    [else
                     (loop this-line-last-snip
                           (+ y 1))])])))
           (set! known-blank (if invalid-start
                                 (max known-blank invalid-start)
                                 +inf.0))
           (when (get-admin)
             (define-values (view-height
                             bitmap-first-visible-paragraph
                             top-paragraph
                             bot-paragraph
                             bitmap-x-coordinate
                             bitmap-y-coordinate)
               (get-bitmap-placement-info))
             (cond
               [bmp-width-changed?
                (invalidate-entire-overview-region #t)]
               [else
                (union-region-to-invalidate
                 (- bitmap-x-coordinate extra-blue-parts-margin)
                 (+ (- bitmap-y-coordinate
                       bitmap-first-visible-paragraph)
                    start-of-updated-lines)
                 (+ bmp-width extra-blue-parts-margin)
                 (+ (- end-of-updated-lines start-of-updated-lines) 1))]))]
          [else ;; region-to-invalidate must be #t here
           (invalidate-region-to-invalidate)]))

      ;; (or/c #f (vector l t r b <time-first-invalid-region-was-known>))
      (define region-to-invalidate #f)
      (define/private (union-region-to-invalidate l t w h)
        (define r (+ l w))
        (define b (+ t h))
        (cond
          [region-to-invalidate
           (set! region-to-invalidate
                 (vector (min l (vector-ref region-to-invalidate 0))
                         (min t (vector-ref region-to-invalidate 1))
                         (max r (vector-ref region-to-invalidate 2))
                         (max b (vector-ref region-to-invalidate 3))
                         (vector-ref region-to-invalidate 4)))]
          [else
           (set! region-to-invalidate (vector l t r b (current-inexact-milliseconds)))])

        (cond
          [(< (+ (vector-ref region-to-invalidate 4) 1000.) (current-inexact-milliseconds))
           ;; when we have had an invalid region for a while, then
           ;; just go ahead an invalidate it so the user sees some progress
           (invalidate-region-to-invalidate)]
          [else
           ;; when we have an invalid region that starts in the viewing
           ;; region but continues on beyond the viewing region, then
           ;; we draw it right now (even if we are not finished computing
           ;; the entire overview)
           ;; the thought is that the user might not be scrolling, and
           ;; so this region is the one that they're going to be
           ;; looking at, so lets draw it right away instead of making
           ;; them wait until we've figured out the overview for the
           ;; entire file (which might be big)
           (define-values (x y view-width view-height) (get-view-info))
           (define view-b (+ y view-height))
           (define top-inside? (<= y (vector-ref region-to-invalidate 1) view-b))
           (define bottom-inside? (<= y (vector-ref region-to-invalidate 3) view-b))
           (when (and top-inside? (not bottom-inside?))
             (invalidate-region-to-invalidate))]))

      ;; pre: region-to-invalidate =/= #f
      (define/private (invalidate-region-to-invalidate)
        (invalidate-bitmap-cache (vector-ref region-to-invalidate 0)
                                 (vector-ref region-to-invalidate 1)
                                 (- (vector-ref region-to-invalidate 2)
                                    (vector-ref region-to-invalidate 0))
                                 (- (vector-ref region-to-invalidate 3)
                                    (vector-ref region-to-invalidate 1)))
        (set! region-to-invalidate #f))

      (define scratch-string #f)
      ;; update-one-line : nat bitmap-dc% (or/c #f snip%) nat -> (or/c #f snip%)
      ;; it returns the last snip on the line (if it has one to return)
      ;; and that value is passed back into the function so it can shortcircuit
      ;; the call to `find-snip`
      (define/private (update-one-line y last-lines-last-snip relevant-portion-known-blank?)
        (define para-start (paragraph-start-position y))
        (define this-lines-first-snip
          (if last-lines-last-snip
              (send-generic last-lines-last-snip snip-next)
              (find-snip para-start 'after-or-none)))
        (let loop ([snip this-lines-first-snip]
                   [x 0])
          (cond
            [snip
             ;; we didn't run past the end of the buffer
             ;; iterate over the current snip's content
             ;; filling in this line of the bitmap
             (define count (send-generic snip snip-get-count))
             (send-generic snip snip-get-text! scratch-string 0 (min maximum-bitmap-width count) 0)
             (cond
               [(and (= 1 count)
                     (eq? #\newline (string-ref scratch-string 0)))
                (copy-single-line-bytes-out y x)
                (unless relevant-portion-known-blank?
                  (erase-rest-of-line primary-bmp bmp-width x y))
                snip]
               [(< x maximum-bitmap-width)
                (setup-color (send (send-generic snip snip-get-style) get-foreground))
                (for ([ch (in-string scratch-string)]
                      [i (in-range x (min maximum-bitmap-width (+ count x)))])
                  (cond
                    [(char-whitespace? ch)
                     (set-transparent-pixel i)]
                    [else
                     (set-colored-pixel i)]))
                (loop (send-generic snip snip-next)
                      (+ x count))]
               [else
                (copy-single-line-bytes-out y x)
                #f])]
            [else
             (copy-single-line-bytes-out y x)
             (unless relevant-portion-known-blank?
               (erase-rest-of-line primary-bmp bmp-width x y))
             #f])))

      (define single-line-bytes (make-bytes (* maximum-bitmap-width 4)))
      (define (copy-single-line-bytes-out y _w)
        (define w (min maximum-bitmap-width _w))
        (send-generic primary-bmp bitmap-set-argb-pixels 0 y w 1 single-line-bytes))

      (define color-bytes (bytes 0 0 0 0))
      (define/private (setup-color c)
        (define α (send-generic c color-alpha))
        (cond
          [(= α 1.0)
           (bytes-set! color-bytes 0 255)]
          [else
           (bytes-set! color-bytes 0 (inexact->exact (round (* α 255))))])
        (bytes-set! color-bytes 1 (send-generic c color-red))
        (bytes-set! color-bytes 2 (send-generic c color-green))
        (bytes-set! color-bytes 3 (send-generic c color-blue)))
      (define/private (set-colored-pixel x)
        (bytes-copy! single-line-bytes (* x 4) color-bytes))
      (define/private (set-transparent-pixel x)
        (bytes-copy! single-line-bytes (* x 4) transparent-bytes 0 4))

      (define/public (up-to-date?)
        (and (not invalid-start)
             (not invalid-end)
             (not region-to-invalidate)))
      (define/public (get-invalid-start)
        invalid-start)
      (define/public (get-invalid-end)
        invalid-end)))

  (define snip-get-style (generic snip% get-style))
  (define snip-get-count (generic snip% get-count))
  (define snip-get-text! (generic snip% get-text!))
  (define snip-next (generic snip% next))
  (define bitmap-set-argb-pixels (generic bitmap% set-argb-pixels))
  (define color-red (generic color% red))
  (define color-green (generic color% green))
  (define color-blue (generic color% blue))
  (define color-alpha (generic color% alpha)))

;; this is a cheat, as we get bitmap% both from
;; the unit import and from the a direct require
;; of racket/gui/base, meaning that we're not really
;; parameterized over mred^
(require (only-in racket/gui/base bitmap%))

;; provided for the test suite
(provide set-transparent-pixels
         erase-rest-of-line
         transparent-bytes-count
         maximum-bitmap-width
         do-all-of-the-work
         get-primary-bmp)

(define bitmap-set-argb-pixels (generic bitmap% set-argb-pixels))
(define transparent-bytes
  (bytes 0 255 255 255 0 255 255 255
         0 255 255 255 0 255 255 255
         0 255 255 255 0 255 255 255
         0 255 255 255 0 255 255 255))
(define transparent-bytes-count (/ (bytes-length transparent-bytes) 4))
(define (set-transparent-pixels bmp x y n)
  (send-generic bmp bitmap-set-argb-pixels x y n 1 transparent-bytes))

(define (erase-rest-of-line bmp w x y)
  (cond
    [(>= x w) (void)]
    [else
     (for ([x (in-range x (- w transparent-bytes-count) transparent-bytes-count)])
       (set-transparent-pixels bmp x y transparent-bytes-count))
     (define leftover (modulo (- w x) transparent-bytes-count))
     (cond
       [(= 0 leftover)
        (set-transparent-pixels bmp
                                (- w transparent-bytes-count)
                                y
                                transparent-bytes-count)]
       [else
        (set-transparent-pixels bmp (- w leftover) y leftover)])]))
