#lang racket/base
(require racket/unit
         racket/class
         data/queue
         mred/mred-sig
         string-constants
         "../preferences.rkt"
         "interfaces.rkt"
         "sig.rkt"
         "text-sig.rkt")

(provide text-basic@)

(define-unit text-basic@
  (import mred^
          [prefix icon: framework:icon^]
          [prefix finder: framework:finder^]
          [prefix color-model: framework:color-model^]
          [prefix editor: framework:editor^])
  (export text-basic^)
  (init-depend framework:editor^)

  
  ;; rectangles : (or/c #f (listof rectangle))
  ;;  #f => range information needs to be computed for this rectangle
  (define-struct range ([start #:mutable] 
                        [end #:mutable]
                        caret-space?
                        style color 
                        adjust-on-insert/delete?
                        key
                        [rectangles #:mutable]) #:inspector #f)
  (define-struct rectangle (left top right bottom style color) #:inspector #f)

  (define (build-rectangle left top right bottom style color [info (λ () "")])
    (unless (or (symbol? right) (symbol? left))
      (when (right . < . left)
        (error 'build-rectangle "found right to the right of left: ~s; info ~a"
               (list left top right bottom style color)
               (info))))
    (unless (or (symbol? top) (symbol? bottom))
      (when (bottom . < . top)
        (error 'build-rectangle "found bottom above top: ~s; info ~a"
               (list left top right bottom style color)
               (info))))
    (make-rectangle left top right bottom style color))
  

  (define-values (register-port-name! lookup-port-name)
    ;; port-name->editor-ht: (hashof symbol (weakboxof editor:basic<%>))
    ;; Maintains a mapping from port names back to their respective editors.
    (let ([port-name->editor-ht (make-weak-hasheq)])
    
      ;; register-port-name-to-editor!: symbol editor<%> -> void
      ;; Registers the editor's port name.
      (define (register-port-name! a-port-name an-editor)
        (hash-set! port-name->editor-ht a-port-name (make-weak-box an-editor)))
    
      ;; lookup-port-name: symbol -> (or/c editor:basic<%> #f)
      ;; Given a port name, tries to get the editor with that name.
      (define (lookup-port-name a-port-name)
        (let ([a-weak-box (hash-ref port-name->editor-ht a-port-name #f)])
          (cond
            [(not a-weak-box)
             #f]
            [else
             (weak-box-value a-weak-box)])))
    
      (values register-port-name! lookup-port-name)))

  (define basic<%> text:basic<%>)

  (define hollow-ellipse-pen-size 3)
  (define hollow-ellipse-embiggen 4)

  (define highlight-range-mixin
    (mixin (editor:basic<%> (class->interface text%)) ()
  
      (inherit invalidate-bitmap-cache
               last-position 
               position-locations
               position-location
               position-line 
               line-start-position
               line-end-position
               get-style-list
               get-admin)

      (define highlight-tmp-color (make-object color% 0 0 0))

      (define ranges-deq (make-queue))
    
      (define/public-final (get-highlighted-ranges) 
        (for/list ([x (in-queue ranges-deq)]) x))
    
      (define/private (recompute-range-rectangles)
        (set! pending-ranges (queue->list ranges-deq))
        (unless recompute-callback-running?
          (set! recompute-callback-running? #t)
          (queue-callback (λ () (run-recompute-range-rectangles)) #f)))
    
      (define pending-ranges '())
      (define recompute-callback-running? #f)
    
      (define/private (run-recompute-range-rectangles)
        (when (get-admin)
          ;; when there is no admin, then the position-location information
          ;; is bogus, so we just give up trying to recompute this information
        
          (define done-time (+ (current-inexact-milliseconds) 20)) 
          (define did-something? #f)
          (let loop ([left #f]
                     [top #f]
                     [right #f]
                     [bottom #f])
            (cond
              [(and did-something? ((current-inexact-milliseconds) . >= . done-time))
               (final-invalidate left top right bottom)
               (queue-callback
                (λ () (run-recompute-range-rectangles))
                #f)]
              [(null? pending-ranges)
               (final-invalidate left top right bottom)
               (set! recompute-callback-running? #f)]
              [else
               (set! did-something? #t)
               (define a-range (car pending-ranges))
               (set! pending-ranges (cdr pending-ranges))
               (define old-rectangles (range-rectangles a-range))
               (cond
                 [old-rectangles
                  (define new-rectangles (compute-rectangles a-range))
                  (cond
                    [(equal? new-rectangles old-rectangles)
                     (loop left top right bottom)]
                    [else
                     (define-values (new-left new-top new-right new-bottom)
                       (for/fold ([left left] [top top] [right right] [bottom bottom]) 
                                 ([r (in-list new-rectangles)])
                         (join-rectangles left top right bottom r)))
                     (define-values (both-left both-top both-right both-bottom)
                       (for/fold ([left new-left] [top new-top] [right new-right] [bottom new-bottom]) 
                                 ([r (in-list old-rectangles)])
                         (join-rectangles left top right bottom r)))
                     (set-range-rectangles! a-range new-rectangles)
                     (loop both-left both-top both-right both-bottom)])]
                 [else 
                  ;; when old-rectangles is #f, that means that this
                  ;; range has been removed from the ranges-deq, so 
                  ;; can just skip over it here.
                  (loop left top right bottom)])]))))
    
      (define/private (join-rectangles left top right bottom r)
        (define this-left
          (if (number? (rectangle-left r))
              (adjust r (rectangle-left r) -)
              0.0))
        (define this-right
          (if (number? (rectangle-right r))
              (adjust r (rectangle-right r) +)
              'display-end))
        (define this-top (adjust r (rectangle-top r) -))
        (define this-bottom (adjust r (rectangle-bottom r) +))
        (if (and left top right bottom)
            (values (min this-left left)
                    (min this-top top)
                    (if (and (number? this-right) (number? right))
                        (max this-right right)
                        'display-end)
                    (max this-bottom bottom))
            (values this-left 
                    this-top
                    this-right
                    this-bottom)))
    
      (define/private (final-invalidate left top right bottom)
        (when left
          (let ([width (if (number? right) (- right left) 'display-end)]
                [height (if (number? bottom) (- bottom top) 'display-end)])
            (when (and (or (symbol? width) (> width 0))
                       (or (symbol? height) (> height 0)))
              (invalidate-bitmap-cache left top width height)))))
    
      (define/private (adjust r w f)
        (+ w (f (case (rectangle-style r)
                  [(dot) 8]
                  [(hollow-ellipse) (+ hollow-ellipse-pen-size hollow-ellipse-embiggen)]
                  [else 0]))))
    
      (define b1 (box 0))
      (define b2 (box 0))
      (define b3 (box 0))
      (define/private (compute-rectangles range)
        (define start (range-start range))
        (define end (range-end range))
        (define caret-space? (range-caret-space? range))
        (define style (range-style range))
        (define color (range-color range))
        (define lp (last-position))
        (define-values (start-eol? end-eol?) (if (= start end) (values #f #f) (values #f #t)))
        (define-values (end-x top-end-y bottom-end-y)
          (begin (position-locations end b1 b2 #f b3 end-eol? #t)
                 (values (unbox b1) 
                         (unbox b2)
                         (unbox b3))))
        (define-values (start-x top-start-y bottom-start-y)
          (begin 
            (position-locations start b1 b2 #f b3 start-eol? #t)
            (values (if (and caret-space? 
                             (not (= start end))
                             (<= (+ (unbox b1) 1) end-x))
                        (+ 1 (unbox b1))
                        (unbox b1))
                    (unbox b2)
                    (unbox b3))))
        (cond
          ;; the position-location values can be strange when
          ;; this condition is true, so we just bail out.
          [(or (> start lp) (> end lp)) '()]
          [(= top-start-y top-end-y)
           (list (build-rectangle start-x
                                  top-start-y
                                  (if (= end-x start-x)
                                      (+ end-x 1)
                                      end-x)
                                  bottom-start-y
                                  style
                                  color
                                  (λ () (format "start = ~s end = ~s filename = ~s content = ~s"
                                                start end 
                                                (send this get-filename)
                                                (send this get-text 0 100)))))]
          [(or (equal? style 'hollow-ellipse)
               (equal? style 'ellipse))
           (define end-line (position-line end end-eol?))
           (let loop ([l (min start-x end-x)]
                      [r (max start-x end-x)]
                      [line (position-line start start-eol?)])
           
             (cond
               [(> line end-line) 
                (list (build-rectangle l top-start-y
                                       r bottom-end-y
                                       style color))]
               [else
                (define line-start (line-start-position line))
                (define line-end (line-end-position line))
                (position-location line-start b1 #f #t)
                (position-location line-end b2 #f #t)
                (loop (min (unbox b1) (unbox b2) l)
                      (max (unbox b1) (unbox b2) r)
                      (+ line 1))]))]
          [else
           (list (build-rectangle start-x top-start-y
                                  'right-edge bottom-start-y
                                  style color)
                 (build-rectangle 'left-edge bottom-start-y
                                  'right-edge top-end-y
                                  style color)
                 (build-rectangle 'left-edge top-end-y
                                  end-x bottom-end-y
                                  style color))]))
    
      (define/augment (after-insert insert-start insert-len)
        (for ([r (in-queue ranges-deq)])
          (when (range-adjust-on-insert/delete? r)
            (define rstart (range-start r))
            (define rend (range-end r))
            (cond
              [(<= insert-start rstart) 
               (set-range-start! r (+ rstart insert-len))
               (set-range-end! r (+ rend insert-len))]
              [(<= insert-start rend)
               (set-range-end! r (+ rend insert-len))])))
        (inner (void) after-insert insert-start insert-len))
      (define/augment (after-delete delete-start delete-len)
        (define delete-end (+ delete-start delete-len))
        (for ([r (in-queue ranges-deq)])
          (when (range-adjust-on-insert/delete? r)
            (define rstart (range-start r))
            (define rend (range-end r))
            (cond
              [(<= delete-end rstart)
               (set-range-start! r (- rstart delete-len))
               (set-range-end! r (- rend delete-len))]
              [(<= delete-start rstart delete-end rend)
               (define new-len (- rend delete-end))
               (set-range-start! r delete-start)
               (set-range-end! r (+ delete-start new-len))]
              [(<= rstart delete-start delete-end rend)
               (define new-len (- rend delete-end))
               (set-range-start! r delete-start)
               (set-range-end! r (- rend delete-len))]
              [(<= rstart delete-start rend)
               (set-range-end! r delete-end)])))
        (inner (void) after-delete delete-start delete-len))
   
      (define/augment (on-reflow)
        (recompute-range-rectangles)
        (inner (void) on-reflow))
    
      (define/augment (after-load-file success?)
        (inner (void) after-load-file success?)
        (when success?
          (set! ranges-deq (make-queue))))
        
      (define/public (highlight-range start end in-color 
                                      [caret-space? #f]
                                      [priority 'low]
                                      [style 'rectangle] 
                                      #:adjust-on-insert/delete? [adjust-on-insert/delete? #f]
                                      #:key [key #f])
        (unless (let ([exact-pos-int?
                       (λ (x) (and (integer? x) (exact? x) (x . >= . 0)))])
                  (and (exact-pos-int? start)
                       (exact-pos-int? end)))
          (error 'highlight-range
                 "expected first two arguments to be non-negative exact integers, got: ~e ~e"
                 start
                 end))
        (unless (<= start end)
          (error 'highlight-range
                 "expected start to be less than end, got ~e ~e" start end))
        (unless (or (eq? priority 'high) (eq? priority 'low))
          (error 'highlight-range
                 "expected priority argument to be either 'high or 'low, got: ~e"
                 priority))
        (unless (or (is-a? in-color color%)
                    (and (string? in-color)
                         (send the-color-database find-color in-color)))
          (error 'highlight-range
                 "expected a color or a string in the-color-database for the third argument, got ~e" 
                 in-color))
        (unless (memq style '(rectangle hollow-ellipse ellipse dot))
          (error 'highlight-range
                 "expected one of 'rectangle, 'ellipse 'hollow-ellipse, or 'dot as the style, got ~e"
                 style))
        (when (eq? style 'dot)
          (unless (= start end)
            (error 'highlight-range
                   "when the style is 'dot, the start and end regions must be the same")))
      
        (define color (if (is-a? in-color color%)
                          in-color
                          (send the-color-database find-color in-color)))
        (define l (make-range start end caret-space? style color adjust-on-insert/delete? key #f))
        (if (eq? priority 'high)
            (enqueue! ranges-deq l)
            (enqueue-front! ranges-deq l))
        (set-range-rectangles! l (compute-rectangles l))
        (invalidate-rectangles (range-rectangles l))
        (unless adjust-on-insert/delete?
          (λ () 
            (unhighlight-range start end color caret-space? style))))
        
      (define/public (unhighlight-range start end in-color [caret-space? #f] [style 'rectangle])
        (define color (if (is-a? in-color color%)
                          in-color
                          (send the-color-database find-color in-color)))
        (unhighlight-ranges
         (λ (r-start r-end r-color r-caret-space? r-style r-adjust-on-insert/delete? r-key)
           (and (equal? start r-start)
                (equal? end r-end)
                (equal? color r-color)
                (equal? caret-space? r-caret-space?)
                (equal? style r-style)))
         #t))
    
      (define/public (unhighlight-ranges/key key)
        (unhighlight-ranges
         (λ (r-start r-end r-color r-caret-space? r-style r-adjust-on-insert/delete? r-key)
           (equal? r-key key))))
    
      (define/public (unhighlight-ranges pred [just-one? #f])
        (define left #f)
        (define top #f)
        (define right #f)
        (define bottom #f)
        (define found-one? #f)
        (queue-filter! 
         ranges-deq
         (λ (a-range)
           (cond
             [(and just-one? found-one?) #t]
             [(pred (range-start a-range)
                    (range-end a-range)
                    (range-color a-range)
                    (range-caret-space? a-range)
                    (range-style a-range)
                    (range-adjust-on-insert/delete? a-range)
                    (range-key a-range))
              (set! found-one? #t)
              (for ([rect (in-list (range-rectangles a-range))])
                (set!-values (left top right bottom)
                             (join-rectangles left top right bottom rect)))
              (set-range-rectangles! a-range #f)
              #f]
             [else
              #t])))
        (final-invalidate left top right bottom))
    
      (define/private (invalidate-rectangles rectangles)
        (let loop ([left #f]
                   [top #f]
                   [right #f]
                   [bottom #f]
                   [rectangles rectangles])
          (cond
            [(null? rectangles)
             (final-invalidate left top right bottom)]
            [else
             (define-values (new-left new-top new-right new-bottom)
               (join-rectangles left top right bottom (car rectangles)))
             (loop new-left new-top new-right new-bottom
                   (cdr rectangles))])))
    
      (define/override (on-paint before dc left-margin top-margin right-margin bottom-margin
                                 dx dy draw-caret)
        (super on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
        (when before
          (define-values (view-x view-y view-width view-height)
            (let ([admin (get-admin)])
              (if admin
                  (let ([b1 (box 0)]
                        [b2 (box 0)]
                        [b3 (box 0)]
                        [b4 (box 0)])
                    (send admin get-view b1 b2 b3 b4)
                    (values (unbox b1)
                            (unbox b2)
                            (unbox b3)
                            (unbox b4)))
                  (values left-margin top-margin right-margin bottom-margin))))
          (define old-pen (send dc get-pen))
          (define old-brush (send dc get-brush))
          (define old-smoothing (send dc get-smoothing))
          (define last-color #f)
          (send dc set-smoothing 'aligned)
          (for ([range (in-queue ranges-deq)])
            (for ([rectangle (in-list (range-rectangles range))])
              (define left (if (number? (rectangle-left rectangle))
                               (rectangle-left rectangle)
                               view-x))
              (define top (rectangle-top rectangle))
              (define right (if (number? (rectangle-right rectangle))
                                (rectangle-right rectangle)
                                (+ view-x view-width)))
              (define bottom (rectangle-bottom rectangle))
              (when (and (or (<= left-margin left right-margin)
                             (<= left-margin right right-margin)
                             (<= left left-margin right-margin right))
                         (or (<= top-margin top bottom-margin)
                             (<= top-margin bottom bottom-margin)
                             (<= top top-margin bottom-margin bottom)))
                (define width (if (right . <= . left) 0 (- right left)))
                (define height (if (bottom . <= . top) 0 (- bottom top)))
                (define color (let ([rc (rectangle-color rectangle)])
                                (cond
                                  [(not (= 1 (send rc alpha))) rc]
                                  [(and last-color (eq? last-color rc))
                                   rc]
                                  [rc
                                   (set! last-color #f)
                                   (send dc try-color rc highlight-tmp-color)
                                   (if (<= (color-model:rgb-color-distance
                                            (send rc red)
                                            (send rc green)
                                            (send rc blue)
                                            (send highlight-tmp-color red)
                                            (send highlight-tmp-color green)
                                            (send highlight-tmp-color blue))
                                           18)
                                       (begin (set! last-color rc)
                                              rc)
                                       #f)]
                                  [else 
                                   (set! last-color #f)
                                   rc])))
                (when color
                  (case (rectangle-style rectangle)
                    [(dot)
                     (let ([cx left]
                           [cy bottom])
                       (send dc set-pen "black" 1 'transparent)
                       (send dc set-brush color 'solid)
                       (send dc draw-ellipse (+ dx cx -3) (+ dy cy -3) 6 6))]
                    [(hollow-ellipse)
                     (send dc set-pen color hollow-ellipse-pen-size 'solid)
                     (send dc set-brush "black" 'transparent)
                     (send dc draw-ellipse 
                           (+ dx left (- hollow-ellipse-embiggen))
                           (+ dy top (- hollow-ellipse-embiggen))
                           (+ width (+ hollow-ellipse-embiggen hollow-ellipse-embiggen))
                           (+ height (+ hollow-ellipse-embiggen hollow-ellipse-embiggen)))]
                    [(rectangle)
                     (send dc set-pen color 1 'transparent)
                     (send dc set-brush color 'solid)
                     (send dc draw-rectangle (+ left dx) (+ top dy) width height)]
                    [(ellipse)
                     (send dc set-pen color 1 'transparent)
                     (send dc set-brush color 'solid)
                     (send dc draw-ellipse (+ left dx) (+ top dy) width height)])))))
          (send dc set-smoothing old-smoothing)
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)))
    
      (super-new)))
  
  (define other-basics-mixin
    (mixin (editor:basic<%> (class->interface text%)) ()
      (inherit get-canvas split-snip get-snip-position
               begin-edit-sequence end-edit-sequence
               set-autowrap-bitmap
               delete find-snip 
               get-style-list change-style
               position-line line-start-position
               get-filename get-end-position)

      (define/override (blink-caret)
        (unless (preferences:get 'framework:caret-blink-disable?)
          (super blink-caret)))

      (define/public (get-fixed-style)
        (send (get-style-list) find-named-style "Standard"))

      (define port-name-identifier #f)
      (define port-name-unsaved-name "unsaved-editor")
      (define/public-final (set-port-unsaved-name p)
        (unless (equal? port-name-unsaved-name p)
          (set! port-name-unsaved-name p)
          (set! port-name-identifier #f)
          (after-set-port-unsaved-name)))
      (define/public (after-set-port-unsaved-name) (void))
      (define/public (get-port-name)
        (let* ([b (box #f)]
               [n (get-filename b)])
          (cond
            [(or (unbox b) (not n))
             (unless port-name-identifier
               (set! port-name-identifier (string->uninterned-symbol port-name-unsaved-name))
               (register-port-name! port-name-identifier this))
             port-name-identifier]
            [else n])))
      (define/public (port-name-matches? id)
        (let ([filename (get-filename)])
          (or (and (path? id)
                   (path? filename)
                   (or (equal? id filename) ;; "fast path" check
                       (equal? (normal-case-path (simplify-path filename #f))
                               (normal-case-path (simplify-path id #f)))))
              (and (symbol? port-name-identifier)
                   (symbol? id)
                   (equal? port-name-identifier id)))))
    
    
      (define styles-fixed? #f)
      (public get-styles-fixed set-styles-fixed)
      (define (get-styles-fixed) styles-fixed?)
      (define (set-styles-fixed b) (set! styles-fixed? b))
    
      (define edition 0)
      (define/public (get-edition-number) edition)
    
      (define/augment (on-insert start len)
        (begin-edit-sequence #t #f)
        (inner (void) on-insert start len))
      (define/augment (after-insert start len)
        (set! edition (+ edition 1))
        (when styles-fixed?
          (change-style (get-fixed-style) start (+ start len) #f))
        (inner (void) after-insert start len)
        (end-edit-sequence))
      (define/augment (after-delete start len)
        (set! edition (+ edition 1))
        (inner (void) after-delete start len))

      (define/public (move-to dest-edit start end dest-position)
        (unless (and (<= 0 start) (<= 0 end) (<= 0 dest-position))
          (error 'move-to
                 "expected start, end, and dest-pos to be non-negative"))
        (when (> start end)
          (error 'move-to
                 "expected start position smaller than end position"))
        (define (release-or-copy snip)
          (cond
            [(send snip release-from-owner) snip]
            [else
             (define copy (send snip copy))
             (define snip-start (get-snip-position snip))
             (define snip-end (+ snip-start (send snip get-count)))
             (delete snip-start snip-end)
             copy]))
        (define move-to-self? (object=? this dest-edit))
        (unless (or (= start end) (and move-to-self? (<= start dest-position end)))
          (let loop ([current-start start]
                     [current-end (min end (get-end-position))]
                     [current-dest dest-position])
            (split-snip current-start)
            (split-snip current-end)
            (define snip (find-snip current-end 'before-or-none))
            (cond
              [(or (not snip) (< (get-snip-position snip) current-start)) (void)]
              [else
               (define released/copied (release-or-copy snip))
               (define snip-count (send released/copied get-count))
               (define new-start
                 (cond
                   [(or (not move-to-self?) (> current-dest current-start)) current-start]
                   [else (+ current-start snip-count)]))
               (define new-end
                 (cond
                   [(and move-to-self? (< current-dest current-end)) current-end]
                   [else (- current-end snip-count)]))
               (define new-dest
                 (cond
                   [(or (not move-to-self?) (< current-dest current-start)) current-dest]
                   [else (- current-dest snip-count)]))
               (send dest-edit insert released/copied new-dest new-dest)
               (loop new-start new-end new-dest)]))))

      (define/public (copy-to dest-edit start end dest-position)
        (unless (and (<= 0 start) (<= 0 end) (<= 0 dest-position))
          (error 'copy-to
                 "expected start, end, and dest-pos to be non-negative"))
        (when (> start end)
          (error 'copy-to
                 "expected start position smaller than end position"))
        (unless (= start end)
          (split-snip start)
          (split-snip end)
          (define snips
            (let loop ([snip (find-snip end 'before)] [snips '()])
              (cond
                [(or (not snip) (< (get-snip-position snip) start)) (reverse snips)]
                [else (loop (send snip previous) (cons (send snip copy) snips))])))
          (for ([snip (in-list snips)])
            (send dest-edit insert snip dest-position dest-position))))
    
      (define/public (move/copy-to-edit dest-edit start end dest-position
                                        #:try-to-move? [try-to-move? #t])
        (unless (and (<= 0 start) (<= 0 end) (<= 0 dest-position))
          (error 'move/copy-to-edit
                 "expected start, end, and dest-pos to be non-negative"))
        (when (> start end)
          (error 'move/copy-to-edit
                 "expected start position smaller than end position"))
        (cond
          [try-to-move? (move-to dest-edit start end dest-position)]
          [else (copy-to dest-edit start end dest-position)]))
    
      (public initial-autowrap-bitmap)
      (define (initial-autowrap-bitmap) (icon:get-autowrap-bitmap))
    
      (define/override (put-file directory default-name)
        (let* ([canvas (get-canvas)]
               [parent (and canvas (send canvas get-top-level-window))])
          (finder:put-file default-name
                           directory
                           #f
                           (string-constant select-file)
                           #f
                           ""
                           parent)))
    
      (define/public (get-start-of-line pos)
        (line-start-position (position-line pos)))
    
      (super-new)
      (set-autowrap-bitmap (initial-autowrap-bitmap))))

  (define (basic-mixin %)
    (class* (highlight-range-mixin (other-basics-mixin %)) (basic<%>)
      (super-new))))
