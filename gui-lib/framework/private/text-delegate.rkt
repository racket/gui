#lang racket/base
(require racket/unit
         racket/class
         mred/mred-sig
         "text-sig.rkt"
         "sig.rkt")

(provide text-delegate@)

(define-unit text-delegate@
  (import mred^
          text-basic^
          [prefix racket: framework:racket^])
  (export text-delegate^)

  (define delegate<%> (interface (basic<%>) 
                        get-delegate
                        set-delegate))

  (define small-version-of-snip%
    (class snip%
      (init-field big-snip)
      (define width 0)
      (define height 0)
      (define/override (get-extent dc x y wb hb db sb lb rb)
        (set/f! db 0)
        (set/f! sb 0)
        (set/f! lb 0)
        (set/f! rb 0)
        (let ([bwb (box 0)]
              [bhb (box 0)])
          (send big-snip get-extent dc x y bwb bhb #f #f #f #f)
          (let* ([cw (send dc get-char-width)]
                 [ch (send dc get-char-height)]
                 [w (floor (/ (unbox bwb) cw))]
                 [h (floor (/ (unbox bhb) ch))])
            (set/f! wb w)
            (set/f! hb h)
            (set! width w)
            (set! height h))))
    
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (send dc draw-rectangle x y width height))
      (define/override (copy) (instantiate small-version-of-snip% () (big-snip big-snip)))
      (super-instantiate ())))

  (define 1-pixel-string-snip%
    (class string-snip%
      (init-rest args)
      (inherit get-text get-count set-count get-flags)
      (define/override (split position first second)
        (let* ([str (get-text 0 (get-count))]
               [new-second (make-object 1-pixel-string-snip%
                             (substring str position (string-length str)))])
          (set-box! first this)
          (set-box! second new-second)
          (set-count position)
          (void)))
      (define/override (copy)
        (let ([cpy (make-object 1-pixel-string-snip%
                     (get-text 0 (get-count)))])
          (send cpy set-flags (get-flags))))
      (define/override (partial-offset dc x y len)
        len)
      (define/override (get-extent dc x y wb hb db sb lb rb)
        (cond
          [(memq 'invisible (get-flags))
           (set/f! wb 0)]
          [else
           (set/f! wb (get-count))])
        (set/f! hb 1)
        (set/f! db 0)
        (set/f! sb 0)
        (set/f! lb 0)
        (set/f! rb 0))
    
      (define cache-function void)
      (define cache-str (make-string 1 #\space))
      (define container-str (make-string 1 #\space))
    
      (inherit get-text!)
    
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([len (get-count)])
          (unless (= len (string-length container-str))
            (set! container-str (make-string len #\space))
            (set! cache-function void))
          (get-text! container-str 0 len 0)
          (unless (string=? container-str cache-str)
            (set! cache-function (for-each/sections container-str))
            (set! cache-str (make-string len #\space))
            (get-text! cache-str 0 len 0)))
        (when (<= top y bottom)
          (cache-function dc x y)))
    
      (apply super-make-object args)))

  ;; for-each/sections : string -> dc number number -> void
  (define (for-each/sections str)
    (let ([str-len (string-length str)])
      (cond
        [(zero? str-len)
         void]
        [else
         (let loop ([i 1]
                    [len 1]
                    [start 0]
                    [blank? (char-whitespace? (string-ref str 0))])
           (cond
             [(= i str-len)
              (if blank?
                  void
                  (λ (dc x y)
                    (send dc draw-line (+ x start) y (+ x start (- len 1)) y)))]
             [else
              (let ([white? (char-whitespace? (string-ref str i))])
                (cond
                  [(eq? white? blank?)
                   (loop (+ i 1) (+ len 1) start blank?)]
                  [else
                   (let ([res (loop (+ i 1) 1 i (not blank?))])
                     (if blank?
                         res
                         (λ (dc x y)
                           (res dc x y)
                           (send dc draw-line (+ x start) y (+ x start (- len 1)) y))))]))]))])))


  #;
  (let ()
    ;; test cases for for-each/section
    (define (run-fe/s str)
      (let ([calls '()])
        ((for-each/sections str)
         (new (class object%
                (define/public (draw-line x1 y1 x2 y2)
                  (set! calls (cons (list x1 x2) calls)))
                (super-new)))
         0
         0)
        calls))
  
    (printf "framework/private/text.rkt: ~s\n" 
            (list
             (equal? (run-fe/s "") '())
             (equal? (run-fe/s "a") '((0 0)))
             (equal? (run-fe/s " ") '())
             (equal? (run-fe/s "ab") '((0 1)))
             (equal? (run-fe/s "ab c") '((0 1) (3 3)))
             (equal? (run-fe/s "a bc") '((0 0) (2 3)))
             (equal? (run-fe/s "a b c d") '((0 0) (2 2) (4 4) (6 6)))
             (equal? (run-fe/s "a b c d    ") '((0 0) (2 2) (4 4) (6 6)))
             (equal? (run-fe/s "abc def ghi") '((0 2) (4 6) (8 10)))
             (equal? (run-fe/s "abc   def   ghi") '((0 2) (6 8) (12 14))))))

  (define 1-pixel-tab-snip%
    (class tab-snip%
      (init-rest args)
      (inherit get-text get-count set-count get-flags)
      (define/override (split position first second)
        (let* ([str (get-text 0 (get-count))]
               [new-second (make-object 1-pixel-string-snip%
                             (substring str position (string-length str)))])
          (set-box! first this)
          (set-box! second new-second)
          (set-count position)
          (void)))
      (define/override (copy)
        (let ([cpy (make-object 1-pixel-tab-snip%)])
          (send cpy set-flags (get-flags))))
    
      (inherit get-admin)
      (define/override (get-extent dc x y wb hb db sb lb rb)
        (set/f! wb 0)
        (let ([admin (get-admin)])
          (when admin
            (let ([ed (send admin get-editor)])
              (when (is-a? ed text%)
                (let ([len-b (box 0)]
                      [tab-width-b (box 0)]
                      [in-units-b (box #f)])
                  (send ed get-tabs len-b tab-width-b in-units-b)
                  (when (and (or (equal? (unbox len-b) 0)
                                 (equal? (unbox len-b) null))
                             (not (unbox in-units-b)))
                    (let ([tabspace (unbox tab-width-b)])
                      (set/f! wb (tabspace . - . (x . modulo . tabspace))))))))))
      
        (set/f! hb 0)
        (set/f! db 0)
        (set/f! sb 0)
        (set/f! lb 0)
        (set/f! rb 0))
    
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (void))
      (apply super-make-object args)))

  (define (set/f! b n)
    (when (box? b)
      (set-box! b n)))

  (define delegate-mixin
    (mixin (basic<%>) (delegate<%>) 
      (inherit split-snip find-snip 
               get-snip-position
               find-first-snip 
               get-style-list set-tabs)
    
      (define linked-snips #f)
    
      (define/private (copy snip)
        (let ([new-snip
               (cond
                 [(is-a? snip tab-snip%)
                  (let ([new-snip (make-object 1-pixel-tab-snip%)])
                    (send new-snip insert (string #\tab) 1)
                    new-snip)]
                 [(is-a? snip string-snip%)
                  (make-object 1-pixel-string-snip%
                    (send snip get-text 0 (send snip get-count)))]
                 [else 
                  (let ([new-snip
                         (instantiate small-version-of-snip% ()
                           (big-snip snip))])
                    (hash-set! linked-snips snip new-snip)
                    new-snip)])])
          (send new-snip set-flags (send snip get-flags))
          (send new-snip set-style (send snip get-style))
          new-snip))
    
      (define delegate #f)
      (inherit get-highlighted-ranges)
      (define/public-final (get-delegate) delegate)
      (define/public-final (set-delegate _d)
        (when delegate
          ;; the delegate may be in a bad state because we've killed the pending todo
          ;; items; to clear out the bad state, end any edit sequences, and unhighlight
          ;; any highlighted ranges. The rest of the state is reset if the editor
          ;; is ever installed as a delegate again (by refresh-delegate)
          (let loop ()
            (when (send delegate in-edit-sequence?)
              (send delegate end-edit-sequence)
              (loop)))
          (for ([range (in-list (send delegate get-highlighted-ranges))])
            (send delegate unhighlight-range
                  (range-start range)
                  (range-end range)
                  (range-color range)
                  (range-caret-space? range)
                  (range-style range))))
      
        (set! delegate _d)
        (set! linked-snips (if _d
                               (make-hasheq)
                               #f))
        (refresh-delegate))
    
      (define/private (refresh-delegate)
        (when delegate 
          (refresh-delegate/do-work)))
    
      (define/private (refresh-delegate/do-work)
        (send delegate begin-edit-sequence)
        (send delegate lock #f)
        (when (is-a? this racket:text<%>)
          (send delegate set-tabs null (send this get-tab-size) #f))
        (send delegate hide-caret #t)
        (send delegate erase)
        (send delegate set-style-list (get-style-list))
        (let loop ([snip (find-first-snip)])
          (when snip
            (let ([copy-of-snip (copy snip)])
              (send delegate insert
                    copy-of-snip
                    (send delegate last-position)
                    (send delegate last-position))
              (loop (send snip next)))))
        (for-each
         (λ (range)
           (send delegate unhighlight-range 
                 (range-start range)
                 (range-end range)
                 (range-color range)
                 (range-caret-space? range)
                 (range-style range)))
         (send delegate get-highlighted-ranges))
        (for-each
         (λ (range)
           (send delegate highlight-range 
                 (range-start range)
                 (range-end range)
                 (range-color range)
                 (range-caret-space? range)
                 'high
                 (range-style range)))
         (reverse (get-highlighted-ranges)))
        (send delegate lock #t)
        (send delegate end-edit-sequence))
    
      (define/override (highlight-range start end color 
                                        [caret-space? #f] 
                                        [priority 'low]
                                        [style 'rectangle] 
                                        #:adjust-on-insert/delete? [adjust-on-insert/delete? #f]
                                        #:key [key #f])
        (when delegate 
          (send delegate highlight-range start end color caret-space? priority style
                #:adjust-on-insert/delete? adjust-on-insert/delete?
                #:key key))
        (super highlight-range start end color caret-space? priority style 
               #:adjust-on-insert/delete? adjust-on-insert/delete?
               #:key key))
    
      ;; only need to override this unhighlight-ranges, since 
      ;; all the other unhighlighting variants call this one
      (define/override (unhighlight-ranges pred [just-one? #f])
        (when delegate 
          (send delegate unhighlight-ranges pred just-one?))
        (super unhighlight-ranges pred just-one?))
    
      (inherit get-canvases get-active-canvas has-focus?)
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret?)
        (super on-paint before? dc left top right bottom dx dy draw-caret?)
        (when delegate 
          (unless before?
            (let ([active-canvas (get-active-canvas)])
              (when active-canvas
                (send (send active-canvas get-top-level-window) delegate-moved))))))
    
      (define no-delegate-edit-sequence-depth 0)
     
      (define/augment (on-edit-sequence)
        (cond
          [delegate 
           (send delegate begin-edit-sequence)]
          [else
           (set! no-delegate-edit-sequence-depth
                 (+ no-delegate-edit-sequence-depth 1))])
        (inner (void) on-edit-sequence))

      (define/augment (after-edit-sequence)
        (cond
          [(and delegate 
                (= 0 no-delegate-edit-sequence-depth))
           (send delegate end-edit-sequence)]
          [else
           (set! no-delegate-edit-sequence-depth
                 (- no-delegate-edit-sequence-depth 1))])
        (inner (void) after-edit-sequence))
    
      (define/override (resized snip redraw-now?)
        (super resized snip redraw-now?)
        (when (and delegate
                   (not (is-a? snip string-snip%)))
          (when linked-snips
            (let ([delegate-copy (hash-ref linked-snips snip (λ () #f))])
              (when delegate-copy
                (send delegate resized delegate-copy redraw-now?))))))
    
      (define/augment (after-insert start len)
        (when delegate 
          (send delegate begin-edit-sequence)
          (send delegate lock #f)
          (split-snip start)
          (split-snip (+ start len))
          (let loop ([snip (find-snip (+ start len) 'before-or-none)])
            (when snip
              (unless ((get-snip-position snip) . < . start)
                (send delegate insert (copy snip) start start)
                (loop (send snip previous)))))
          (send delegate lock #t)
          (send delegate end-edit-sequence))
        (inner (void) after-insert start len))
    
      (define/augment (after-delete start len)
        (when delegate 
          (send delegate lock #f)
          (send delegate begin-edit-sequence)
          (send delegate delete start (+ start len))
          (send delegate end-edit-sequence)
          (send delegate lock #t))
        (inner (void) after-delete start len))
    
      (define/augment (after-change-style start len)
        (when delegate 
          (send delegate begin-edit-sequence)
          (send delegate lock #f)
          (split-snip start)
          (let* ([snip (find-snip start 'after)]
                 [style (send snip get-style)])
            (send delegate change-style style start (+ start len)))
          (send delegate lock #f)
          (send delegate end-edit-sequence))
        (inner (void) after-change-style start len))
    
      (define/augment (after-load-file success?)
        (when success?
          (refresh-delegate))
        (inner (void) after-load-file success?))
      (super-new))))
