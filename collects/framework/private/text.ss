#|

WARNING: printf is rebound in the body of the unit to always
         print to the original output port.

|#

(module text mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   "../macro.ss"
	   "../gui-utils.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "list.ss")
	   (lib "etc.ss"))
  (provide text@)

  (define text@
    (unit/sig framework:text^
      (import mred^
              [icon : framework:icon^]
              [editor : framework:editor^]
              [preferences : framework:preferences^]
              [keymap : framework:keymap^]
              [color-model : framework:color-model^]
              [frame : framework:frame^]
              [scheme : framework:scheme^]
              [number-snip : framework:number-snip^])
      
      (rename [-keymap% keymap%])
      
      (define original-output-port (current-output-port))
      (define (printf . args) (apply fprintf original-output-port args))
      
      (define-struct range (start end b/w-bitmap color caret-space?))
      (define-struct rectangle (left top right bottom b/w-bitmap color))
      
      ;; wx: `default-wrapping?', add as the initial value for auto-wrap bitmap,
      ;; unless matthew makes it primitive
      
      (define basic<%>
        (interface (editor:basic<%> (class->interface text%))
          highlight-range
          get-highlighted-ranges
          get-styles-fixed
          set-styles-fixed
          move/copy-to-edit
          initial-autowrap-bitmap))
      
      (define basic-mixin
        (mixin (editor:basic<%> (class->interface text%)) (basic<%>)
          (inherit get-canvases get-admin split-snip get-snip-position
                   begin-edit-sequence end-edit-sequence
                   set-autowrap-bitmap
                   delete find-snip invalidate-bitmap-cache
                   set-file-format get-file-format
                   get-style-list is-modified? change-style set-modified
                   position-location get-extent)
          
          (define highlight-pen #f)
          (define highlight-brush #f)

          (define range-rectangles null)
          (define ranges null)
          
          (define/public (get-highlighted-ranges) ranges)
          
          (define (invalidate-rectangles rectangles)
            (let ([b1 (box 0)]
                  [b2 (box 0)]
                  [b3 (box 0)]
                  [b4 (box 0)]
                  [canvases (get-canvases)])
              (let-values ([(min-left max-right)
                            (cond
                              [(null? canvases)
                               (let ([admin (get-admin)])
                                 (if admin
                                     (begin
                                       (send admin get-view b1 b2 b3 b4)
                                       (let* ([this-left (unbox b1)]
                                              [this-width (unbox b3)]
                                              [this-right (+ this-left this-width)])
                                         (values this-left
                                                 this-right)))
                                     (values #f #f)))]
                              [else 
                               (let loop ([left #f]
                                          [right #f]
                                          [canvases canvases])
                                 (cond
                                   [(null? canvases)
                                    (values left right)]
                                   [else
                                    (let-values ([(this-left this-right)
                                                  (send (car canvases)
                                                        call-as-primary-owner
                                                        (lambda ()
                                                          (send (get-admin) get-view b1 b2 b3 b4)
                                                          (let* ([this-left (unbox b1)]
                                                                 [this-width (unbox b3)]
                                                                 [this-right (+ this-left this-width)])
                                                            (values this-left
                                                                    this-right))))])
                                      (if (and left right)
                                          (loop (min this-left left)
                                                (max this-right right)
                                                (cdr canvases))
                                          (loop this-left
                                                this-right
                                                (cdr canvases))))]))])])
                (when (and min-left max-right)
                  (let loop ([left #f]
                             [top #f]
                             [right #f]
                             [bottom #f]
                             [rectangles rectangles])
                    (cond
                      [(null? rectangles)
                       (when left
                         (let ([width (- right left)]
                               [height (- bottom top)])
                           (when (and (> width 0)
                                      (> height 0))
                             (invalidate-bitmap-cache left top width height))))]
                      [else (let* ([r (car rectangles)]
                                   
                                   [rleft (rectangle-left r)]
                                   [rright (rectangle-right r)]
                                   [rtop (rectangle-top r)]
                                   [rbottom (rectangle-bottom r)]
                                   
                                   [this-left (if (number? rleft)
                                                  rleft
                                                  min-left)]
                                   [this-right (if (number? rright)
                                                   rright
                                                   max-right)]
                                   [this-bottom rbottom]
                                   [this-top rtop])
                              (if (and left top right bottom)
                                  (loop (min this-left left)
                                        (min this-top top)
                                        (max this-right right)
                                        (max this-bottom bottom)
                                        (cdr rectangles))
                                  (loop this-left 
                                        this-top
                                        this-right
                                        this-bottom
                                        (cdr rectangles))))]))))))
          
          (define (recompute-range-rectangles)
            (let* ([b1 (box 0)]
                   [b2 (box 0)]
                   [new-rectangles
                    (lambda (range)
                      (let* ([start (range-start range)]
                             [end (range-end range)]
                             [b/w-bitmap (range-b/w-bitmap range)]
                             [color (range-color range)]
                             [caret-space? (range-caret-space? range)]
                             [start-eol? #f]
                             [end-eol? (if (= start end)
                                           start-eol?
                                           #t)])
                        (let-values ([(start-x top-start-y)
                                      (begin 
                                        (position-location start b1 b2 #t start-eol? #t)
                                        (values (if caret-space?
                                                    (+ 1 (unbox b1))
                                                    (unbox b1))
                                                (unbox b2)))]
                                     [(end-x top-end-y)
                                      (begin (position-location end b1 b2 #t end-eol? #t)
                                             (values (unbox b1) (unbox b2)))]
                                     [(bottom-start-y)
                                      (begin (position-location start b1 b2 #f start-eol? #t)
                                             (unbox b2))]
                                     [(bottom-end-y)
                                      (begin (position-location end b1 b2 #f end-eol? #t)
                                             (unbox b2))])
                          (cond
                            [(= top-start-y top-end-y)
                             (list 
                              (make-rectangle start-x
                                              top-start-y
                                              (if (= end-x start-x)
                                                  (+ end-x 1)
                                                  end-x)
                                              bottom-start-y
                                              b/w-bitmap 
                                              color))]
                            [else
                             (list
                              (make-rectangle start-x
                                              top-start-y
                                              'right-edge
                                              bottom-start-y
                                              b/w-bitmap
                                              color)
                              (make-rectangle 'left-edge
                                              bottom-start-y
                                              'max-width
                                              top-end-y
                                              b/w-bitmap
                                              color)
                              (make-rectangle 'left-edge
                                              top-end-y
                                              end-x
                                              bottom-end-y
                                              b/w-bitmap
                                              color))]))))]
                   [old-rectangles range-rectangles])
              
              (set! range-rectangles 
                    (foldl (lambda (x l) (append (new-rectangles x) l))
                           null ranges))))
          
          (define/public highlight-range
            (opt-lambda (start end color [bitmap #f] [caret-space? #f] [priority 'low])
              (unless (let ([exact-pos-int?
                             (lambda (x) (and (integer? x) (exact? x) (x . >= . 0)))])
                        (and (exact-pos-int? start)
                             (exact-pos-int? end)))
                (error 'highlight-range "expected first two arguments to be non-negative exact integers, got: ~e ~e"
                       start end))
              (unless (or (eq? priority 'high) (eq? priority 'low))
                (error 'highlight-range "expected last argument to be either 'high or 'low, got: ~e"
                       priority))
              (let ([l (make-range start end bitmap color caret-space?)])
                (invalidate-rectangles range-rectangles)
                (set! ranges (if (eq? priority 'high) (cons l ranges) (append ranges (list l))))
                (recompute-range-rectangles)
                (invalidate-rectangles range-rectangles)
                (lambda ()
                  (let ([old-rectangles range-rectangles])
                    (set! ranges
                          (let loop ([r ranges])
                            (cond
                              [(null? r) r]
                              [else (if (eq? (car r) l)
                                        (cdr r)
                                        (cons (car r) (loop (cdr r))))])))
                    (recompute-range-rectangles)                    
                    (invalidate-rectangles old-rectangles))))))
          (rename [super-on-paint on-paint])
          (override on-paint)
          (define (on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
            (super-on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
            (recompute-range-rectangles)
            (let ([b1 (box 0)]
                  [b2 (box 0)]
                  [b3 (box 0)]
                  [b4 (box 0)])
              (for-each
               (lambda (rectangle)
                 (let-values ([(view-x view-y view-width view-height)
                               (begin 
                                 (send (get-admin) get-view b1 b2 b3 b4)
                                 (values (unbox b1)
                                         (unbox b2)
                                         (unbox b3)
                                         (unbox b4)))])
                   (let* ([old-pen (send dc get-pen)]
                          [old-brush (send dc get-brush)]
                          [b/w-bitmap (rectangle-b/w-bitmap rectangle)]
                          [color (let* ([rc (rectangle-color rectangle)]
                                        [tmpc (make-object color% 0 0 0)])
                                   (if rc
                                       (begin (send dc try-color rc tmpc)
                                              (if (<= (color-model:rgb-color-distance
                                                       (send rc red)
                                                       (send rc green)
                                                       (send rc blue)
                                                       (send tmpc red)
                                                       (send tmpc green)
                                                       (send tmpc blue))
                                                      18)
                                                  rc
                                                  #f))
                                       rc))]
                          [first-number (lambda (x y) (if (number? x) x y))]
                          [left (max left-margin (first-number (rectangle-left rectangle) view-x))]
                          [top (max top-margin (rectangle-top rectangle))]
                          [right (min right-margin
                                      (first-number 
                                       (rectangle-right rectangle)
                                       (+ view-x view-width)))]
                          [bottom (min bottom-margin (rectangle-bottom rectangle))]
                          [width (max 0 (- right left))]
                          [height (max 0 (- bottom top))])
                     (let/ec k
                       (cond
                         [(and before color)
                          (send dc set-pen (send the-pen-list find-or-create-pen color 0 'solid))
                          (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))]
                         [(and (not before) (not color) b/w-bitmap)
                          (unless highlight-pen
                            (set! highlight-pen (make-object pen% "BLACK" 0 'solid)))
                          (unless highlight-brush
                            (set! highlight-brush (make-object brush% "black" 'solid)))
                          (send highlight-pen set-stipple b/w-bitmap)
                          (send highlight-brush set-stipple b/w-bitmap)
                          (send dc set-pen highlight-pen)
                          (send dc set-brush highlight-brush)]
                         [else (k (void))])
                       (send dc draw-rectangle (+ left dx) (+ top dy) width height)
                       (send dc set-pen old-pen)
                       (send dc set-brush old-brush)))))
               range-rectangles)))
          
          (define styles-fixed? #f)
          (public get-styles-fixed set-styles-fixed)
          (define (get-styles-fixed) styles-fixed?)
          (define (set-styles-fixed b) (set! styles-fixed? b))
          
          (rename [super-on-insert on-insert]
                  [super-after-insert after-insert])
          (define/override (on-insert start len)
            (begin-edit-sequence)
            (super-on-insert start len))
          (define/override (after-insert start len)
            (when styles-fixed?
              (change-style (send (get-style-list) find-named-style "Standard")
                            start
                            (+ start len)
                            #f))
            (super-after-insert start len)
            (end-edit-sequence))
          
          (public move/copy-to-edit)
          (define (move/copy-to-edit dest-edit start end dest-position)
            (split-snip start)
            (split-snip end)
            (let loop ([snip (find-snip end 'before)])
              (cond
                [(or (not snip) (< (get-snip-position snip) start))
                 (void)]
                [else
                 (let ([prev (send snip previous)]
                       [released/copied (if (send snip release-from-owner)
                                            snip
                                            (let* ([copy (send snip copy)]
                                                   [snip-start (get-snip-position snip)]
                                                   [snip-end (+ snip-start (send snip get-count))])
                                              (delete snip-start snip-end)
                                              snip))])
                   (send dest-edit insert released/copied dest-position dest-position)
                   (loop prev))])))
          
          (public initial-autowrap-bitmap)
          (define (initial-autowrap-bitmap) (icon:get-autowrap-bitmap))
          
          (super-instantiate ())
          (set-autowrap-bitmap (initial-autowrap-bitmap))))
      
      (define hide-caret/selection<%> (interface (basic<%>)))
      (define hide-caret/selection-mixin
        (mixin (basic<%>) (hide-caret/selection<%>)
          (override after-set-position)
          (inherit get-start-position get-end-position hide-caret)
          (define (after-set-position)
            (hide-caret (= (get-start-position) (get-end-position))))
          (super-instantiate ())))

      (define nbsp->space<%> (interface ((class->interface text%))))
      (define nbsp->space-mixin
        (mixin ((class->interface text%)) (nbsp->space<%>)
          (field [rewriting #f])
          (inherit begin-edit-sequence end-edit-sequence delete insert get-character)
          (rename [super-on-insert on-insert]
                  [super-after-insert after-insert])
          (define/override (on-insert start len)
            (begin-edit-sequence)
            (super-on-insert start len))
	  (inherit find-string)
          (define/override (after-insert start len)
            (unless rewriting
              (set! rewriting #t)
	      (let ([str (string (integer->char 160))]
		    [last-pos (+ start len)])
		(let loop ([pos start])
		  (when (< pos last-pos)
		    (let ([next-pos (find-string str 'forward pos last-pos)])
		      (when next-pos
			(delete next-pos (+ next-pos 1) #f)
			(insert " " next-pos next-pos #f)
			(loop (+ next-pos 1)))))))
	      (set! rewriting #f))
            (super-after-insert start len)
            (end-edit-sequence))
          (super-instantiate ())))
      
      (define searching<%> (interface (editor:keymap<%> basic<%>)))
      (define searching-mixin
        (mixin (editor:keymap<%> basic<%>) (searching<%>)
          (rename [super-get-keymaps get-keymaps])
          (override get-keymaps)
          (define (get-keymaps)
            (cons (keymap:get-search) (super-get-keymaps)))
          (super-instantiate ())))
      
      (define return<%> (interface ((class->interface text%))))
      (define return-mixin
        (mixin ((class->interface text%)) (return<%>) 
          (init-field return)
          (rename [super-on-local-char on-local-char])
          (override on-local-char)
          (define (on-local-char key)
            (let ([cr-code #\return]
                  [lf-code #\newline]
                  [code (send key get-key-code)])
              (or (and (char? code)
                       (or (char=? lf-code code)
                           (char=? cr-code code))
                       (return))
                  (super-on-local-char key))))
          (super-instantiate ())))
      
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
          
	  (define cache-function #f)

          (rename [super-insert insert])
          (define/override (insert s len pos)
            (set! cache-function #f)
            (super-insert s len pos))

	  ;; for-each/sections : string -> dc number number -> void
          (define (for-each/sections str)
            (let loop ([n (string-length str)]
                       [len 0]
                       [blank? #t])
              (cond
                [(zero? n)
                 (if blank?
		     (lambda (dc x y) (void))
		     (lambda (dc x y)
                       (send dc draw-line (+ x n) y (+ x n (- len 1)) y)))]
                [else
                 (let ([white? (char-whitespace? (string-ref str (- n 1)))])
                   (cond
                     [(eq? white? blank?)
                      (loop (- n 1) (+ len 1) blank?)]
                     [else
		      (let ([res (loop (- n 1) 1 (not blank?))])
			(if blank?
			    res
			    (lambda (dc x y)
			      (send dc draw-line (+ x n) y (+ x n (- len 1)) y)
			      (res dc x y))))]))])))
          
          (define/override (draw dc x y left top right bottom dx dy draw-caret)
            (let ([str (get-text 0 (get-count))])
	      (unless cache-function
		(set! cache-function (for-each/sections str)))
              (when (<= top y bottom)
		(cache-function dc x y))))
          (apply super-make-object args)))
      
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
          (inherit split-snip find-snip get-snip-position
                   find-first-snip get-style-list set-tabs)
          
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
                        (hash-table-put! linked-snips snip new-snip)
                        new-snip)])])
              (send new-snip set-flags (send snip get-flags))
              new-snip))

          (define delegate #f)
          (inherit get-highlighted-ranges)
          (define/public (get-delegate) delegate)
          (define/public (set-delegate _d)
            (set! delegate _d)
            (set! linked-snips (if _d
                                   (make-hash-table)
                                   #f))
            (refresh-delegate))
          
          (define/private (refresh-delegate)
            (when delegate
              (send delegate begin-edit-sequence)
              (send delegate lock #f)
              (when (is-a? this scheme:text<%>)
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
               (lambda (range)
                 (send delegate highlight-range 
                       (range-start range)
                       (range-end range)
                       (range-color range)
                       (range-b/w-bitmap range)
                       (range-caret-space? range)
                       'high))
               (reverse (get-highlighted-ranges)))
              (send delegate lock #t)
              (send delegate end-edit-sequence)))
          
          (rename [super-highlight-range highlight-range])
          (define/override highlight-range
            (opt-lambda (start end color [bitmap #f] [caret-space? #f] [priority 'low])
              (let ([res (super-highlight-range start end color bitmap caret-space? priority)])
                (if delegate
                    (let ([delegate-res (send delegate highlight-range 
                                              start end color bitmap caret-space? priority)]) 
                      (lambda ()
                        (res)
                        (delegate-res)))
                    res))))
          
          (rename [super-on-paint on-paint])
          (inherit get-canvases get-active-canvas has-focus?)
          (define/override (on-paint before? dc left top right bottom dx dy draw-caret?)
            (super-on-paint before? dc left top right bottom dx dy draw-caret?)
            (unless before?
              (let ([active-canvas (get-active-canvas)])
                (when active-canvas
                  (send (send active-canvas get-top-level-window) delegate-moved)))))

          (rename [super-on-edit-sequence on-edit-sequence])
          (define/override (on-edit-sequence)
            (super-on-edit-sequence)
            (when delegate
              (send delegate begin-edit-sequence)))
          
          (rename [super-after-edit-sequence after-edit-sequence])
          (define/override (after-edit-sequence)
            (super-after-edit-sequence)
            (when delegate
              (send delegate end-edit-sequence)))

          (rename [super-resized resized])
          (define/override (resized snip redraw-now?)
            (super-resized snip redraw-now?)
            (when (and delegate
                       linked-snips
                       (not (is-a? snip string-snip%)))
              (let ([delegate-copy (hash-table-get linked-snips snip (lambda () #f))])
                (when delegate-copy
                  (send delegate resized delegate-copy redraw-now?)))))
          
          (rename [super-after-insert after-insert])
          (define/override (after-insert start len)
            (super-after-insert start len)
            (when delegate
              (send delegate begin-edit-sequence)
              (send delegate lock #f)
              (split-snip start)
              (split-snip (+ start len))
              (let loop ([snip (find-snip (+ start len) 'before)])
                (when snip
                  (unless ((get-snip-position snip) . < . start)
                    (send delegate insert (copy snip) start start)
                    (loop (send snip previous)))))
              (send delegate lock #t)
              (send delegate end-edit-sequence)))
          
          (rename [super-after-delete after-delete])
          (define/override (after-delete start len)
            (super-after-delete start len)
            (when delegate
              (send delegate lock #f)
              (send delegate begin-edit-sequence)
              (send delegate delete start (+ start len))
              (send delegate end-edit-sequence)
              (send delegate lock #t)))
          
          (rename [super-after-change-style after-change-style])
          (define/override (after-change-style start len)
            (super-after-change-style start len)
            (when delegate
              (send delegate begin-edit-sequence)
              (send delegate lock #f)
              (split-snip start)
              (let* ([snip (find-snip start 'after)]
                     [style (send snip get-style)]
                     [other-style 
                      '(send (send delegate get-style-list) find-or-create-style
                             style delegate-style-delta)])
                (send delegate change-style style start (+ start len)))
              (send delegate lock #f)
              (send delegate end-edit-sequence)))
          
          (define filename #f)
          (define format #f)
          (rename [super-on-load-file on-load-file]
                  [super-after-load-file after-load-file])
          (define/override (on-load-file _filename _format)
            (super-on-load-file _filename _format)
            (set! filename _filename)
            (set! format _format))
          (define/override (after-load-file success?)
            (super-after-load-file success?)
            (when success?
              (refresh-delegate)))
          (super-instantiate ())))
    
      (define info<%> (interface (basic<%>)))
      
      (define info-mixin
        (mixin (editor:keymap<%> basic<%>) (info<%>)
          (inherit get-start-position get-end-position get-canvas
                   run-after-edit-sequence)
          (rename [super-after-set-position after-set-position]
                  [super-after-edit-sequence after-edit-sequence]
                  [super-on-edit-sequence on-edit-sequence]
                  [super-after-insert after-insert]
                  [super-after-delete after-delete]
                  [super-set-overwrite-mode set-overwrite-mode]
                  [super-set-anchor set-anchor])
          (define (enqueue-for-frame call-method tag)
            (run-after-edit-sequence
             (rec from-enqueue-for-frame
               (lambda ()
                 (call-with-frame call-method)))
             tag))
          
          ;; call-with-frame : ((is-a?/c frame:text-info<%>) -> void) -> void
          ;; calls the argument thunk with the frame showing this editor.
          (define (call-with-frame call-method)
            (let ([canvas (get-canvas)])
              (when canvas
                (let ([frame (send canvas get-top-level-window)])
                  (when (is-a? frame frame:text-info<%>)
                    (call-method frame))))))
          
          (override set-anchor set-overwrite-mode after-set-position after-insert after-delete)
          (define (set-anchor x)
            (super-set-anchor x)
            (enqueue-for-frame 
             (lambda (x) (send x anchor-status-changed))
             'framework:anchor-status-changed))
          (define (set-overwrite-mode x)
            (super-set-overwrite-mode x)
            (enqueue-for-frame
             (lambda (x) (send x overwrite-status-changed))
             'framework:overwrite-status-changed))
          (define (after-set-position)
            (super-after-set-position)
            (maybe-queue-editor-position-update))
          
          ;; maybe-queue-editor-position-update : -> void
          ;; updates the editor-position in the frame,
          ;; but delays it until the next low-priority event occurs.
          (define callback-running? #f)
          (define/private (maybe-queue-editor-position-update)
            (enqueue-for-frame 
             (lambda (frame) 
               (unless callback-running?
                 (set! callback-running? #t)
                 (queue-callback
                  (lambda ()
                    (send frame editor-position-changed)
                    (set! callback-running? #f))
                  #f)))
             'framework:info-frame:update-editor-position))
          
          (define (after-insert start len)
            (super-after-insert start len)
            (maybe-queue-editor-position-update))
          (define (after-delete start len)
            (super-after-delete start len)
            (maybe-queue-editor-position-update))
          (super-instantiate ())))
      
      (define clever-file-format<%> (interface ((class->interface text%))))
      
      (define clever-file-format-mixin
        (mixin ((class->interface text%)) (clever-file-format<%>)
          (inherit get-file-format set-file-format find-first-snip)
          (rename [super-on-save-file on-save-file])
          (define (all-string-snips)
            (let loop ([s (find-first-snip)])
              (cond
                [(not s) #t]
                [(is-a? s string-snip%)
                 (loop (send s next))]
                [else #f])))
          (define/override (on-save-file name format)
            (let ([all-strings? (all-string-snips)])
              (cond
                [(and all-strings?
                      (eq? format 'same)
                      (eq? 'standard (get-file-format))
                      (or (not (preferences:get 'framework:verify-change-format))
                          (gui-utils:get-choice
                           (string-constant save-as-plain-text) 
                           (string-constant yes)
                           (string-constant no))))
                 (set-file-format 'text)]
                [(and (not all-strings?)
                      (eq? format 'same)
                      (eq? 'text (get-file-format))
                      (or (not (preferences:get 'framework:verify-change-format))
                          (gui-utils:get-choice
                           (string-constant save-in-drs-format)
                           (string-constant yes)
                           (string-constant no))))
                 (set-file-format 'standard)]
                [else (void)]))
            (super-on-save-file name format))
          (super-instantiate ())))
      
      (define ports<%>
        (interface ()
          get-insertion-point 
          set-insertion-point
          get-unread-start-point
          set-unread-start-point
          set-allow-edits
          get-allow-edits
          has-between?
          submit-to-port?
          on-submit
          send-eof-to-in-port
          clear-output-ports
          clear-input-port
          get-in-port
          get-out-port
          get-err-port
          get-value-port))
      
      (define ports-mixin
        (mixin ((class->interface text%) #;scheme:text<%>) (ports<%>)
          (inherit begin-edit-sequence
                   end-edit-sequence
                   insert
                   change-style
                   split-snip
                   find-snip
                   get-start-position
                   get-end-position
                   get-snip-position
                   last-position
                   lock
                   is-locked?)
          
          ;; private field
          (define eventspace (current-eventspace))
          
          ;; insertion-point : number
          ;; only updated in `eventspace' (above)'s main thread
          (define insertion-point 0)
          
          ;; unread-start-points : number
          ;; from this position to the end of the buffer is the
          ;;      users editing that has not been committed to the
          ;;      port.
          ;; only updated in `eventspace' (above)'s main thread
          (define unread-start-point 0)
          
          ;; allow-edits? : boolean
          ;; when this flag is set, only insert/delete after the
          ;; insertion-point are allowed.
          (define allow-edits? #f)
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  public interface
          ;;
          
          ;; insert-between : string -> void
          ;; inserts something between the insertion point and the unread region
          (define/public (insert-between str)
            (insert str unread-start-point unread-start-point)
            (set! unread-start-point (+ insertion-point 
                                        ;; string-length is bad here
                                        (string-length str))))
          
          ;; has-between? : -> boolean
          ;; indicates if there is currently some text after the insertion
          ;; point, but before the unread region
          (define/public (has-between?)
            (not (= insertion-point unread-start-point)))
          
          (define/public (get-insertion-point) insertion-point)
          (define/public (set-insertion-point ip) (set! insertion-point ip))
          (define/public (get-unread-start-point) unread-start-point)
          (define/public (set-unread-start-point u) (set! unread-start-point u))
          
          (define/public (set-allow-edits allow?) (set! allow-edits? allow?))
          (define/public (get-allow-edits) allow-edits?)
          
          (define/public (send-eof-to-in-port) (channel-put read-chan eof))
          
          (define/public (clear-input-port)
            (channel-put clear-input-chan (void)))
          
          (define/public (clear-output-ports) 
            (channel-put clear-output-chan (void)))
          
          (define/public (get-in-port)
            (unless in-port (error 'get-in-port "not ready"))
            in-port)
          (define/public (get-out-port)
            (unless out-port (error 'get-out-port "not ready"))
            out-port)
          (define/public (get-err-port)
            (unless err-port (error 'get-err-port "not ready"))
            err-port)
          (define/public (get-value-port)
            (unless err-port (error 'get-value-port "not ready"))
            value-port)
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  specialization interface
          ;;
          
          (define/public (submit-to-port? key) #t)
          (define/public (on-submit) (void))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  editor integration
          ;;
          
          (rename [super-can-insert? can-insert?])
          (define/override (can-insert? start len)
            (and (super-can-insert? start len)
                 (or allow-edits?
                     (start . >= . insertion-point))))
          
          (rename [super-can-delete? can-delete?])
          (define/override (can-delete? start len)
            (and (super-can-delete? start len)
                 (or allow-edits?
                     (start . >= . insertion-point))))
          
          (rename [super-on-local-char on-local-char])
          (define/override (on-local-char key)
            (let ([start (get-start-position)]
                  [end (get-end-position)]
                  [code (send key get-key-code)])
              (cond
                [(not (or (eq? code 'numpad-enter)
                          (equal? code #\return)
                          (equal? code #\newline)))
                 (super-on-local-char key)]
                [(and (insertion-point . <= . start)
                      (= start end)
                      (submit-to-port? key))
                 (let ([snips/chars (extract-snips/chars unread-start-point (last-position))])
                   (for-each (lambda (s/c) 
                               (cond
                                 [(is-a? s/c snip%)
                                  (channel-put read-chan s/c)]
                                 [(char? s/c)
                                  (for-each (lambda (b) (channel-put read-chan b))
                                            (bytes->list (string->bytes/utf-8 (string s/c))))]))
                             snips/chars)
                   (set! allow-tabify? #f)
                   (super-on-local-char key)
                   (set! allow-tabify? #t)
                   (set! unread-start-point (last-position))
                   (set! insertion-point (last-position))
                   (on-submit))]
                [else
                 (super-on-local-char key)])))
          
          (define allow-tabify? #t)
;          (rename [super-tabify-on-return? tabify-on-return?])
;          (define/override (tabify-on-return?)
;            (and (super-tabify-on-return?)
;                 allow-tabify?))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  syncronization code
          ;;
          
          
          ;; flush-chan : (channel (waitable void))
          ;; signals that the buffer-thread should flush pending output
          ;; the waitable inside is waited on to indicate the flush has occurred
          (define flush-chan (make-channel))
          
          ;; clear-output-chan, clear-input-chan : (channel void)
          ;; dumps all data and readers making the ports empty again
          (define clear-output-chan (make-channel))
          (define clear-input-chan (make-channel))
          
          ;; write-chan : (channel (cons bytes style))
          ;; send output to the editor
          (define write-chan (make-channel))
          
          ;; read-chan : (channel (union byte snip eof))
          ;; send input from the editor
          (define read-chan (make-channel))
          
          ;; readers-chan : (channel (list (channel (union byte snip))
          ;;                               (channel ...)))
          (define readers-chan (make-channel))
          
          ;; readers-waiting-chan : (channel (channel boolean))
          (define readers-waiting-chan (make-channel))
          
          ;; queue-insertion : (listof (cons (union string snip) style-delta)) waitable -> void
          ;; txt is in the reverse order of the things to be inserted.
          ;; the waitable is waited on when the text has actually been inserted
          ;; thread: any thread, except the eventspace main thread
          (define/private (queue-insertion txts signal) 
            (parameterize ([current-eventspace eventspace])
              (queue-callback
               (lambda ()
                 (do-insertion txts)
                 (object-wait-multiple #f signal)))))
          
          ;; do-insertion : (listof (cons (union string snip) style-delta)) -> void
          ;; thread: eventspace main thread
          (define/private (do-insertion txts)
            (let ([locked? (is-locked?)])
              (begin-edit-sequence)
              (lock #f)
              (let loop ([txts txts])
                (cond
                  [(null? txts) (void)]
                  [else 
                   (let* ([fst (car txts)]
                          [str/snp (car fst)]
                          [sd (cdr fst)])
                     (insert (if (is-a? str/snp snip%)
                                 (send str/snp copy)
                                 str/snp)
                             insertion-point
                             insertion-point
                             #f)
                     (let ([inserted-count
                            (if (is-a? str/snp snip%)
                                1
                                (string-length str/snp))])
                       (change-style sd insertion-point (+ insertion-point inserted-count))
                       (set! insertion-point (+ insertion-point inserted-count))
                       (set! unread-start-point (+ unread-start-point inserted-count))))
                   (loop (cdr txts))]))
              (lock locked?)
              (end-edit-sequence)))
          
          (define input-buffer-thread
            (thread
             (lambda ()
               (define (data-waiting data)
                 (printf "data-waiting ~s\n" (queue->list data))
                 (object-wait-multiple
                  #f
                  (make-wrapped-waitable
                   readers-waiting-chan
                   (lambda (result)
                     (channel-put result #t)
                     (data-waiting data)))
                  (make-wrapped-waitable
                   clear-input-chan
                   (lambda (_)
                     (data-and-readers-waiting (empty-queue) (empty-queue))))
                  (make-wrapped-waitable
                   read-chan
                   (lambda (new-data)
                     (data-waiting (enqueue new-data data))))
                  (make-wrapped-waitable
                   readers-chan
                   (lambda (new-reader)
                     (data-and-readers-waiting data (enqueue new-reader (empty-queue)))))))
               
               (define (readers-waiting readers)
                 (printf "readers-waiting ~s\n" (queue->list readers))
                 (object-wait-multiple
                  #f
                  (make-wrapped-waitable
                   clear-input-chan
                   (lambda (_)
                     (data-and-readers-waiting (empty-queue) (empty-queue))))
                  (make-wrapped-waitable
                   readers-waiting-chan
                   (lambda (result)
                     (channel-put result #f)
                     (readers-waiting readers)))
                  (make-wrapped-waitable
                   read-chan
                   (lambda (new-data)
                     (data-and-readers-waiting (enqueue new-data (empty-queue)) readers)))
                  (make-wrapped-waitable
                   readers-chan
                   (lambda (new-reader)
                     (readers-waiting (enqueue new-reader readers))))))
               
               (define (data-and-readers-waiting data readers)
                 (cond
                   [(queue-empty? data) (readers-waiting readers)]
                   [(queue-empty? readers) (data-waiting data)]
                   [else 
                    (let* ([data-hd (queue-first data)]
                           [reader-hd (queue-first readers)]
                           [reader-succeed (car reader-hd)]
                           [reader-fail (cadr reader-hd)])
                      (object-wait-multiple
                       #f
                       (make-wrapped-waitable
                        clear-input-chan
                        (lambda (_)
                          (data-and-readers-waiting (empty-queue) (empty-queue))))
                       (make-wrapped-waitable
                        readers-waiting-chan
                        (lambda (result)
                          (channel-put result #t)
                          (data-and-readers-waiting data readers)))
                       (make-wrapped-waitable
                        (make-channel-put-waitable reader-succeed data-hd)
                        (lambda (v)
                          (data-and-readers-waiting (queue-rest data)
                                                    (queue-rest readers))))
                       (make-wrapped-waitable
                        reader-fail
                        (lambda (v)
                          (data-and-readers-waiting data
                                                    (queue-rest readers))))))]))
               
               (data-and-readers-waiting (empty-queue) (empty-queue)))))
          
          (define output-buffer-thread
            (let ([buffer-full 40]
                  [converter (bytes-open-converter "UTF-8-permissive" "UTF-8")])
              (thread
               (lambda ()
                 (let loop (;; text-to-insert : (queue (cons (union snip bytes) style))
                            [text-to-insert (empty-queue)])
                   (object-wait-multiple
                    #f
                    (make-wrapped-waitable
                     flush-chan
                     (lambda (return-waitable)
                       (let-values ([(viable-bytes remaining-queue) (split-queue converter text-to-insert)])
                         (queue-insertion viable-bytes return-waitable)
                         (loop remaining-queue))))
                    (make-wrapped-waitable
                     clear-output-chan
                     (lambda (_)
                       (loop (empty-queue))))
                    (make-wrapped-waitable
                     write-chan
                     (lambda (pr)
                       (cond
                         [((queue-size text-to-insert) . < . buffer-full)
                          (loop (enqueue pr text-to-insert))]
                         [else
                          (let ([chan (make-channel)])
                            (let-values ([(viable-bytes remaining-queue) (split-queue converter text-to-insert)])
                              (queue-insertion viable-bytes (make-channel-put-waitable chan (void)))
                              (channel-get chan)
                              (loop remaining-queue)))])))))))))
          
          (field [in-port #f]
                 [out-port #f]
                 [err-port #f]
                 [value-port #f])
          (let ()
            
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;
            ;;  the following must be able to run
            ;;  in any thread (even concurrently)
            ;;
            (define op (current-output-port))
            (define (read-bytes-proc bytes)
              ;; this shouldn't return 0. it should return a waitable and 
              ;; let the system block and then re-call into this thing. 
              ;; yuck.
              (let ([readers-waiting-answer-chan (make-channel)])
                (channel-put readers-waiting-chan readers-waiting-answer-chan)
                (if (channel-get readers-waiting-answer-chan)
                    (let ([s/c
                           (object-wait-multiple
                            #f
                            (make-nack-guard-waitable
                             (lambda (fail-channel)
                               (let ([return-channel (make-channel)])
                                 (channel-put readers-chan (list return-channel fail-channel))
                                 return-channel))))])
                      (cond
                        [(byte? s/c)
                         (bytes-set! bytes 0 s/c)
                         1]
                        [(eof-object? s/c) s/c]
                        [else
                         (lambda (src line column position)
                           (values s/c 1))]))
                    0)))
            
            (define (in-close-proc) (void))
            
            (define (make-write-bytes-proc style)
              (lambda (to-write start end block/buffer?)
                (cond
                  [(eq? (current-thread) (eventspace-handler-thread eventspace))
                   (error 'write-bytes-proc "cannot write to port on eventspace main thread")]
                  [else
                   (channel-put write-chan (cons (subbytes to-write start end) style))])
                (- end start)))
            
            (define (flush-proc)
              (cond
                [(eq? (current-thread) (eventspace-handler-thread eventspace))
                 (error 'flush-proc "cannot flush port on eventspace main thread")]
                [else
                 (object-wait-multiple
                  #f
                  (make-nack-guard-waitable
                   (lambda (fail-channel)
                     (let* ([return-channel (make-channel)]
                            [return-waitable
                             (waitables->waitable-set
                              fail-channel
                              (make-channel-put-waitable return-channel (void)))])
                       (channel-put flush-chan return-waitable)
                       return-channel))))]))
            
            (define (out-close-proc)
              (void))
            
            (define out-sd (make-object style-delta% 'change-normal))
            (define err-sd (make-object style-delta% 'change-italic))
            (define value-sd (make-object style-delta% 'change-normal))
            (send out-sd set-delta-foreground (make-object color% 150 0 150))
            (send err-sd set-delta-foreground (make-object color% 255 0 0))
            (send value-sd set-delta-foreground (make-object color% 0 0 175))
            
            (set! in-port (make-custom-input-port read-bytes-proc
                                                  #f
                                                  in-close-proc))
            (set! out-port (make-custom-output-port #f
                                                    (make-write-bytes-proc out-sd)
                                                    flush-proc
                                                    out-close-proc))
            (set! err-port (make-custom-output-port #f 
                                                    (make-write-bytes-proc err-sd)
                                                    flush-proc
                                                    out-close-proc))
            (set! value-port (make-custom-output-port #f
                                                      (make-write-bytes-proc value-sd)
                                                      flush-proc
                                                      out-close-proc)))
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;;  helpers
          ;;
          
          ;; extract-snips/chars : number number -> (listof (union char snip))
          (define/private (extract-snips/chars start end)
            (split-snip start)
            (split-snip end)
            (let loop ([snip (find-snip start 'after-or-none)])
              (cond
                [(not snip) null]
                [(< (get-snip-position snip) end)
                 (if (is-a? snip string-snip%)
                     (append (string->list (send snip get-text 0 (send snip get-count)))
                             (loop (send snip next)))
                     (cons (send snip copy)
                           (loop (send snip next))))]
                [else null])))
          
          ;; split-queue : converter (queue (cons (union snip bytes) style) 
          ;;            -> (values (listof (queue (cons (union snip bytes) style)) queue)
          ;; this function must only be called on the output-buffer-thread
          ;; extracts the viable bytes (and other stuff) from the front of the queue
          ;; and returns them as strings (and other stuff).
          (define/private (split-queue converter q)
            (let ([lst (queue->list q)])
              (if (null? lst)
                  (values null (empty-queue))
                  (let loop ([lst lst]
                             [acc null])
                    (let-values ([(front rest) (peel lst)])
                      (cond
                        [(not front) (values (reverse acc)
                                             (empty-queue))]
                        [(bytes? (car front))
                         (let ([the-bytes (car front)]
                               [key (cdr front)])
                           (if (null? rest)
                               (let-values ([(converted-bytes src-read-k termination)
                                             (bytes-convert converter the-bytes)])
                                 (if (eq? termination 'aborts)
                                     (values (reverse (cons (cons (bytes->string/utf-8 converted-bytes) key) acc))
                                             (enqueue 
                                              (cons (subbytes the-bytes
                                                              (- (bytes-length the-bytes) src-read-k) 
                                                              (bytes-length the-bytes))
                                                    key)
                                              (empty-queue)))
                                     (values (reverse (cons (cons (bytes->string/utf-8 converted-bytes) key) acc))
                                             (empty-queue))))
                               (let-values ([(converted-bytes src-read-k termination)
                                             (bytes-convert converter the-bytes)]
                                            [(more-bytes more-termination) (bytes-convert-end converter)])
                                 (loop rest
                                       (cons (cons (bytes->string/utf-8 (bytes-append converted-bytes more-bytes))
                                                   key)
                                             acc)))))]
                        [else (loop rest
                                    (cons front acc))]))))))
          
          ;; peel : (listof (cons (union snip bytes) X)
          ;;     -> (values (cons (union snip bytes) X) (listof (cons (union snip bytes) X)
          ;; finds the first segment of bytes with the same style and combines them,
          ;; otherwise a lot like (define (peel x) (values (car x) (cdr x)))
          (define/private (peel lst)
            (let loop ([lst lst]
                       [acc #f]
                       [key #f])
              (cond
                [(null? lst) (values (cons acc key) null)]
                [else 
                 (let* ([fst (car lst)]
                        [fst-key (cdr fst)]
                        [fst-val (car fst)])
                   (cond
                     [(and (not key) (bytes? fst-val))
                      (loop (cdr lst)
                            fst-val
                            fst-key)]
                     [(and key (bytes? fst-val) (eq? key fst-key))
                      (loop (cdr lst)
                            (bytes-append acc fst-val)
                            key)]
                     [(not key)
                      (values fst (cdr lst))]
                     [else (if acc
                               (values (cons acc key) lst)
                               (values fst (cdr lst)))]))])))
          
          (super-new)))
      #|
      (define (drscheme-pretty-print-size-hook x _ port)
        (and (or (eq? port this-out)
                 (eq? port this-err)
                 (eq? port this-result))
             (cond
               [(is-a? x sized-snip<%>) (send x get-character-width)]
               [(is-a? x snip%) 
                (let ([dc (get-dc)]
                      [wbox (box 0)])
                  (send x get-extent dc 0 0 wbox #f #f #f #f #f)
                  (let-values ([(xw xh xa xd) (send dc get-text-extent "x")])
                    (max 1 (inexact->exact (ceiling (/ (unbox wbox) xw))))))]
               [(syntax? x) 
                ;; two spaces is about how big the turn down triangle
                ;; and the extra space accounts for. Of course, when
                ;; it is opened, this will be all wrong.
                (+ 2 (string-length (format "~s" x)))]
               [((use-number-snip) x)
                (let ([number-snip-type ((which-number-snip) x)])
                  (cond
                    [(memq number-snip-type '(repeating-decimal 
                                              repeating-decimal-e
                                              mixed-fraction
                                              mixed-fraction-e))
                     1] ;; no idea of size yet
                    [else 
                     (error 'which-number-snip
                            "unexpected result from parameter: ~e" 
                            number-snip-type)]))]
               [else #f])))
          
      (define (drscheme-pretty-print-print-hook x _ port)
        (let ([port-out-write
               (cond
                 [(eq? port this-out) (lambda (x) (this-out-write x))]
                 [(eq? port this-err) (lambda (x) (this-err-write x))]
                 [(eq? port this-result) (lambda (x) (this-result-write x))]
                 ;; this case should only happen if the user's program overrides the pretty-print-size-hook
                 ;; and doesnt' override the pretty-print-print-hook to match.
                 [else #f])])
          (if port-out-write
              (let ([snip/str
                     (cond
                       [(syntax? x) (render-syntax/snip x)]
                       [((use-number-snip) x)
                        (let ([number-snip-type ((which-number-snip) x)])
                          (cond
                            [(eq? number-snip-type 'repeating-decimal)
                             (drscheme:number-snip:make-repeating-decimal-snip x #f)]
                            [(eq? number-snip-type 'repeating-decimal-e)
                             (drscheme:number-snip:make-repeating-decimal-snip x #t)]
                            [(eq? number-snip-type 'mixed-fraction)
                             (drscheme:number-snip:make-fraction-snip x #f)]
                            [(eq? number-snip-type 'mixed-fraction-e)
                             (drscheme:number-snip:make-fraction-snip x #t)]
                            [else
                             (error 'which-number-snip
                                    "expected either 'repeating-decimal, 'repeating-decimal-e, 'mixed-fraction, or 'mixed-fraction-e got : ~e"
                                    number-snip-type)]))]
                       [else x])])
                (port-out-write snip/str))
              (display x))))
      
      ;; setup-display/write-handlers : -> void
      ;; sets the port-display-handler and the port-write-handler
      ;; for the initial output port, initial error port and the
      ;; value port.
      (define (setup-display/write-handlers)
        (let* ([make-setup-handler
                (lambda (port port-out-write)
                  (lambda (port-handler pretty)
                    (let ([original-handler (port-handler port)])
                      (port-handler
                       port
                       (rec drscheme-port-handler
                         (lambda (v p)
                           ;; avoid looping by calling original-handler
                           ;; for strings, since `pretty' calls write/display with
                           ;; strings
                           (if (string? v)
                               (original-handler v p)
                               (parameterize ([pretty-print-columns 'infinity])
                                 (pretty v p)))))))))]
               
               [setup-handlers
                (lambda (setup-handler)
                  (setup-handler port-display-handler pretty-display)
                  (setup-handler port-write-handler pretty-print))]
               
               [setup-out-handler (make-setup-handler this-out (lambda (x) (this-out-write x)))]
               [setup-err-handler (make-setup-handler this-err (lambda (x) (this-err-write x)))]
               [setup-value-handler (make-setup-handler this-result (lambda (x) (this-result-write x)))])
          (setup-handlers setup-out-handler)
          (setup-handlers setup-err-handler)
          (setup-handlers setup-value-handler)))
      
      |#
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; queues
      ;;
      (define-struct queue (eles count))
      (define (empty-queue) (make-queue '() 0))
      (define (enqueue e q) (make-queue (append (queue-eles q) (list e))
                                        (+ (queue-count q) 1)))
      (define (queue-first q)
        (let ([eles (queue-eles q)])
          (if (null? eles)
              (error 'queue-first "empty queue")
              (car eles))))
      (define (queue-rest q)
        (let ([eles (queue-eles q)])
          (if (null? eles)
              (error 'queue-rest "empty queue")
              (make-queue (cdr eles)
                          (- (queue-count q) 1)))))
      (define (queue-empty? q) (null? (queue-eles q)))
      (define (queue-size q) (queue-count q))
      
      ;; queue->list : (queue x) -> (listof x)
      ;; returns the elements in the order that successive deq's would have
      (define (queue->list q) (queue-eles q))
      
      ;;
      ;;  end queue abstraction
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define basic% (basic-mixin (editor:basic-mixin text%)))
      (define hide-caret/selection% (hide-caret/selection-mixin basic%))
      (define nbsp->space% (nbsp->space-mixin basic%))
      (define delegate% (delegate-mixin basic%))
      (define standard-style-list% (editor:standard-style-list-mixin basic%))
      (define -keymap% (editor:keymap-mixin standard-style-list%))
      (define return% (return-mixin -keymap%))
      (define autowrap% (editor:autowrap-mixin -keymap%))
      (define file% (editor:file-mixin autowrap%))
      (define clever-file-format% (clever-file-format-mixin file%))
      (define backup-autosave% (editor:backup-autosave-mixin clever-file-format%))
      (define searching% (searching-mixin backup-autosave%))
      (define info% (info-mixin (editor:info-mixin searching%))))))
