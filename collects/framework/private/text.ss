
(module text mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
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
              [scheme : framework:scheme^])
      
      (rename [-keymap% keymap%])
      
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
            (when delegate
              (refresh-delegate)))
          
          (define/private (refresh-delegate)
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
            (send delegate end-edit-sequence))
          
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
            (when (and delegate success?)
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
