

;;;;;;;;;;;;;;; Constants ;;;;;;;;;;;;;;;;;;;;

; default spacing between items.
(define const-default-spacing 0)

; default margins:
(define const-default-x-margin 2)
(define const-default-y-margin 2)

; default spacing around edge of panel
(define const-default-border 0)

; the maximum hard-min-width of a gauge
(define const-max-gauge-length 150)

; maximum reasonable minimum width/height
(define max-min 10000)

; maximum reasonable margin
(define max-margin 1000)

;;;;;;;;;;;;;;; Helpers ;;;;;;;;;;;;;;;;;;;;

; this structure holds the information that a child will need to send
; to its parent when the parent must resize itself.
(define-struct child-info (x-posn                ; current x position w.r.t parent
			   y-posn                ; current y position
			   x-min y-min           ; includes margins!
			   x-margin y-margin     ; requested margin space
			   x-stretch y-stretch)) ; booleans indicating strechability

; get-two-int-values: a wrapper around functions that need to return
;   two results.
; input: function: a function which takes two boxes and returns results
;          in them.
; returns: the contents of the two boxes (as multiple values)
(define get-two-int-values
  (lambda (function)
    (let ([a (box 0)]
	  [b (box 0)])
      (function a b)
      (values (unbox a) (unbox b)))))

(define non-negative-number?
  (lambda (n)
    (and (real? n) (not (negative? n)))))

(define same-dimension?
  (lambda (new-dim current-dim)
    (or (= new-dim current-dim)
	(= new-dim -1))))

(define identity (lambda (x) x))

(define (check-reasonable-min who v)
  (unless (<= 1 v max-min)
    (error who "not a reasaonable minimum width: ~a" v)))

(define (check-reasonable-margin who v)
  (unless (<= 0 v max-margin)
    (error who "not a reasaonable margin size: ~a" v)))

(define (range-error who v hard-min-width max-min)
  (error who "value out-of-range: ~a not in: ~a to ~a"
	 v hard-min-width max-min))

; list-diff: computes the difference between two lists
; input: l1, l2: two lists
; returns:  a list of all elements in l1 which are not in l2.
(define list-diff
  (lambda (l1 l2)
    (let ([table (make-hash-table)])
      (for-each
       (lambda (item)
	 (hash-table-put! table item #t))
       l2)
      (let loop ([l l1])
	(cond
	 [(null? l) null]
	 [(hash-table-get table (car l) (lambda () #f))
	  (loop (cdr l))]
	 [else (cons (car l) (loop (cdr l)))])))))

(define (remq i l)
  (let loop ([l l])
    (cond
     [(null? l) null]
     [(eq? (car l) i) (remq i (cdr l))]
     [else (cons (car l) (loop (cdr l)))])))

(define ibeam (make-object wx:cursor% 'ibeam))

;;;;;;;;;;;;;;; wx- Class Construction ;;;;;;;;;;;;;;;;;;;;

; ------------- Mixins for common functionality --------------


(define make-window%
  (lambda (%)
    (class % args
      (rename [super-on-set-focus on-set-focus]
	      [super-on-kill-focus on-kill-focus])
      (private
	[top-level #f]
	[focus? #f]
	[container this])
      (public
	[get-container (lambda () container)]
	[set-container (lambda (c) (set! container c))]
	[get-window (lambda () this)]
	[dx (lambda () 0)]
	[dy (lambda () 0)]
	[get-edit-target (lambda () this)]
	[get-top-level
	 (lambda ()
	   (unless top-level
	     (let loop ([window this])
	       (cond
		[(or (is-a? window wx:frame%)
		     (is-a? window wx:dialog-box%)) 
		 (set! top-level window)]
		[else (loop (send window get-parent))])))
	   top-level)]
	[on-set-focus
	 (lambda ()
	   (send (get-top-level) set-focus-window this)
	   (set! focus? #t)
	   (super-on-set-focus))]
	[on-kill-focus
	 (lambda ()
	   (send (get-top-level) set-focus-window #f)
	   (set! focus? #f)
	   (super-on-kill-focus))]
	[has-focus? (lambda () focus?)])
      (sequence (apply super-init args)))))

; make-container% - for panels and top-level windows
(define (make-container% %) %)

; make-top-container%: adds the necessary functionality to wx:frame% and 
; wx:dialog-box%.
; input: base%: the base class from which to descend the new class.
;          Intended to be either wx:frame% or wx:dialog-box%, but can
;          be anything which contains all methods in the inherit section
;          below.
; returns: a new class, descended from base%, which possesses the added
;            capabilities necessary to serve as the frame/dialog which
;            contains container classes.
(define (make-top-container% base%)
  (class (make-container% (make-window% base%)) args
    (inherit get-x get-y get-width get-height
	     get-client-size is-shown?)
    (rename [super-show show]
	    [super-on-size on-size]
	    [super-set-size set-size]
	    [super-enable enable])
    
    (private
      ; have we had any redraw requests while the window has been
      ; hidden?
      [pending-redraws? #f]

      [perform-updates? #t]
      
      [ignore-redraw-request? #f]
      
      [already-trying? #f] ; hack around stubborn Motif bug
      
      ; pointer to panel in the frame for use in on-size
      [panel #f]
      
      [enabled? #f]
      [focus #f]
      [target #f])
    
    (public
      [enable
       (lambda (b)
	 (set! enabled? (and b #t))
	 (super-enable b))]
      [is-enabled?
       (lambda () enabled?)]

      [set-focus-window
       (lambda (w)
	 (set! focus w)
	 (let ([t (and w (send w get-edit-target))])
	   (when t
	     (set! target t))))]
      
      [get-focus-window
       (lambda () focus)]
      [get-edit-target-window
       (lambda () target)]
      [get-focus-object
       (lambda ()
	 (window->focus-object focus))]
      [get-edit-target-object
       (lambda ()
	 (window->focus-object target))]

      [window->focus-object
       (lambda (w)
	 (and w
	      (if (is-a? focus wx:media-edit%)
		  (let loop ([m (send focus get-media)]
			     [prev w])
		    (if m
			(let ([snip (send m get-focus-snip)])
			  (if (and snip (is-a? snip wx:media-snip%))
			      (loop (send snip get-media) m)
			      m))
			w)))))]

      ; add-child: update panel pointer.
      ; input: new-panel: panel in frame (descendant of
      ;   panel%) 
      ; returns: nothing
      ; effects: sets panel to new-panel
      ;          if new-panel is not a descendant of
      ;            panel%, calls error; panel not updated.
      [add-child
       (lambda (new-panel)
	 (set! panel new-panel)
	 (let-values ([(client-w client-h)
		       (get-two-int-values get-client-size)])
	   (send panel set-size 0 0 client-w client-h))
	 (self-redraw-request))]
      
      [get-top-panel
       (lambda ()
	 panel)]

      [delay-updates
       (case-lambda
	[() (not perform-updates?)]
	[(f) (set! perform-updates? (not f))
	     (when pending-redraws?
	       (force-redraw))])]

      ; force-redraw: receives a message from to redraw the
      ; entire frame.
      ; input: none
      ; returns: nothing
      ; effects: redraws the frame at its current size (changing size
      ;            as necessary).
      [child-redraw-request
       ; since there's only one panel, we assume that `from' is the
       ; panel and the request should be granted
       (lambda (from) 
	 (unless ignore-redraw-request?
	   (self-redraw-request)))]
      [self-redraw-request
       (lambda ()
	 (if (and (is-shown?) perform-updates?)
	     (force-redraw)
	     (set! pending-redraws? #t)))]
      [force-redraw
       (lambda ()
	 (when panel
	   (dynamic-wind
	    (lambda () (set! ignore-redraw-request? #t))
	    (lambda () 
	      ; Ensures that the frame is big enough:
	      (set-size (get-x) (get-y) (get-width) (get-height))
	      (send panel on-container-resize))
	    (lambda () (set! ignore-redraw-request? #f))))
	 (set! pending-redraws? #f))]
      
      ; show: add capability to set perform-updates
      ; input: now : boolean
      ; returns: nothing
      ; effects: if we're showing for the first time, unblock updates
      ;            and force an update.  If we're hiding, block updates.
      ;          pass now to superclass's show.
      [show
       (lambda (now)
	 (when (and now pending-redraws?)
	   (force-redraw))
	 (super-show now))]
      
      [set-size
       (lambda (x y width height)
	 (let-values ([(correct-w correct-h)
		       (correct-size width height)])
	   (if (and (same-dimension? x (get-x))
		    (same-dimension? y (get-y))
		    (and (same-dimension? width (get-width))
			 (= width correct-w))
		    (and (same-dimension? height (get-height))
			 (= height correct-h)))
	       (when (get-top-panel)
		 (let-values ([(f-client-w f-client-h)
			       (get-two-int-values get-client-size)])
		   (send panel set-size 0 0 f-client-w f-client-h)))
	       (super-set-size x y correct-w correct-h))))]
      
      [correct-size
       (lambda (frame-w frame-h)
	 (if (not panel)
	     (values frame-w frame-h)
	     (let-values ([(f-client-w f-client-h)
			   (get-two-int-values get-client-size)])
	       (let* ([panel-info (send panel get-info)]
		      
		      ; difference between panel's full size & 
		      ; frame's full size (tweaked for wm)
		      [delta-w (- (get-width) f-client-w)]
		      [delta-h (- (get-height) f-client-h)]

		      ; minimum frame size:
		      [min-w (+ delta-w (child-info-x-min panel-info))]
		      [min-h (+ delta-h (child-info-y-min panel-info))]
		      
		      ; correct size for frame
		      [new-w
		       (cond
			[(< frame-w min-w) min-w]
			[(and (> frame-w min-w)
			      (not (child-info-x-stretch panel-info)))
			 min-w]
			[else frame-w])]
		      [new-h
		       (cond
			[(< frame-h min-h) min-h]
			[(and (> frame-h min-h)
			      (not (child-info-y-stretch panel-info)))
			 min-h]
			[else frame-h])])
		 (values new-w new-h)))))]
      
      ; on-size: ensures that size of frame matches size of content
      ; input: new-width/new-height: new size of frame
      ; returns: nothing
      ; effects: if new size is smaller than allowed size of
      ;            contents, frame resized to smallest possible size.
      ;            If frame is larger than contents and contents
      ;            aren't stretchable, frame resized to size of
      ;            contents.  Each direction is handled
      ;            independently.
      [on-size
       (opt-lambda (new-width new-height [force? #f])
	 (super-on-size new-width new-height)
	 (unless already-trying?
	   (let ([new-width (get-width)]
		 [new-height (get-height)])
	     (let-values ([(correct-w correct-h)
			   (correct-size new-width new-height)])
	       (unless (and (= new-width correct-w)
			    (= new-height correct-h)
			    (not force?))
		 (set! already-trying? #t)
		 (set-size -1 -1 correct-w correct-h)
		 (set! already-trying? #f))))))])
    (sequence
      (apply super-init args))))

; make-item%: creates items which are suitable for placing into
;  containers.
; input: item%: a wx:item% descendant (but see below) from which the
;          new class will be derived.
;        stretch-x/stretch-y: booleans which specify the default
;          stretchability behavior for the new class.
; returns: a class, descended from wx:item%, which is suitable for
;            placing in a container.
; Note: the item% parameter does not necessarily HAVE to be a
; descendant of wx:item%, so long as it contains the identifiers in the
; inherit section below.  You will note below that I ran wx:panel%
; through this function to create panel%.

(define make-item%
  (polymorphic
   (lambda (item% x-margin-w y-margin-h stretch-x stretch-y)
     (class (make-window% item%) args
       (rename [super-on-set-focus on-set-focus]
	       [super-on-kill-focus on-kill-focus])
       (inherit get-width get-height get-x get-y
		get-parent get-client-size)
       (rename [super-enable enable])
       (private [enabled? #t])
       (public
	 [enable
	  (lambda (b)
	    (set! enabled? (and b #t))
	    (super-enable b))]
	 [orig-enable
	  (lambda args (apply super-enable args))]
	 [is-enabled?
	  (lambda () enabled?)])

       (rename
	[super-set-size set-size])
       
       (public
	 ; Store minimum size of item.  
	 ; This will never change after the item is created.
	 hard-min-width
	 hard-min-height
	 [set-min-height (lambda (v) (set! hard-min-height v) (min-height v))]
	 [set-min-width (lambda (v) (set! hard-min-width v) (min-width v))]
	 
	 [client-inset
	  (lambda (h?)
	    (let ([h #f][w #f])
	      (unless h
		(let ([w-box (box 0)]
		      [h-box (box 0)])
		  (get-client-size w-box h-box)
		  (set! h (- (get-height) (unbox h-box)))
		  (set! w (- (get-width) (unbox w-box)))))
	      (if h? h w)))]

	 ; gets/sets user's requirement for minimum width.  Errors out
	 ; if new value is not a non-negative real number.  Forces a
	 ; redraw upon a set.
	 [min-client-width
	  (case-lambda 
	   [() (- (min-width) (client-inset #f))]
	   [(new-width) (min-width (+ new-width (client-inset #f)))])]
	 [min-client-height
	  (case-lambda 
	   [() (- (user-min-height) (client-inset #t))]
	   [(new-height) (user-min-height (+ new-height (client-inset #t)))])]

	 [mk-param
	  (lambda (val filter check)
	    (case-lambda
	     [() val]
	     [(v) (check v)
		  (let ([v2 (filter v)])
		    (unless (eq? v2 val)
		      (set! val v2)
		      (force-redraw)))]))]
	 
	 [min-width
	  (mk-param
	   0 identity
	   (lambda (v)
	     (check-reasonable-min 'min-width v)
	     (when (< v hard-min-width)
	       (range-error 'min-width v hard-min-width max-min))))]
	 [min-height
	  (mk-param
	   0  identity
	   (lambda (v)
	     (check-reasonable-min 'min-height v)
	     (when (< v hard-min-height)
	       (range-error 'min-height v hard-min-height max-min))))]
	 
	 [x-margin
	  (mk-param
	   x-margin-w identity
	   (lambda (v)
	     (check-reasonable-margin 'x-margin-width v) v))]
	 [y-margin
	  (mk-param
	   y-margin-h identity
	   (lambda (v) (check-reasonable-margin 'y-margin-width v)))]

	 [stretchable-in-x
	  (mk-param stretch-x (lambda (x) (and x #t)) void)]
	 [stretchable-in-y
	  (mk-param stretch-y (lambda (x) (and x #t)) void)]
	  
	 ; get-info: passes necessary info up to parent.
	 ; input: none
	 ; returns: child-info struct containing the info about this
	 ;   item.
	 ; intended to be called by item's parent upon resize.
	 [get-info
	   (lambda ()
	     (let* ([min-size (get-min-size)]
		    [result (make-child-info (get-x) (get-y)
					     (car min-size) (cadr min-size)
					     (x-margin) (y-margin)
					     (stretchable-in-x)
					     (stretchable-in-y))])
	       result))]
	  
	 [area-parent (lambda () (car args))]

	  ; force-redraw: unconditionally trigger redraw.
	  ; input: none
	  ; returns: nothing
	  ; effects: forces the item's parent (if it exists) to redraw
	  ;   itself. This will recompute the min-size cache if it is
	  ;   invalid.
	  [force-redraw
	   (lambda ()
	     (let ([parent (area-parent)])
	       (unless parent
		 (send parent child-redraw-request this))))]
	  
	  ; set-size: caches calls to set-size to avoid unnecessary work,
	  ;           and works with windowsless panels
	  ; input: x/y: new position for object
	  ;        width/height: new size for object
	  ; returns: nothing
	  ; effect: if arguments mark a different geometry than the object's
	  ;   current geometry, passes args to super-class's set-size.
	  ;   Otherwise, does nothing.
	  [set-size
	   (lambda (x y width height)
	     (set! x (+ x (send (area-parent) dx)))
	     (set! y (+ y (send (area-parent) dy)))
	     (unless (and (same-dimension? x (get-x))
			  (same-dimension? y (get-y))
			  (same-dimension? width (get-width))
			  (same-dimension? height (get-height)))
	       (super-set-size x y width height)))]

	  [on-container-resize void] ; This object doesn't contain anything
	  
	  ; get-min-size: computes the minimum size the item can
	  ;   reasonably assume.
	  ; input: none
	  ; returns: a list containing the minimum width & height.
	  [get-min-size
	   (lambda ()
	     (let ([w (+ (* 2 (x-margin)) (min-width))]
		   [h (+ (* 2 (y-margin)) (min-height))])
	       (list w h)))])
	 
	 (sequence
	   (apply super-init (send (car args) get-window) (cdr args))
	   (set-min-width (get-width))
	   (set-min-height (get-height))
	   
	   (send (area-parent) add-child this))))))

; make-control% - for non-panel items
(define (make-control% item% x-margin y-margin
		       stretch-x stretch-y)
  (class (make-item% item% x-margin y-margin
		     stretch-x stretch-y)
      args
    (inherit get-parent)
    (sequence
      (apply super-init args)
      (send (get-parent) set-item-cursor 0 0))))

(define (make-simple-control% item%)
  (make-control% item%
		 const-default-x-margin const-default-y-margin 
		 #f #f))

;------------- Mixins for glue to mred classes -----------------

(define wx<%> (interface () get-mred))
(define wx/proxy<%> (interface (wx<%>) get-proxy))

(define (make-glue% %)
  (class* % (wx/proxy<%>) (mred proxy . args)
    (public
      [get-mred (lambda () mred)]
      [get-proxy (lambda () proxy)])
    (sequence (apply super-init args))))

(define (make-window-glue% %)  ; implies make-glue%
  (class (make-glue% %) (mred proxy . args)
    (rename [super-on-size on-size]
	    [super-on-set-focus on-set-focus]
	    [super-on-kill-focus on-kill-focus]
	    [super-pre-on-char pre-on-char]
	    [super-pre-on-event pre-on-event])
    (public
      [on-size (lambda (x y)
		 (super-on-size x y)
		 (and mred (send mred on-size x y)))]
      [on-set-focus (lambda ()
		      (super-on-set-focus)
		      (send proxy on-focus #t))]
      [on-kill-focus (lambda ()
		      (super-on-kill-focus)
		      (send proxy on-focus #f))]
      [pre-on-char (lambda (w e)
		     (super-pre-on-char w e)
		     (send proxy pre-on-char (wx->proxy w) e))]
      [pre-on-event (lambda (w e)
		      (super-pre-on-event w e)
		      (send proxy pre-on-event (wx->proxy w) e))])
    (sequence (apply super-init mred proxy args))))

(define (make-container-glue% %)
  (class % (mred proxy . args)
    (inherit do-place-children)
    (public
      [place-children (lambda (l w h) (cond
				       [(null? l) null]
				       [mred (send mred place-children l w h)]
				       [else (do-place-children l w h)]))])
    (sequence
      (apply super-init mred proxy args))))

(define (make-top-level-window-glue% %) ; implies make-window-glue%
  (class (make-window-glue% %) (mred proxy . args)
    (rename [super-on-activate on-activate])
    (public
      [on-activate (lambda (on?)
		     (super-on-activate on?)
		     (send mred on-activate on?))])
    (sequence (apply super-init mred proxy args))))

(define (make-canvas-glue% %) ; implies make-window-glue%
  (class (make-window-glue% %) (mred proxy . args)
    (rename [super-on-char on-char]
	    [super-on-event on-event]
	    [super-on-paint on-paint]
	    [super-on-scroll on-scroll])
    (public
      [on-char (lambda (e)
		 (if mred
		     (send mred on-char e)
		     (super-on-char e)))]
      [do-on-char (lambda (e) (super-on-char e))]
      [on-event (lambda (e)
		 (if mred
		     (send mred on-event e)
		     (super-on-event e)))]
      [do-on-event (lambda (e) (super-on-event e))]
      [on-scroll (lambda (e)
		 (if mred
		     (send mred on-scroll e)
		     (super-on-scroll e)))]
      [do-on-scroll (lambda (e) (super-on-scroll e))]
      [on-paint (lambda ()
		 (if mred
		     (send mred on-paint)
		     (super-on-paint)))]
      [do-on-paint (lambda () (super-on-paint))])
    (sequence (apply super-init mred proxy args))))

;------------- Create the actual wx classes -----------------

(define wx-frame%
  (make-top-level-window-glue% 
   (class (make-top-container% wx:frame%) args
     (rename [super-set-menu-bar set-menu-bar])
     (public
       [menu-bar #f]
       [set-menu-bar
	(lambda (mb)
	  (when mb (set! menu-bar mb))
	  (super-set-menu-bar mb))]
       [on-menu-command
	(lambda (id)
	  (let ([wx (wx:id-to-menu-item id)])
	    (send (wx->mred wx) go)))])
     (sequence
       (apply super-init args)))))

(define wx-dialog-box%
  (make-top-level-window-glue% 
   (class (make-top-container% wx:dialog-box%) args
     (sequence
       (apply super-init args)))))

(define wx-button% (make-window-glue% (make-simple-control% wx:button%)))
(define wx-check-box% (make-window-glue% (make-simple-control% wx:check-box%)))
(define wx-choice% (make-window-glue% (make-simple-control% wx:choice%)))
(define wx-message% (make-window-glue% (make-simple-control% wx:message%)))

(define wx-gauge%
 (make-window-glue% 
  (class (make-control% wx:gauge% 
			const-default-x-margin const-default-y-margin 
			#t #f)
      (parent label range style)
    (inherit get-client-size get-width get-height
	     set-size 
	     stretchable-in-x stretchable-in-y set-min-height set-min-width
	     get-parent)
    (private
      ; # pixels per unit of value.
      [pixels-per-value 1])
    (sequence
      (super-init parent label range -1 -1 -1 -1 style)

      (let-values ([(client-width client-height)
		    (get-two-int-values get-client-size)])
	(let ([delta-w (- (get-width) client-width)]
	      [delta-h (- (get-height) client-height)]
	      [vertical-labels? (eq? (send (get-parent) get-label-position)
				     'vertical)]
	      [horizontal (eq? 'horizontal style)])
	  (set-min-width (if horizontal
			     (let ([cw (min const-max-gauge-length
					    (* range pixels-per-value))])
			       (if vertical-labels?
				   (max cw (get-width))
				   (+ cw delta-w)))
			     ; client-height is the default
			     ; dimension in the minor direction.
			     (+ client-width delta-w)))
	  (set-min-height (if horizontal
			      (+ client-height delta-h)
			      (let ([ch (min const-max-gauge-length
					     (* range pixels-per-value))])
				(if vertical-labels?
				    (+ ch delta-h)
				    (max ch (get-height))))))))

      (if (memq 'horizontal style)
	  (begin
	    (stretchable-in-x #t)
	    (stretchable-in-y #f))
	  (begin
	    (stretchable-in-x #f)
	    (stretchable-in-y #t)))))))

(define wx-list-box%
  (make-window-glue% 
   (make-control% wx:list-box%
		  const-default-x-margin const-default-y-margin 
		  #t #t)))

(define wx-radio-box%
  (make-window-glue% 
   (class (make-simple-control% wx:radio-box%) args
     (inherit number orig-enable)
     (rename [super-enable enable]
	     [super-is-enabled? is-enabled?])
     (public
       [enable
	(case-lambda
	 [(on?) (super-enable on?)]
	 [(which on?) (when (< -1 which (number))
			(vector-set! enable-vector which (and on? #t))
			(orig-enable which on?))])]
       [is-enabled?
	(case-lambda
	 [() (super-is-enabled?)]
	 [(which) (and (< -1 which (number))
		       (vector-ref enable-vector which))])])

     (sequence (apply super-init args))

     (private [enable-vector (make-vector (number) #t)]))))

(define wx-slider%
  (make-window-glue% 
   (class (make-control% wx:slider% 
			 const-default-x-margin const-default-y-margin 
			 #t #f)
       (parent func label value min-val max-val style)
     (inherit set-min-width set-min-height stretchable-in-x stretchable-in-y
	      get-client-size get-width get-height)
     (private
       ; # pixels per possible setting.
       [pixels-per-value 3])
     ; 3 is good because with horizontal sliders under Xt, with 1 or 2
     ; pixels per value, the thumb is too small to display the number,
     ; which looks bad.
     
     (sequence
       (super-init parent func label value min-val max-val -1 -1 -1 style)
       
       (let-values ([(client-w client-h) (get-two-int-values get-client-size)])
	 (let ([range (* pixels-per-value (add1 (- max-val min-val)))]
	       [horizontal? (memq 'horizontal style)])
	   (when (not horizontal?)
	     (stretchable-in-x #f)
	     (stretchable-in-y #t))
	   ((if horizontal? set-min-width set-min-height) (min const-max-gauge-length range))))))))

(define wx-canvas% (make-canvas-glue% (make-control% wx:canvas% 0 0 #t #t)))

;--------------------- wx media Classes -------------------------

(define (make-media-canvas% %)
  (class % (parent x y w h name style spp init-buffer)
    (inherit get-media force-redraw
	     call-as-primary-owner min-height get-size
	     hard-min-height set-min-height)
    (private
      [fixed-height? #f]
      [fixed-height-lines 0]
      [edit-target this]
      [orig-hard #f])
    (public
      [on-container-resize (lambda ()
			     (let ([edit (get-media)])
			       (when edit
				 (send edit on-display-size))))])
    (rename [super-set-media set-media]
	    [super-on-set-focus on-set-focus])
    (public       
      [set-edit-target (lambda (t) (set! edit-target t))]
      [get-edit-target (lambda () edit-target)]

      [set-media
       (letrec ([l (case-lambda
		    [(media) (l media #t)]
		    [(media redraw?)
		     (super-set-media media redraw?)
		     
		     (let ([mred (wx->mred this)])
		       (when mred
			 (send media add-canvas mred)))

		     (update-size)
		     
		     ; force-redraw causes on-container-resize to be called,
		     ;  but only when the size of the canvas really matters
		     ;  (i.e., when it is shown)
		     (force-redraw)])])
	 l)]

      [on-set-focus
       (lambda ()
	 (super-on-set-focus)
	 (let ([m (get-media)])
	   (when m 
	     (let ([mred (wx->mred this)])
	       (when mred
		 (send m set-active-canvas mred))))))]

      [set-line-count (lambda (n)
			(if n
			    (begin
			      (unless orig-hard
				(set! orig-hard hard-min-height))
			      (set! fixed-height? #t)
			      (set! fixed-height-lines n))
			    (begin
			      (set! fixed-height? #f)
			      (set-min-height orig-hard)))
			(update-size))]

      [update-size
       (lambda ()
	 (let ([media (get-media)])
	   (when (and media fixed-height?)
	     (let* ([top (send media line-location 0 #t)]
		    [bottom (send media line-location 0 #f)]
		    [height (- bottom top)])
	       (let* ([ch (box 0)]
		      [h (box 0)])
		 (call-as-primary-owner
		  (lambda ()
		    (send (send media get-admin) 
			  get-view #f #f #f ch)))
		 (get-size (box 0) h)
		 (let ([new-min-height (+ (* fixed-height-lines height) 
					  (- (unbox h) (unbox ch)))])
		   (set-min-height new-min-height)))))))])
    
    (sequence
      (super-init parent x y w h (or name "") style spp init-buffer)
      (when init-buffer
	(let ([mred (wx->mred this)])
	  (when mred
	    (send init-buffer add-canvas mred)))))))

(define wx-media-canvas% (make-canvas-glue%
			  (make-media-canvas% (make-control% wx:media-canvas%
							     0 0 #t #t))))

(define (make-media-buffer% % can-wrap?)
  ; >>> This class is instantiated directly by the end-user <<<
  (class % args
    (inherit get-max-width set-max-width get-admin)
    (rename [super-set-modified set-modified]
	    [super-set-filename set-filename]
	    [super-on-display-size on-display-size])
    (private
      [canvases null]
      [active-canvas #f]
      [auto-set-wrap? #f])
    (public
      [get-canvases (lambda () (map wx->mred canvases))]
      [get-active-canvas (lambda () (and active-canvas (wx->mred active-canvas)))]
      [get-canvas
       (lambda ()
	 (let ([c (or active-canvas
		      (and (not (null? canvases))
			   (car canvases)))])
	   (and c (wx->mred c))))]
      [set-filename
       (letrec ([l (case-lambda 
		    [(name) (l name #f)]
		    [(name temp?)
		     (super-set-filename name temp?)])])
	 l)]

      [set-active-canvas
       (lambda (new-canvas)
	 (set! active-canvas (mred->wx new-canvas)))]

      [add-canvas
       (lambda (new-canvas)
	 (let ([new-canvas (mred->wx new-canvas)])
	   (unless (memq new-canvas canvases)
	     (set! canvases (cons new-canvas canvases)))))]

      [remove-canvas
       (lambda (old-canvas)
	 (let ([old-canvas (mred->wx old-canvas)])
	   (when (eq? old-canvas active-canvas)
	     (set! active-canvas #f))
	   (set! canvases (remq old-canvas canvases))))]

      [auto-wrap (case-lambda
		  [() auto-set-wrap?]
		  [(on?) (set! auto-set-wrap? (and on? #t))
			 (on-display-size)])]
      [on-display-size
       (lambda ()
	 (super-on-display-size)
	 (when (and can-wrap? auto-set-wrap?)
	   (let* ([current-width (get-max-width)]
		  [admin-width (lambda (a)
				 (let ([w-box (box 0)])
				   (send a get-view #f #f w-box (box 0))
				   (unbox w-box)))]
		  [new-width
		   (apply max
			  (let ([a (get-admin)])
			    (if a
				(admin-width a)
				-1))
			  (map
			   (lambda (canvas)
			     (send canvas call-as-primary-owner
				   (lambda ()
				     (admin-width (get-admin)))))
			   canvases))])
	     (when (and (not (= current-width new-width))
			(< 0 new-width))
	       (set-max-width new-width)))))]

      [on-new-box
       (lambda (type)
	 (make-object wx-media-snip%
		      (make-object
		       (cond
			[(eq? type 'pasteboard-buffer) (make-object media-pasteboard%)]
			[else (make-object media-edit%)]))))])

    (sequence (apply super-init args))))

(define media-edit% (make-media-buffer% wx:media-edit% #t))
(define media-pasteboard% (make-media-buffer% wx:media-edit% #f))

;--------------------- wx Panel Classes -------------------------

(define wx:windowless-panel%
  (class null (parent x y w h style)
    (private
      [pos-x 0] [pos-y 0] [width 1] [height 1])
    (public
      [on-set-focus void]
      [on-kill-focus void]
      [set-focus void]
      [on-size void]
      [enable void]
      [show void]
      [get-parent (lambda () parent)]
      [get-client-size (lambda (wb hb)
			 (when wb (set-box! wb width))
			 (when hb (set-box! hb height)))]
      [set-size (lambda (x y w h) 
		  (unless (negative? x) (set! pos-x x))
		  (unless (negative? y) (set! pos-y y))
		  (unless (negative? w) (set! width w))
		  (unless (negative? h) (set! height h)))]
      [get-x (lambda () pos-x)]
      [get-y (lambda () pos-y)]
      [get-width (lambda () width)]
      [get-height (lambda () height)])))

(define (wx-make-basic-panel% wx:panel%)
  (class (make-container% (make-item% wx:panel% 0 0 #t #t)) (parent style)
    (inherit get-x get-y get-width get-height
	     min-width min-height
	     x-margin y-margin
	     get-client-size area-parent)
    
    (rename [super-set-focus set-focus]
	    [super-on-size on-size]
	    [super-set-size set-size])
    
    (private
      ; cache to prevent on-size from recomputing its result every
      ; time. when curr-width is #f, cache invalid.
      curr-width
      curr-height
      
      ; list of child-info structs corresponding to the children.  (#f
      ;  if no longer valid.)
      [children-info null]
      
      [ignore-redraw-request? #f])
    
    (public
      [border
       (let ([curr-border const-default-border])
	 (case-lambda
	  [() curr-border]
	  [(new-val)
	   (check-reasonable-margin 'border new-val)
	   (set! curr-border new-val)
	   (force-redraw)]))]

      [set-focus ; dispatch focus to a child panel
       (lambda ()
	 (if (null? children)
	     (super-set-focus)
	     (send (car children) set-focus)))]
      
      ; list of panel's contents.
      [children null]
      [set-children (lambda (l) (set! children l))]
      
      ; add-child: adds an existing child to the panel.
      ; input: new-child: item% descendant to add
      ; returns: nothing
      ; effects: adds new-child to end of list of children.
      [add-child
       (lambda (new-child)
	 (unless (eq? this (send new-child area-parent))
	   (error 'add-child "not a child window"))
	 (change-children
	  (lambda (l)
	    (append l (list new-child)))))]
      
      ; change-children: changes the list of children.
      ; input: f is a function which takes the current list of children
      ;   and returns a new list of children.
      ; returns: nothing
      ; effects: sets the list of children to the value of applying f.
      [change-children
       (lambda (f)
	 (let ([new-children (f children)])
	   (unless (andmap (lambda (child)
			     (eq? this (send child area-parent)))
			   new-children)
	     (error 'change-children
		    (string-append 
		     "Not all members of the new list are "
		     "children of this panel ~s~nlist: ~s")
		    this new-children))
	   ; show all new children, hide all deleted children.
	   (let ([added-children (list-diff new-children children)]
		 [removed-children (list-diff children new-children)])
	     (for-each (lambda (child) (send child show #f))
		       removed-children)
	     (set! children new-children)
	     (force-redraw)
	     (for-each (lambda (child) (send child show #t))
		       added-children))))]
      
      ; delete-child: removes a child from the panel.
      ; input: child: child to delete.
      ; returns: nothing
      ; effects: removes child from list; forces redraw.
      [delete-child
       (lambda (child)
	 (change-children (lambda (child-list)
			    (remq child child-list))))]
      
      ; get-children-info: returns children info list, recomputing it
      ;   if needed.
      ; input: none
      ; returns: list of child-info structs.
      ; effects: upon exit, children-info is eq? to result.
      [get-children-info
       (lambda ()
	 (unless children-info
	   (set! children-info
		 (map (lambda (child)
			(send child get-info))
		      children)))
	 children-info)]
      
      ; force-redraw: forces a redraw of the entire window.
      ; input: none
      ; returns: nothing
      ; effects: sends a message up to the top container to redraw
      ;   itself and all of its children.
      [child-redraw-request
       (lambda (from)
	 (unless (or ignore-redraw-request?
		     (not (memq from children)))
	   (force-redraw)))]
      [force-redraw
       (lambda ()
	 (set! children-info #f)
	 (set! curr-width #f)
	 (let ([parent (area-parent)])
	   (send parent child-redraw-request this)))]

      ; do-graphical-size: creates a function which returns the minimum
      ;   possible size for a horizontal-panel% or vertical-panel% object.
      ; input: compute-x/compute-y: functions which take the current x/y
      ;          location, the amount of spacing which will come after the
      ;          current object, and the list of child-info structs beginning
      ;          with the current object, and return the new x/y locations.
      ; returns: a thunk which returns the minimum possible size of the
      ;   entire panel (not just client) as a list of two elements:
      ;   (min-x min-y). 
      [do-graphical-size
	(lambda (compute-x compute-y)
	  (letrec ([gms-help
		    (lambda (kid-info x-accum y-accum)
		      (if (null? kid-info)
			  (list x-accum y-accum)
			  (gms-help
			   (cdr kid-info)
			   (compute-x x-accum kid-info)
			   (compute-y y-accum kid-info))))])
	    (let-values ([(client-w client-h)
			  (get-two-int-values get-client-size)])
	      (let* ([border (border)]
		     [min-client-size
		      (gms-help (get-children-info)
				(* 2 border) (* 2 border))]
		     [delta-w (- (get-width) client-w)]
		     [delta-h (- (get-height) client-h)])
		(list (+ delta-w (car min-client-size))
		      (+ delta-h (cadr min-client-size)))))))]
      
      ; get-min-graphical-size: poll children and return minimum possible
      ;   size, as required by the graphical representation of the tree,
      ;   of the panel.
      ; input: none
      ; returns: minimum full size (as a list, width & height) of the
      ;   container.
      ; effects: none
      [get-graphical-min-size
       (lambda ()
	 (do-graphical-size 
	  (lambda (x-accum kid-info)
	    (max x-accum (+ (* 2 (border))
			    (child-info-x-min (car kid-info)))))
	  (lambda (y-accum kid-info)
	    (max y-accum (+ (* 2 (border))
			    (child-info-y-min (car kid-info)))))))]
      
      ; get-min-size: poll children and return minimum possible size
      ;   for the container which considers the user min sizes.
      ; input: none
      ; returns: minimum full size (as a list, width & height) of
      ;   container.
      ; effects: none.
      [get-min-size
       (lambda ()
	 (let ([graphical-min-size (get-graphical-min-size)])
	   (list (+ (* 2 (x-margin))
		    (max (car graphical-min-size) (min-width)))
		 (+ (* 2 (y-margin))
		    (max (cadr graphical-min-size) (min-height))))))]
      
      ; set-size:
      [set-size
       (lambda (x y width height)
	 (unless (and (same-dimension? x (get-x))
		      (same-dimension? y (get-y))
		      (same-dimension? width (get-width))
		      (same-dimension? height (get-height)))
	   (super-set-size x y width height)))]
      
      ; on-size: called when the container is resized (usu by its
      ;   parent) 
      ; input: new-width/new-height: new size of panel
      ; returns: nothing
      ; effects: causes children to redraw themselves.
      [on-size
       (lambda (new-width new-height)
	 (super-on-size new-width new-height)
	 (force-redraw))]
      
      [on-container-resize
       (lambda ()
	 (let-values ([(client-width client-height)
		       (get-two-int-values get-client-size)])
	   (unless (and (number? curr-width)
			(number? curr-height)
			(= curr-width client-width)
			(= curr-height client-height))
	     (set! curr-width client-width)
	     (set! curr-height client-height)
	     (redraw client-width client-height))))]
      
      ; place-children: determines where each child of panel should be
      ; placed.
      ; input: children-info: list of child-info structs
      ;          corresponding to children.
      ;        width/height: size of panel's client area.
      ; returns: list of placement info for children; each item in list
      ;   is a list of 4 elements, consisting of child's x-posn,
      ;   y-posn, x-size, y-size (including margins).  Items are in same 
      ;   order as children-info list.
      [place-children void]
      [do-place-children
       (lambda (children-info width height)
	 (let loop ([children-info children-info])
	   (if (null? children-info)
	       null
	       (let ([curr-info (car children-info)])
		 (cons
		  (list
		   (child-info-x-margin curr-info)
		   (child-info-y-margin curr-info)
		   (child-info-x-min curr-info)
		   (child-info-y-min curr-info))
		  (loop (cdr children-info)))))))]
      
      ; redraw: redraws panel and all children
      ; input: width, height: size of area area in panel.
      ; returns: nothing
      ; effects: places children at default positions in panel.
      [redraw
       (lambda (width height)
	 (let ([children-info (get-children-info)])
	   (panel-redraw children children-info
			 (place-children children-info width height))))]
      [panel-redraw
       (lambda (childs child-infos placements)
	 (for-each
	  (lambda (child info placement)
	    (let-values ([(x y w h) (apply values placement)])
	      (let ([xm (child-info-x-margin info)]
		    [ym (child-info-y-margin info)])
		(dynamic-wind
		 (lambda () (set! ignore-redraw-request? #t))
		 (lambda ()
		   (send child set-size
			 (+ x xm) (+ y ym)
			 (max 1 (- w (* 2 xm)))
			 (max 1 (- h (* 2 ym)))))
		 (lambda () (set! ignore-redraw-request? #f)))
		(send child on-container-resize))))
	  childs
	  child-infos
	  placements))])
    (sequence
      (super-init parent -1 -1 -1 -1 style))))

(define (wx-make-pane% wx:panel%)
  (class (make-container-glue% (make-glue% (wx-make-basic-panel% wx:panel%))) args
    (inherit get-parent get-x get-y)
    (public
      [get-window (lambda () (send (get-parent) get-window))]
      [dx (lambda () (get-x))]
      [dy (lambda () (get-y))])
    (sequence
      (apply super-init args))))

(define (wx-make-panel% wx:panel%)
   (make-container-glue% (make-window-glue% (wx-make-basic-panel% wx:panel%))))

(define (wx-make-linear-panel% wx-panel%)
  (class wx-panel% args
    (private
      [major-align-pos 'left]
      [minor-align-pos 'center])
    
    (inherit force-redraw border get-width get-height
	     get-graphical-min-size)
    (public
      [do-align (lambda (h v set-h set-v)
		  (unless (memq h '(left center right))
		    (raise-type-error 'alignment "horizontal alignment symbol: left, center, or right" h))
		  (unless (memq v '(top center bottom))
		    (raise-type-error 'alignment "vertical alignment symbol: top, center, or bottom" v))
		  (set-h h)
		  (set-v (case v [(top) 'left] [(center) 'center] [(bottom) 'right])))]
      [do-get-alignment (lambda (pick) (values (pick major-align-pos minor-align-pos)
					       (case (pick minor-align-pos major-align-pos)
						 [(top) 'left] [(center) 'center] [(right) 'bottom])))]
      [minor-align (lambda (a) (set! minor-align-pos a) (force-redraw))]
      [major-align (lambda (a) (set! major-align-pos a) (force-redraw))]
      [major-offset (lambda (space)
		      (case major-align-pos
			[(center) (/ space 2)]
			[(left) 0]
			[(right) space]))]
      [minor-offset (lambda (width size)
		      (case minor-align-pos
			[(center) (/ (- width size) 2)]
			[(left) 0]
			[(right) (- width size)]))]
      
      [spacing
       (let ([curr-spacing const-default-spacing])
	 (case-lambda
	  [() curr-spacing]
	  [(new-val)
	   (check-reasonable-margin 'spacing new-val)
	   (set! curr-spacing new-val)
	   (force-redraw)]))]

      ; place-linear-children: implements place-children functions for
      ; horizontal-panel% or vertical-panel% classes.
      ; input: child-major-size: function which takes a child-info struct
      ;          and returns the child's minimum size in the major direction
      ;          of the panel.
      ;        child-major-stretch: function which takes a child-info
      ;          struct and returns the child's stretchability in the major
      ;          direction of the panel.
      ;        child-minor-size/child-minor-stretch: see above.
      ;        major-dim/minor-dim: functions which take the width and the
      ;          height of the panel and return the panel's major and minor
      ;          dimensions, respectively.
      ;        get-h-info/get-v-info: functions which take info lists
      ;          describing the major and minor directions and select the
      ;          appropriate one.
      ; returns: a function which takes the children info, the width and the
      ;   height of the panel's client and returns a list which contains
      ;   posn&size info for each child. 
      [place-linear-children
       (lambda (kid-info width height
			 child-major-size
			 child-major-stretch
			 child-major-offset
			 child-minor-size
			 child-minor-stretch
			 child-minor-position
			 major-dim minor-dim
			 get-x-info get-y-info)
	 (letrec ([count-stretchable
		   (lambda (kid-info)
		     (if (null? kid-info)
			 0
			 (let ([curr-info (car kid-info)])
			   (if (child-major-stretch curr-info)
			       (add1 (count-stretchable (cdr kid-info)))
			       (count-stretchable (cdr kid-info))))))])
	   (let* ([spacing (spacing)]
		  [border (border)]
		  [full-w (get-width)]
		  [full-h (get-height)]
		  [delta-list (list
			       (- full-w width)
			       (- full-h height))]
		  [num-stretchable (count-stretchable kid-info)]
		  [extra-space (- (major-dim width height)
				  (- (apply 
				      major-dim
				      (get-graphical-min-size))
				     (apply major-dim delta-list)))]
		  [extra-per-stretchable (if (zero? num-stretchable)
					     0
					     (inexact->exact
					      (floor
					       (/ extra-space
						  num-stretchable))))]
		  [leftover (- extra-space (* extra-per-stretchable num-stretchable))]
		  [num-children (length kid-info)]
		  [major-offset (if (= num-stretchable 0)
				    (child-major-offset extra-space)
				    0)])
	     (letrec
		 ([pc-help
		   (lambda (kid-info left-edge leftover)
		     (if (null? kid-info)
			 null
			 (let* ([curr-info (car kid-info)]
				[rest (cdr kid-info)]
				[major-posn left-edge]
				[next-leftover (if (zero? leftover)
						   0
						   (- leftover 1))]
				[extra-this-stretchable (if (zero? leftover)
							    extra-per-stretchable
							    (+ extra-per-stretchable 1))]
				[major-size
				 (if (child-major-stretch curr-info)
				     (+ extra-this-stretchable
					(child-major-size curr-info))
				     (child-major-size curr-info))]
				[minor-posn (if (child-minor-stretch
						 curr-info)
						border
						(inexact->exact
						 (round
						  (child-minor-position 
						   (minor-dim width height) 
						   (child-minor-size curr-info)))))]
				[minor-size (if (child-minor-stretch
						 curr-info)
						(- (minor-dim width height)
						   (* 2 border))
						(child-minor-size
						 curr-info))])
			   (cons
			    (list
			     (get-x-info major-posn minor-posn)
			     (get-y-info major-posn minor-posn)
			     (get-x-info major-size minor-size)
			     (get-y-info major-size minor-size))
			    (pc-help rest
				     (+ major-size major-posn spacing)
				     next-leftover)))))])
	       (pc-help kid-info (+ border major-offset) leftover)))))])
    
    (sequence (apply super-init args))))

; horizontal-panel%: a panel which arranges its children in an evenly
; spaced horizontal row.  Items are vertically centered (or stretched
; to fit the dialog box if they are stretchable).  The items are evenly
; spaced horizontally, with any extra space divided evenly among the
; stretchable items. 
(define (wx-make-horizontal-panel% wx-linear-panel%)
  (class wx-linear-panel% args
    (inherit major-align minor-align do-align do-get-alignment major-offset minor-offset
	     spacing border do-graphical-size place-linear-children)
    (public
      [alignment (lambda (h v) (do-align h v major-align minor-align))]
      [get-alignment (lambda () (do-get-alignment (lambda (x y) x)))])
    
    (public
      [get-graphical-min-size
       (lambda ()
	 (do-graphical-size 
	  (lambda (x-accum kid-info)
	    (+ x-accum (child-info-x-min (car kid-info))
	       (if (null? (cdr kid-info))
		   0
		   (spacing))))
	  (lambda (y-accum kid-info)
	    (max y-accum
		 (+ (child-info-y-min (car kid-info))
		    (* 2 (border)))))))]
      [do-place-children
       (lambda (l w h)
	 (place-linear-children l w h
				child-info-x-min
				child-info-x-stretch
				major-offset
				child-info-y-min
				child-info-y-stretch
				minor-offset
				(lambda (width height) width)
				(lambda (width height) height)
				(lambda (major minor) major)
				(lambda (major minor) minor)))])
    (sequence (apply super-init args))))

; vertical-panel%.  See horizontal-panel%, but reverse
; "horizontal" and "vertical."
(define (wx-make-vertical-panel% wx-linear-panel%)
  (class wx-linear-panel% args
    (inherit major-align minor-align do-align do-get-alignment major-offset minor-offset
	     spacing border do-graphical-size place-linear-children)
    (public
      [alignment (lambda (h v) (do-align h v minor-align major-align))]
      [get-alignment (lambda () (do-get-alignment (lambda (x y) y)))])
    
    (public
      [get-graphical-min-size
       (lambda ()
	 (do-graphical-size
	  (lambda (x-accum kid-info)
	    (max x-accum
		 (+ (child-info-x-min (car kid-info))
		    (* 2 (border)))))
	  (lambda (y-accum kid-info)
	    (+ y-accum (child-info-y-min (car kid-info))
	       (if (null? (cdr kid-info))
		   0
		   (spacing))))))]
      
      [do-place-children
       (lambda (l w h)
	 (place-linear-children l w h
				child-info-y-min
				child-info-y-stretch
				major-offset
				child-info-x-min
				child-info-x-stretch
				minor-offset
				(lambda (width height) height)
				(lambda (width height) width)
				(lambda (major minor) minor)
				(lambda (major minor) major)))])
    (sequence (apply super-init args))))

; implement a panel which can hold multiple objects but only displays
; one at a time.  The size of the panel is the smallest size possible
; for displaying each of the panel's children.
(define (wx-make-single-panel% wx-panel%)
  (class wx-panel% args
    
    (inherit children set-children force-redraw panel-redraw)
    
    (rename
     [super-add add-child]
     [super-delete delete-child])
    
    (public
      
      ; pointer to currently active child
      [active #f]
      
      [add-child
       (lambda (new-child)
	 (super-add new-child)
	 (send new-child show #f))]
      
      ; if the child is active, make the next child active (null if
      ; child was last in list)
      [delete-child
       (lambda (child)
	 (when (eq? child (active-child))
	   (let ([rest-of-list (cdr (memq child children))])
	     (active-child (if (null? rest-of-list)
			       null
			       (car rest-of-list)))))
	 (super-delete child))]
      
      ; if the active child is removed, make nothing active.
      [change-children
       (lambda (f)
	 (let ([new-children (f children)])
	   (unless (andmap (lambda (child)
			     (eq? this (send child area-parent)))
			   new-children)
	     (unless (memq (active-child) new-children)
	       (active-child #f))
	     (set-children new-children)
	     (force-redraw))))]
      
      [active-child
       (case-lambda
	[() active]
	[(new-child)
	 (unless (or (not new-child) 
		     (eq? this (send new-child area-parent)))
	   (error 'active-child
		  (string-append
		   "The child specified (~s) is not "
		   "a child of this panel (~s)")
		  new-child this))
	 (when active (send active show #f))
	 (when new-child (send new-child show #t))
	 (set! active new-child)
	 (force-redraw)])]
      
      ; only place the active child.
      [do-place-children
       (lambda (children-info width height)
	 (when active
	   (let* ([active-info (send active get-info)]
		  [x-stretch (child-info-x-stretch active-info)]
		  [x-min (child-info-x-min active-info)]
		  [y-stretch (child-info-y-stretch active-info)]
		  [y-min (child-info-y-min active-info)]
		  [x-posn (if x-stretch
			      (border)
			      (/ (- width x-min) 2))]
		  [x-size (if x-stretch
			      (- width (* 2 (border)))
			      x-min)]
		  [y-posn (if y-stretch
			      (border)
			      (/ (- height y-min) 2))]
		  [y-size (if y-stretch
			      (- height (* 2 (border)))
			      y-min)])
	     (list (list x-posn y-posn x-size y-size)))))]
      
      [redraw
       (lambda (width height)
	 (when active
	   (panel-redraw (list active) 
			 (list (send active get-info))
			 (place-children null width height))))])
    (sequence
      (apply super-init args))))

(define wx-panel% (wx-make-panel% wx:panel%))
(define wx-linear-panel% (wx-make-linear-panel% wx-panel%))
(define wx-horizontal-panel% (wx-make-horizontal-panel% wx-linear-panel%))
(define wx-vertical-panel% (wx-make-vertical-panel% wx-linear-panel%))
(define wx-single-panel% (wx-make-single-panel% wx-panel%))

(define wx-pane% (wx-make-pane% wx:windowless-panel%))
(define wx-linear-pane% (wx-make-linear-panel% wx-pane%))
(define wx-horizontal-pane% (wx-make-horizontal-panel% wx-linear-pane%))
(define wx-vertical-pane% (wx-make-vertical-panel% wx-linear-pane%))
(define wx-single-pane% (wx-make-single-panel% wx-pane%))

;-------------------- Text control simulation -------------------------

(define wx-text-media-edit% 
  (class media-edit% (cb return-cb control)
      (rename [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-on-char on-char])
      (inherit get-text last-position)
      (private
	[block-callback 1]
	[callback
	 (lambda (type str?)
	   (when (zero? block-callback)
	     (let ([str (if str? (get-text 0 (last-position)) #f)]
		   [e (make-object wx:command-event% type)])
	       (send e set-event-object control)
	       (when str
		 (send e set-command-string str))
	       (cb control e))))])
      (public
	[on-char
	 (lambda (e)
	   (let ([c (send e get-key-code)])
	     (unless (and (or (eq? c #\return) (eq? c #\newline))
			  return-cb
			  (return-cb (lambda () (callback 'text-enter #t))))
	       (super-on-char e))))]
	[after-insert
	 (lambda args
	   (apply super-after-insert args)
	   (callback 'text #t))]
	[after-delete
	 (lambda args
	   (apply super-after-delete args)
	   (callback 'text #t))]
	[callback-ready
	 (lambda () 
	   (set! block-callback 0))]
	[without-callback
	 (lambda (thunk)
	   (dynamic-wind
	    (lambda () (set! block-callback (add1 block-callback)))
	    thunk
	    (lambda () (set! block-callback (sub1 block-callback)))))])
      (sequence
	(super-init))))
  
(define wx-text-media-canvas% 
  (class wx-media-canvas% (mred proxy control parent style)
    (rename [super-on-char on-char])
    (public
      [on-char (lambda (e) (send control on-char e))]
      [continue-on-char (lambda (e) (super-on-char e))])
    (sequence
      (super-init mred proxy parent -1 -1 100 20 #f style 100 #f))))
  
(define (make-wx-text% multi?)
  (class wx-horizontal-panel% (mred proxy parent func label value style)
    (inherit alignment stretchable-in-y get-button-font)
    (rename [super-place-children place-children])
    (sequence
      (super-init #f proxy parent null))
    (private
      [horiz? (eq? (send parent get-label-position) 'horizontal)]
      [p (if horiz?
	     this
	     (make-object wx-vertical-pane% #f proxy this null))]
      [l (and label
	      (make-object wx-message% #f proxy p label -1 -1 null))]
      [c (make-object wx-text-media-canvas% #f proxy this p
		      (if multi?
			  (if (memq 'hscroll style)
			      null
			      '(hide-h-scroll))
			  '(hide-v-scroll hide-h-scroll)))]
      [e (make-object wx-text-media-edit%
		      func
		      (lambda (do-cb)
			(if multi?
			    #f
			    (do-cb)))
		      this)]
      [dy 0])
    (public
      [get-edit (lambda () e)]
      
      [get-value (lambda () (send e get-text))]
      [set-value (lambda (v) (send e without-callback
				   (lambda () (send e insert v 0 (send e last-position)))))]

      ;; wx:text% and wx:multi-text%
      [on-char (lambda (ev) (send c continue-on-char ev))]
      
      [set-label (lambda (str) (send l set-label str))]
      [get-label (lambda () (send l get-label))]

      [set-cursor (lambda (c) (send e set-cursor c #t))]
	
      [place-children
       (lambda (children-info width height)
	 (let ([r (super-place-children children-info width height)])
	   (if horiz?
	       ;; Line up label right with text:
	       (if (null? r)
		   r
		   (cons (list* (caar r) (+ (cadar r) dy) (cddar r))
			 (cdr r)))
	       r)))])
    (sequence
      (alignment 'left 'top)
      (unless horiz? (send p alignment 'left 'top))
      (unless multi? (stretchable-in-y #f))
      (send e auto-wrap multi?)
      (let ([f (get-button-font)]
	    [s (send (send e get-style-list) find-named-style "Standard")]
	    [d (make-object wx:style-delta%)])
	(send d set-delta-face (send f get-face))
	(send d set-delta 'change-size (send f get-point-size))
	(send d set-delta 'change-style (send f get-style))
	(send d set-delta 'change-weight (send f get-weight))
	(send s set-delta d))
      (send c set-media e)
      (send c set-line-count (if multi? 3 1))

      (when (and l horiz?)
	;; Find amount to drop label down to line up the baselines:
	(let ([wbox (box 0)]
	      [hbox (box 0)]
	      [ybox (box 0)]
	      [abox (box 0)])
	  ; To bottom of first line
	  (send (send e get-admin) get-dc #f ybox)
	  (set! dy (+ -3 (abs (unbox ybox)) (send e line-location 0 #f))) ; 3 is fudge factor
	    
	  ; Add diff for client size
	  (send c get-client-size wbox hbox)
	  (let ([d (- (send c get-height) (unbox hbox))])
	    (set! dy (+ dy (quotient d 2))))
	  
	  ; Subtract descent of canvas-drawn text
	  (let ([font (send (send (send e get-style-list) find-named-style "Standard") get-font)])
	    (send c get-text-extent "hi" wbox hbox ybox #f font)
	    (set! dy (- dy (unbox ybox))))
	  
	  ; Subtract ascent of label
	  (send l get-text-extent "hi" wbox hbox ybox abox)
	  (set! dy (- dy (- (unbox hbox) (unbox ybox))))
	  
	  ; Subtract space above label
	  (set! dy (- dy (quotient (- (send l get-height) (unbox hbox)) 2)))))
      
      (when value
	(set-value value)
	(unless (string=? value "")
	  (let* ([ew (box 0)]
		 [cw (box 0)]
		 [tw (box 0)])
	    (send e get-extent ew #f)
	    (send (send e get-admin) get-view #f #f cw #f)
	    (send c get-size tw (box 0))
	    (let ([new-min-width (+ (unbox ew) (- (unbox tw) (unbox cw)))])
	      (send c set-min-width new-min-width)))))
      (send e callback-ready))))

(define wx-text% (make-wx-text% #f))
(define wx-multi-text% (make-wx-text% #t))

;;;;;;;;;;;;;;;;;;;;;;;;; mred Class Construction ;;;;;;;;;;;;;;;;;;;;;;;;;

;------------ More helpers ---------------

(define wx-get-mred (make-generic wx<%> get-mred))
(define wx-get-proxy (make-generic wx/proxy<%> get-proxy))

(define (wx->mred w) ((wx-get-mred w)))
(define (wx->proxy w) ((wx-get-proxy w)))

(define (param get-obj method)
  (case-lambda
   [() ((ivar/proc (get-obj) method))]
   [(v) ((ivar/proc (get-obj) method) v)]))

(define (check-container-parent who p)
  (unless (is-a? p internal-container<%>)
    (raise-type-error (string->symbol (format "~a-constructor" who))
		      "built-in container<%> object" p)))

(define (check-orientation who l)
  (unless (and (list? l) (andmap symbol? l) 
	       (or (memq 'horizontal l) (memq 'vertical l))
	       (not (and (memq 'horizontal l) (memq 'vertical l))))
    (error (string->symbol (format "~a-constructor" who))
	   (cond
	    [(not (and (list? l) (andmap symbol? l))) "style specification is not a list of symbols: ~e"]
	    [(or (memq 'horizontal l) (memq 'vertical l)) "style specification includes both orientations: ~e"]
	    [else "style specification does not include an orientation: ~e"])
	   l)))

(define double-boxed
  (lambda (x y f)
    (let ([x (box x)][y (box y)])
      (f x y)
      (values (unbox x) (unbox y)))))

(define mred%
  (class null (wx)
    (public
      [get-low-level-window (lambda (key)
			      (unless (eq? key wx-key)
				(error 'get-low-level-window "bad key"))
			      wx)])))

(define (wrap-callback cb)
  (if (and (procedure? cb)
	     (procedure-arity-includes? cb 2))
      (lambda (w e) (cb (wx->mred w) e))
      cb))

(define mred-get-low-level-window (make-generic mred% get-low-level-window))
(define wx-key (gensym))
(define (mred->wx w) ((mred-get-low-level-window w) wx-key))

(define (mred->wx-container w) (send (mred->wx w) get-container))

;---------------- Window interfaces and base classes ------------

(define area<%>
  (interface ()
    get-parent
    min-width min-height
    stretchable-width stretchable-height))

(define area%
  (class* mred% (area<%>) (mk-wx get-wx-panel parent)
    (public
      [get-parent (lambda () parent)]
      [min-width (param get-wx-panel 'min-width)]
      [min-height (param get-wx-panel 'min-height)]
      [stretchable-width (param get-wx-panel 'stretchable-in-x)]
      [stretchable-height (param get-wx-panel 'stretchable-in-y)])
    (private
      [wx (mk-wx)])
    (sequence (super-init wx))))

(define subarea<%> 
  (interface (area<%>)
    horiz-margin vert-margin))

(define (make-subarea% %) ; % implements area<%>
  (class* % (subarea<%>) (mk-wx get-wx-panel parent)
    (public
      [horiz-margin (param get-wx-panel 'x-margin)]
      [vert-margin (param get-wx-panel 'y-margin)])
    (sequence (super-init mk-wx get-wx-panel parent))))

(define container<%> 
  (interface (area<%>) 
    get-children change-children place-children
    add-child delete-child
    border))

(define internal-container<%> (interface ()))

(define (make-container% %) ; % implements area<%>
  (class* % (container<%> internal-container<%>) (mk-wx get-wx-panel parent) 
    (public
      [get-children (lambda () (map wx->mred (ivar (get-wx-panel) children)))]
      [border (param get-wx-panel 'border)]
      [change-children (lambda (f)
			 (map mred->wx
			      (send (get-wx-panel) change-children
				    (lambda (kids)
				      (f (map wx->mred kids))))))]
      [place-children (lambda (l w h) (send (get-wx-panel) do-place-children l w h))]
      [add-child (lambda (c) (send (get-wx-panel) add-child (mred->wx c)))]
      [delete-child (lambda (c) (send (get-wx-panel) delete-child (mred->wx c)))])
    (sequence
      (super-init mk-wx get-wx-panel parent))))

(define linear-container<%>
  (interface (container<%>)
    spacing 
    set-alignment))

(define (make-linear-container% %) ; % implements container<%>
  (class* % (linear-container<%>) (mk-wx get-wx-panel parent)
    (public
      [spacing (param get-wx-panel 'spacing)]
      [set-alignment (lambda (h v) (send (get-wx-panel) alignment h v))]
      [get-alignment (lambda () (send (get-wx-panel) get-alignment))])
    (sequence
      (super-init mk-wx get-wx-panel parent))))


(define window<%>
  (interface (area<%>)
    on-focus focus
    on-size
    pre-on-char pre-on-event
    client->screen screen->client
    enable is-enabled?
    get-label set-label
    get-client-size get-geometry get-width get-height get-x get-y
    get-text-extent
    get-cursor set-cursor 
    show is-shown?
    refresh))

(define (make-window% %) ; % implements area<%>
  (class* % (window<%>) (mk-wx get-wx-panel label parent cursor)
    (public
      [on-focus void]
      [on-size void]
      [pre-on-char (lambda (w e) #f)]
      [pre-on-event (lambda (w e) #f)]

      [focus (lambda () (send wx set-focus))]
      [has-focus? (lambda () (send wx has-focus?))]
      [enable (lambda (on?) (send wx enable on?))]
      [is-enabled? (lambda () (send wx is-enabled?))]
      
      [get-label (lambda () label)]
      [set-label (lambda (l) (set! label l))]
      
      [client->screen (lambda (x y)
			(double-boxed
			 x y
			 (lambda (x y) (send wx client-to-screen x y))))]
      [screen->client (lambda (x y)
			(double-boxed
			 x y
			 (lambda (x y) (send wx screen-to-client x y))))]
      [get-client-size (lambda ()
			 (double-boxed
			  0 0
			  (lambda (x y) (send wx get-client-size x y))))]
      [get-geometry (lambda ()
		      (let ([x (box 0)][y (box 0)][w (box 0)][h (box 0)])
			(send wx get-size w h x y)
			(values (unbox x) (unbox y) (unbox w) (unbox h))))]
      
      [get-width (lambda () (send wx get-width))]
      [get-height (lambda () (send wx get-height))]
      [get-x (lambda () (send wx get-x))]
      [get-y (lambda () (send wx get-y))]

      [get-text-extent (letrec ([l (case-lambda 
				    [(s w h) (l s w h #f #f #f)]
				    [(s w h d) (l s w h d #f #f)]
				    [(s w h d a) (l s w h d a #f)]
				    [(s w h d a f) (send wx get-text-extent s w h d a f)])])
			 l)]
      
      [get-cursor (lambda () cursor)]
      [set-cursor (lambda (x)
		    (send wx set-cursor x)
		    (set! cursor x))]

      [show (lambda (on?) (send wx show on?))]
      [is-shown? (lambda () (send wx is-shown?))]

      [refresh (lambda () (send wx refresh))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () (set! wx (mk-wx)) wx) get-wx-panel parent))))

(define area-container-window<%>
  (interface (window<%> container<%>)
    set-control-font get-control-font
    set-label-font get-label-font
    set-label-position get-label-position))

(define (make-area-container-window% %) ; % implements window<%> (and container<%>)
  (class* % (area-container-window<%>) (mk-wx get-wx-panel label parent cursor) 
    (public
      [get-control-font (lambda () (send (get-wx-panel) get-button-font))]
      [set-control-font (lambda (x) (send (get-wx-panel) set-button-font x))]
      [get-label-font (lambda () (send (get-wx-panel) get-label-font))]
      [set-label-font (lambda (x) (send (get-wx-panel) set-label-font x))]
      [get-label-position (lambda () (send (get-wx-panel) get-label-position))]
      [set-label-position (lambda (x) (send (get-wx-panel) set-label-position x))])
    (sequence
      (super-init mk-wx get-wx-panel label parent cursor))))

(define top-level-window<%>
  (interface (linear-container<%> area-container-window<%>)
    on-activate
    get-focus-window get-edit-target-window
    get-focus-object get-edit-target-object
    center move resize))

(define basic-top-level-window%
  (class* (make-area-container-window% (make-window% (make-linear-container% (make-container% area%)))) (top-level-window<%>) (mk-wx label parent)
    (rename [super-set-label set-label])
    (private
      [wx-object->mred
       (lambda (o)
	 (or (and (is-a? o wx:window%))
	     (wx->mred o)
	     o))])
    (public
      [on-activate void]
      [center (case-lambda
	       [() (send wx center)]
	       [(dir) (send wx center dir)])]
      [set-label (lambda (l)
		   (send wx set-title l)
		   (super-set-label))]
      [move (lambda (x y)
	      (send wx move x y))]
      [resize (lambda (w h)
		(send wx set-size -1 -1 w h))]

      [get-focus-window (lambda () (let ([w (send wx get-focus-window)])
				     (and w (wx->mred w))))]
      [get-edit-target-window (lambda () (let ([w (send wx get-edit-target-window)])
					   (and w (wx->mred w))))]
      [get-focus-object (lambda () (let ([o (send wx get-focus-object)])
				     (and o (wx-object->mred o))))]
      [get-edit-target-object (lambda () (let ([o (send wx get-edit-target-object)])
					   (and o (wx-object->mred o))))])
    (private
      [wx #f]
      [wx-panel #f]
      [finish (lambda (top-level)
		(set! wx-panel (make-object wx-vertical-panel% #f this top-level null))
		(send top-level set-container wx-panel)
		top-level)])
    (sequence (super-init (lambda () (set! wx (mk-wx finish)) wx) (lambda () wx-panel) label parent #f))))

(define subwindow<%> 
  (interface (window<%> subarea<%>)))

(define control<%>
  (interface (subwindow<%>)
    command))

(define basic-control%
  (class* (make-window% (make-subarea% area%)) (control<%>) (mk-wx label parent cursor)
    (rename [super-set-label set-label])
    (public
      [set-label (lambda (l)
		   (send wx set-label l)
		   (super-set-label l))]
      [command (lambda (e) (send wx command e))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () (set! wx (mk-wx)) wx) (lambda () wx) label parent cursor))))

;--------------------- Final mred class construction --------------------
    
(define frame%
  (class basic-top-level-window% (label [parent #f] [x #f] [y #f] [width #f] [height #f] [style null])
    (private
      [wx #f])
    (public
      [create-status-line (lambda () (send wx create-status-line))]
      [set-status-line (lambda () (send wx create-status-line))]
      [get-menu-bar (lambda () (let ([mb (ivar wx menu-bar)])
				 (and mb (wx->mred mb))))])
    (sequence
      (super-init (lambda (finish) 
		    (set! wx (finish (make-object wx-frame% this this
						  (and parent (mred->wx parent)) label 
						  (or x -1) (or y -1) (or width -1) (or height -1)
						  style)))
		    wx)
		  label parent))))

(define dialog-box%
  (class basic-top-level-window% (label [modal? #t] [parent #f] [x #f] [y #f] [width #f] [height #f] [style null])
    (sequence
      (super-init (lambda (finish) (finish (make-object wx-dialog-box% this this
							(and parent (mred->wx parent)) label modal?
							(or x -1) (or y -1) (or width -1) (or height -1)
							style)))
		  label parent))))

(define message%
  (class basic-control% (label parent [style null])
    (sequence
      (check-container-parent 'message parent)
      (super-init (lambda () (make-object wx-message% this this
					  (mred->wx-container parent)
					  label -1 -1 style))
		  label parent #f))))

(define button%
  (class basic-control% (label parent callback [style null])
    (sequence
      (check-container-parent 'button parent)
      (super-init (lambda () (make-object wx-button% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label -1 -1 -1 -1 style))
		  label parent #f))))

(define check-box%
  (class basic-control% (label parent callback [style null])
    (sequence (check-container-parent 'check-box parent))
    (private
      [wx #f])
    (public
      [get-value (lambda () (send wx get-value))]
      [set-value (lambda (v) (send wx set-value v))])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-check-box% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label -1 -1 -1 -1 style))
		    wx)
		  label parent #f))))

(define radio-box%
  (class basic-control% (label choices parent callback [style '(vertical)])
    (sequence (check-container-parent 'radio-box parent) (check-orientation 'radio-box style))
    (private
      [wx #f])
    (public
      [enable (case-lambda
	       [(on?) (send wx enable on?)]
	       [(which on?) (send wx enable which on?)])]
      [is-enabled? (case-lambda
		    [() (send wx is-enabled?)]
		    [(which) (send wx is-enabled? which)])]
      [get-number (lambda () (length choices))]

      [get-item-label (lambda (n) 
			(if (>= n (get-number))
			    #f
			    (list-ref choices n)))]
       
      [get-selection (lambda () (send wx get-selection))]
      [set-selection (lambda (v) (send wx set-selection v))])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-radio-box% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label -1 -1 -1 -1 choices 0 style))
		    wx)
		  label parent #f))))

(define slider%
  (class basic-control% (label min-val max-val parent callback [value min-val] [style '(horizontal)])
    (sequence (check-container-parent 'slider parent) (check-orientation 'slider style))
    (private
      [wx #f])
    (public
      [get-value (lambda () (send wx get-value))]
      [set-value (lambda (v) (send wx set-value v))])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-slider% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label value min-val max-val style))
		    wx)
		  label parent #f))))

(define gauge%
  (class basic-control% (label parent range [style '(horizontal)])
    (sequence (check-container-parent 'gauge parent) (check-orientation 'gauge style))
    (private
      [wx #f])
    (public
      [get-value (lambda () (send wx get-value))]
      [set-value (lambda (v) (send wx set-value v))])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-gauge% this this
					  (mred->wx-container parent)
					  label range style))
		    wx)
		  label parent #f))))

(define list-control<%>
  (interface (control<%>)
    clear append
    get-number
    get-string find-string
    get-selection
    get-string-selection
    set-selection
    set-string-selection))

(define basic-list-control%
  (class* basic-control% (list-control<%>) (mk-wx label parent)
    (public
      [append (lambda (i) (send wx append i))]
      [clear (lambda () (send wx clear))]
      [get-number (lambda () (send wx number))]
      [get-string (lambda (n) (send wx get-string n))]
      [get-selection (lambda () (send wx get-selection))]
      [get-string-selection (lambda () (send wx get-string-selection))]
      [set-selection (lambda (s) (send wx set-selection s))]
      [set-string-selection (lambda (s) (send wx set-string-selection s))]
      [find-string (lambda (x) (send wx find-string x))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () (set! wx (mk-wx)) wx) label parent #f))))

(define choice%
  (class basic-list-control% (label choices parent callback [style null])
    (sequence
      (check-container-parent 'choice parent)
      (super-init (lambda () (make-object wx-choice% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label -1 -1 -1 -1 choices style))
		  label parent))))

(define list-box%
  (class basic-list-control% (label choices parent callback [style '(single)])
    (sequence 
      (check-container-parent 'list-box parent)
      (let ([c (+ (if (memq 'single style) 1 0)
		  (if (memq 'multiple style) 1 0)
		  (if (memq 'extended style) 1 0))])
	(when (zero? c)
	  (error 'list-box-constructor "style does not specify single, multiple, or extended: ~a" style))
	(when (> c 1)
	  (error 'list-box-constructor "style specifies more than one of single, multiple, or extended: ~a" style))))
    (rename [super-append append])
    (public
      [append (case-lambda
	       [(i) (super-append i)]
	       [(i d) (send wx append i d)])]
      [delete (lambda (n) (send wx delete n))]
      [get-data (lambda (n) (send wx get-data n))]
      [get-selections (lambda () (send wx get-selections))]
      [number-of-visible-items (lambda () (send wx number-of-visible-items))]
      [is-selected? (lambda (n) (send wx selected? n))]
      [set (lambda (l) (send wx set l))]
      [set-string (lambda (n d) (send wx set-string n d))]
      [set-data (lambda (n d) (send wx set-data n d))]
      [get-first-visible (lambda () (send wx get-first-item))]
      [set-first-visible (lambda () (send wx set-first-item))]
      [select (case-lambda 
	       [(n) (send wx set-selection n)]
	       [(n on?) (send wx set-selection n on?)])])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () 
		    (let-values ([(kind style)
				  (cond
				   [(memq 'single style) (values 'single (remq 'single style))]
				   [(memq 'multiple style) (values 'multiple (remq 'multiple style))]
				   [else (values 'extended (remq 'extended style))])])
		      (set! wx (make-object wx-list-box% this this
					    (mred->wx-container parent) (wrap-callback callback)
					    label kind
					    -1 -1 -1 -1 choices style)))
		    wx)
		  label parent))))

(define text-control<%>
  (interface (control<%>)
    get-edit get-value set-value))

(define (make-text% wx-text% who)
  (class* basic-control% (text-control<%>) (label parent callback [init-val ""] [style null])
    (sequence (check-container-parent who parent))
    (private
      [wx #f])
    (public
      [get-edit (lambda () (send wx get-edit))]
      [get-value (lambda () (send wx get-value))]
      [set-value (lambda (v) (send wx set-value v))])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-text% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label init-val style))
		    wx)
		  label parent ibeam))))

(define text% (make-text% wx-text% 'text))
(define multi-text% (make-text% wx-multi-text% 'multi-text))

;-------------------- Canvas class constructions --------------------

(define canvas-default-size 20) ; an arbitrary default size for canvases to avoid initial size problems

(define canvas<%>
  (interface (subwindow<%>)
    on-char on-event on-paint on-scroll
    popup-menu warp-pointer get-dc))

(define basic-canvas%
  (class* (make-window% (make-subarea% area%)) (canvas<%>) (mk-wx parent)
    (public
      [on-char (lambda (e) (send wx do-on-char e))]
      [on-event (lambda (e) (send wx do-on-event e))]
      [on-paint (lambda () (send wx do-on-paint))]
      [on-scroll (lambda (e) (send wx do-on-scroll e))]
      
      [popup-menu (lambda (m x y) (send wx popup-menu (mred->wx m) x y))]
      [warp-pointer (lambda (x y) (send wx warp-pointer x y))]

      [get-dc (lambda () (send wx get-dc))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () (set! wx (mk-wx)) wx) (lambda () wx) #f parent #f))))

(define canvas%
  (class basic-canvas% (parent [style null])
    (sequence (check-container-parent 'canvas parent))
    (public
      [virtual-size (lambda () (double-boxed
				0 0
				(lambda (x y) (send wx get-virtual-size))))]
      [view-start (lambda () (double-boxed
			      0 0
			      (lambda (x y) (send wx get-view-start))))]

      [scroll (lambda (x y) (send wx scroll x y))]

      [set-scrollbars (lambda (h-pixels v-pixels x-len y-len x-page y-page x-val y-val man?)
			(send wx set-scrollbars 
			      h-pixels v-pixels x-len y-len x-page y-page x-val y-val man?))]

      [get-scroll-pos (lambda () (send wx get-scroll-pos))]
      [set-scroll-pos (lambda (v) (send wx set-scroll-pos v))]
      [get-scroll-range (lambda () (send wx get-scroll-range))]
      [set-scroll-range (lambda (v) (send wx set-scroll-range v))]
      [get-scroll-page (lambda () (send wx get-scroll-page))]
      [set-scroll-page (lambda (v) (send wx set-scroll-page v))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-canvas% this this
					  (mred->wx-container parent)
					  -1 -1 canvas-default-size canvas-default-size
					  style))
		    wx)
		  parent))))
    
(define media-canvas%
  (class basic-canvas% (parent [buffer #f] [style null] [scrolls-per-page 100])
    (sequence (check-container-parent 'canvas parent))
    (public
      [call-as-primary-owner (lambda (f) (send wx call-as-primary-owner f))]
      [allow-scroll-to-last (lambda (on?) (send wx allow-scroll-to-last on?))]
      [scroll-with-bottom-base (lambda (on?) (send wx scroll-with-bottom-base on?))]
      
      [has-lazy-refresh? (lambda () (send wx get-lazy-refresh))]
      [lazy-refresh (lambda (on?) (send wx set-lazy-referesh))]
      
      [force-display-focus (lambda (on?) (send wx force-display-focus on?))]

      [edit-target (lambda (on?) (send x set-edit-target (and on? wx)))]
      [is-edit-target? (lambda () (and #t (send x get-edit-target)))]

      [get-media (lambda () (send wx get-media))]
      [set-media (lambda (m) (send wx set-media m))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-media-canvas% this this
					  (mred->wx-container parent) -1 -1 canvas-default-size canvas-default-size
					  #f style scrolls-per-page buffer))
		    wx)
		  parent))))

;-------------------- Final panel interfaces and class constructions --------------------

(define (make-pane% who pane% wx-pane%)
  (class pane% (parent)
    (private [wx #f])
    (sequence 
      (check-container-parent who parent)
      (super-init (lambda () (set! wx (make-object wx-pane% this this (mred->wx-container parent) null)) wx)
		  (lambda () wx) parent))))

(define basic-pane% (make-subarea% (make-container% area%)))
(define pane% (make-pane% 'pane basic-pane% wx-pane%))
(define single-pane% (make-pane% 'single-pane basic-pane% wx-single-pane%))

(define basic-linear-pane% (make-subarea% (make-linear-container% (make-container% area%))))
(define vertical-pane% (make-pane% 'vertical-pane basic-linear-pane% wx-vertical-pane%))
(define horizontal-pane% (make-pane% 'horizontal-pane basic-linear-pane% wx-horizontal-pane%))

(define (make-panel% who panel% wx-panel%)
  (class panel% (parent [style null])
    (private [wx #f])
    (sequence 
      (check-container-parent who parent)
      (super-init (lambda () (set! wx (make-object wx-panel% this this (mred->wx-container parent) style)) wx)
		  (lambda () wx) #f parent #f))))


(define basic-panel% (make-area-container-window% (make-window% (make-subarea% (make-container% area%)))))
(define panel% (make-panel% 'panel basic-panel% wx-panel%))
(define single-panel% (make-panel% 'single-panel basic-panel% wx-single-panel%))

(define basic-linear-panel% (make-area-container-window% (make-window% (make-linear-container% (make-subarea% (make-container% area%))))))
(define vertical-panel% (make-panel% 'vertical-panel basic-linear-panel% wx-vertical-panel%))
(define horizontal-panel% (make-panel% 'horizontal-panel basic-linear-panel% wx-horizontal-panel%))

;;;;;;;;;;;;;;;;;;;;;; Menu classes ;;;;;;;;;;;;;;;;;;;;;;

(define (find-pos l i)
  (let loop ([l l][n 0])
    (cond
     [(null? l) n]
     [(eq? (car l) i) n]
     [else (loop (cdr l) (add1 n))])))

(define (menu-parent-only who p)
  (unless (is-a? p internal-menu<%>)
    (raise-type-error (string->symbol (format "~a-constructor" who))
		      "parent menu% or popup-menu% object" p)))

(define (menu-or-bar-parent who p)
  (unless (or (is-a? p internal-menu<%>) (is-a? p menu-bar%))
    (raise-type-error (string->symbol (format "~a-constructor" who))
		      "parent menu%, popup-menu%, or menu-bar% object" p)))

(define (barless-frame-parent p)
  (unless (is-a? p frame%)
    (raise-type-error 'menu-bar-cnostructor "parent frame% object" p))
  (when (send (mred->wx p) get-menu-bar)
    (error 'menu-bar-constructor "the specified frame already has a menu bar")))

(define wx-menu-item%
  (class* wx:menu-item% (wx<%>) (mred)
    (public
      [get-mred (lambda () mred)])
    (sequence
      (super-init))))

(define wx-menu-bar%
  (class* wx:menu-bar% (wx<%>) (mred)
    (inherit delete)
    (rename [super-append append])
    (private
      [items null])
    (public
      [get-mred (lambda () mred)]
      [get-items (lambda () items)]
      [append-item (lambda (item menu title)
		     (super-append menu title)
		     (set! items (append items (list item))))]
      [delete-item (lambda (i)
		     (let ([p (position-of i)])
		       (set! items (remq i items))
		       (delete #f p)))]
      [position-of (lambda (i) (find-pos items i))])
    (sequence
      (super-init null null))))

(define wx-menu%
  (class* wx:menu% (wx<%>) (mred popup-label popup-callback)
    (private
      [items null])
    (inherit delete-by-position)
    (rename [super-delete delete])
    (public
      [get-mred (lambda () mred)]
      [get-items (lambda () items)]
      [append-item (lambda (i) (set! items (append items (list i))))]
      [delete (lambda (id i) (super-delete id) (set! items (remq i items)))]
      [delete-sep (lambda (i) (delete-by-position (find-pos items i)) (set! items (remq i items)))])
    (sequence
      (super-init popup-label popup-callback))))

;; Most of the work is in the item. Anything that appears in a menubar or
;;  menu has an item. Submenus are created as instances of menu%, but
;;  menu% has a get-item method for manipulating the menu w.r.t. the parent
;;  (e.g., changing the title or enabled state). A popup menu, created
;;  as an instance of popup-menu%, has no item.
;;
;; A menu bar is created as a menu-bar%, given a frame as its parent. The
;;  frame must not already have a menu bar.
;;
;;  Plain labeled items are created as instances of menu-item% or
;;   checkable-menu-item%. The parent must be a menu-item-container<%>,
;;   which is a menu%, popup-menu%, or menu-bar%

(define menu-item<%>
  (interface ()
    get-parent
    delete restore is-deleted?))

(define labelled-menu-item<%>
  (interface (menu-item<%>)
    get-label set-label get-plain-label
    get-help-string set-help-string
    enable is-enabled?))

(define submenu-item<%>
  (interface (labelled-menu-item<%>) get-menu))

(define separator-menu-item%
  (class* mred% (menu-item<%>) (parent)
    (sequence (menu-parent-only 'separator-menu-item parent))
    (private
      [wx (make-object wx-menu-item% this)]
      [shown? #f]
      [wx-parent (mred->wx parent)])
    (public
      [get-parent (lambda () parent)]
      [restore (lambda ()
		 (unless shown?
		   (send wx-parent append-separator)
		   (send wx-parent append-item this)
		   (set! shown? #t)))]
      [delete (lambda ()
		(when in-menu?
		  (send wx-parent delete-sep this)
		  (set! shown? #f)))]
      [is-deleted? (lambda () (not in-menu?))])
    (sequence
      (super-init wx)
      (restore))))

(define basic-labelled-menu-item%
  (class* mred% (labelled-menu-item<%>) (parent label help-string submenu checkable? set-wx)
    (private
      [wx (set-wx (make-object wx-menu-item% this))]
      [wx-parent (mred->wx parent)]
      [plain-label (wx:strip-menu-codes label)]
      [in-menu? (is-a? parent basic-menu%)]
      [shown? #f]
      [enabled? #t]
      [do-enable (lambda (on?)
		   (if in-menu?
		       (send wx-parent enable (send wx id) on?)
		       (send wx-parent enable-top (send wx-parent position-of this) on?))
		   (set! enabled? (and on? #t)))])
    (public
      [get-parent (lambda () parent)]
      [get-label (lambda () label)]
      [set-label (lambda (l)
		   (set! label l)
		   (set! plain-label (wx:strip-menu-codes l))
		   (when shown?
		     (if in-menu?
			 (send wx-parent set-label (send wx id) label)
			 (send wx-parent set-label-top (send wx-parent position-of this) plain-label))))]
      [get-plain-label (lambda () plain-label)]
      [get-help-string (lambda () help-string)]
      [set-help-string (lambda (s) (set! help-string s)
			       (send wx-parent set-help-string (send wx id) s))]
      [enable (lambda (on?) (do-enable on?))]
      [is-enabled? (lambda () enabled?)]
      [restore (lambda ()
		 (unless shown?
		   (if in-menu?
		       (begin
			 (if submenu
			     (send wx-parent append (send wx id) plain-label (mred->wx submenu) help-string)
			     (send wx-parent append (send wx id) label help-string checkable?))
			 (send wx-parent append-item this))
		       (send wx-parent append-item this (mred->wx submenu) plain-label))
		   (set! shown? #t)
		   (do-enable enabled?)))]
      [delete (lambda ()
		(when shown?
		  (if in-menu?
		      (send wx-parent delete (send wx id) this)
		      (send (mred->wx parent) delete-item this))
		  (set! shown? #f)))]
      [is-deleted? (lambda () (not shown?))])
    (sequence
      (super-init wx)
      (restore))))

(define basic-label-menu-item%
  (class basic-labelled-menu-item% (label checkable? menu callback shortcut help-string set-wx)
    (private
      [wx #f])
    (public
      [go (lambda () (callback this (make-object wx:control-event% 'menu)))])
    (sequence
      (let ([new-label (if shortcut
			   (string-append
			    label
			    (case (system-type)
			      [(unix) (format "~aCtl+m ~a" #\tab (char-downcase shortcut))]
			      [(windows) (format "~aCtl+~a" #\tab (char-upcase shortcut))]
			      [(macos) (format "~aCmd-~a" #\tab (char-upcase shortcut))]))
			   label)]
	    [key-binding (and shortcut
			      (case (system-type)
				[(unix) (format "c:m;~a" (char-downcase shortcut))]
				[(windows) (format "c:~a" (char-downcase shortcut))]
				[(macos) (format "d:~a" (char-downcase shortcut))]))])
	(super-init menu new-label help-string #f checkable? (lambda (x) (set! wx x) (set-wx x)))))))

(define menu-item%
  (class basic-label-menu-item% (label menu callback [shortcut #f] [help-string #f])
    (sequence 
      (menu-parent-only 'menu-item menu)
      (super-init label #f menu callback shortcut help-string (lambda (x) x)))))

(define checkable-menu-item%
  (class basic-label-menu-item% (label menu callback [shortcut #f] [help-string #f])
    (sequence (menu-parent-only 'checkable-menu-item menu))
    (private
      [wx #f])
    (public
      [check (lambda (on?) (send (mred->wx menu) check (send wx id) on?))]
      [is-checked? (lambda () (send (mred->wx menu) checked? (send wx id)))])
    (sequence
      (super-init label #t menu callback shortcut help-string (lambda (x) (set! wx x) x)))))

(define sub-menu-item%
  ; >> Not for export <<
  (class* basic-labelled-menu-item% (submenu-item<%>) (menu label parent help-string)
    (public
      [get-menu (lambda () menu)])
    (sequence
      (super-init parent label help-string menu #f (lambda (x) x)))))

(define menu-item-container<%> (interface () get-items))
(define internal-menu<%> (interface ()))

(define basic-menu%
  (class* mred% (menu-item-container<%> internal-menu<%>) (popup-label callback)
    (public
      [get-items (lambda () (send wx get-items))])
    (private
      [wx (make-object wx-menu% this popup-label callback)])
    (sequence (super-init wx))))

(define menu%
  (class basic-menu% (label parent [help-string #f])
    (sequence 
      (menu-or-bar-parent 'menu parent)
      (super-init #f void))
    (private
      [item (make-object sub-menu-item% this label parent help-string)])
    (public
      [get-item (lambda () item)])))

(define popup-menu%
  (class basic-menu% (title)
    (sequence
      (super-init title
		  (lambda (m e)
		    (let ([wx (wx:id-to-menu-item (send e get-menu-id))])
		      (send (wx->mred wx) go)))))))

(define menu-bar%
  (class* mred% (menu-item-container<%>) (parent)
    (sequence (barless-frame-parent parent))
    (private 
      [wx (make-object wx-menu-bar% this)]
      [wx-parent (mred->wx parent)]
      [shown? #f])
    (public
      [get-items (lambda () (send wx get-items))]
      [enable (lambda (on?) (send wx enable-all on?))]
      [is-enabled? (lambda () (send wx all-enabled?))]
      [show (lambda (on?) 
	      (set! shown? (and on? #t))
	      (send wx-parent set-menu-bar (and on? wx)))]
      [is-shown? (lambda () shown?)])
    (sequence
      (super-init wx)
      (show #t))))
