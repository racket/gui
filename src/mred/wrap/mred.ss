
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
(define-struct child-info (x-min y-min           ; includes margins!
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
  (unless (<= 0 v max-min)
    (error (who->name who) "not a reasaonable minimum width: ~a" v)))

(define (check-reasonable-margin who v)
  (unless (<= 0 v max-margin)
    (error (who->name who) "not a reasaonable margin size: ~a" v)))

(define (range-error who v hard-min-width max-min)
  (error (who->name who) "value out-of-range: ~a not in: ~a to ~a"
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

(define top-x 1)
(define top-y 1)

;;;;;;;;;;;;;;; wx- Class Construction ;;;;;;;;;;;;;;;;;;;;

; ------------- Mixins for common functionality --------------


(define wx-make-window%
  (lambda (%)
    (class % args
      (rename [super-on-set-focus on-set-focus]
	      [super-on-kill-focus on-kill-focus]
	      [super-drag-accept-files drag-accept-files])
      (private
	[top-level #f]
	[focus? #f]
	[container this])
      (public
	[accept-drag? #f]
	[get-container (lambda () container)]
	[set-container (lambda (c) (set! container c))]
	[get-window (lambda () this)]
	[dx (lambda () 0)]
	[dy (lambda () 0)]
	[get-top-level
	 (lambda ()
	   (unless top-level
	     (let loop ([window this])
	       (cond
		[(or (is-a? window wx:frame%)
		     (is-a? window wx:dialog%)) 
		 (set! top-level window)]
		[else (loop (send window get-parent))])))
	   top-level)])
      (override
	[drag-accept-files
	 (lambda (on?)
	   (set! accept-drag? (and on? #t))
	   (super-drag-accept-files on?))]
	[on-set-focus
	 (lambda ()
	   (send (get-top-level) set-focus-window this)
	   (set! focus? #t)
	   (super-on-set-focus))]
	[on-kill-focus
	 (lambda ()
	   (send (get-top-level) set-focus-window #f)
	   (set! focus? #f)
	   (super-on-kill-focus))])
      (public
	[has-focus? (lambda () focus?)])
      (sequence (apply super-init args)))))

; make-container% - for panels and top-level windows
(define (wx-make-container% %) %)

; make-top-container%: adds the necessary functionality to wx:frame% and 
; wx:dialog%.
; input: base%: the base class from which to descend the new class.
;          Intended to be either wx:frame% or wx:dialog%, but can
;          be anything which contains all methods in the inherit section
;          below.
; returns: a new class, descended from base%, which possesses the added
;            capabilities necessary to serve as the frame/dialog which
;            contains container classes.
(define (make-top-container% base%)
  (class (wx-make-container% (wx-make-window% base%)) args
    (inherit get-x get-y get-width get-height
	     get-client-size is-shown?)
    (rename [super-show show] [super-move move] [super-center center]
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

      [use-default-position? #t]
      
      [enabled? #f]
      [focus #f]
      [target #f])
    
    (override
      [enable
       (lambda (b)
	 (set! enabled? (and b #t))
	 (super-enable b))])
    (public
      [eventspace (wx:current-eventspace)]

      [is-enabled?
       (lambda () enabled?)]

      [set-focus-window
       (lambda (w)
	 (set! focus w)
	 (when w
	   (set! target w)))]
      
      [get-focus-window
       (lambda () focus)]
      [get-edit-target-window
       (lambda () (and target (send (wx->proxy target) is-shown?) target))]
      [get-focus-object
       (lambda ()
	 (window->focus-object focus))]
      [get-edit-target-object
       (lambda ()
	 (window->focus-object target))]

      [window->focus-object
       (lambda (w)
	 (and w
	      (if (is-a? w wx:editor-canvas%)
		  (let loop ([m (send w get-editor)]
			     [prev w])
		    (if m
			(let ([snip (send m get-focus-snip)])
			  (if (and snip (is-a? snip wx:editor-snip%))
			      (loop (send snip get-editor) m)
			      m))
			w))
		  focus)))]

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

      [area-parent (lambda () #f)]
      
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
		 (values new-w new-h)))))])
      
    (override
      ; show: add capability to set perform-updates
      ; input: now : boolean
      ; returns: nothing
      ; effects: if we're showing for the first time, unblock updates
      ;            and force an update.  If we're hiding, block updates.
      ;          pass now to superclass's show.
      [show
       (lambda (on?)
	 (when (and on? pending-redraws?)
	   (force-redraw))
	 (when (and on? use-default-position?)
	   (set! use-default-position? #f)
	   (let*-values ([(w) (get-width)]
			 [(h) (get-height)]
			 [(sw sh) (get-display-size)]
			 [(x x-reset?) (if (< (+ top-x w) sw)
					   (values top-x #f)
					   (values (max 0 (- sw w 10)) #t))]
			 [(y y-reset?) (if (< (+ top-y h) sh)
					   (values top-y #f)
					   (values (max 0 (- sh h 20)) #t))])
	     (move x y)
	     (set! top-x (if x-reset? 0 (+ top-x 10)))
	     (set! top-y (if y-reset? 0 (+ top-y 20)))))
	 (super-show on?))]
      
      [move (lambda (x y) (set! use-default-position? #f) (super-move x y))]
      [center (lambda (dir)
		(when pending-redraws? (force-redraw))
		(set! use-default-position? #f)
		(super-center dir))]
      
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
       (lambda (new-width new-height)
	 (super-on-size new-width new-height)
	 (unless already-trying?
	   (let ([new-width (get-width)]
		 [new-height (get-height)])
	     (let-values ([(correct-w correct-h)
			   (correct-size new-width new-height)])
	       (unless (and (= new-width correct-w)
			    (= new-height correct-h))
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
   (lambda (item% x-margin-w y-margin-h stretch-x stretch-y)
     (class (wx-make-window% item%) args
       (rename [super-on-set-focus on-set-focus]
	       [super-on-kill-focus on-kill-focus])
       (inherit get-width get-height get-x get-y
		get-parent get-client-size)
       (rename [super-enable enable]
	       [super-set-size set-size])
       (private [enabled? #t])
       (override
	 [enable
	  (lambda (b)
	    (set! enabled? (and b #t))
	    (super-enable b))]

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
	      (super-set-size x y width height)))])

       (public
	 [orig-enable
	  (lambda args (apply super-enable args))]
	 [is-enabled?
	  (lambda () enabled?)])

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
	   [() (- (min-height) (client-inset #t))]
	   [(new-height) (min-height (+ new-height (client-inset #t)))])]

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
		   [result (make-child-info (car min-size) (cadr min-size)
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
	 
	 [on-container-resize void] ; This object doesn't contain anything

	 [init-min (lambda (x) x)]
	 
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
	 (set-min-width (init-min (get-width)))
	 (set-min-height (init-min (get-height)))
	 
	 (send (area-parent) add-child this)))))

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

(define (queue-window-callback w cb)
  (parameterize ([wx:current-eventspace (ivar (send w get-top-level) eventspace)])
    (wx:queue-callback cb wx:middle-queue-key)))

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
    (inherit get-x get-y get-width get-height area-parent)
    (rename [super-on-size on-size]
	    [super-on-set-focus on-set-focus]
	    [super-on-kill-focus on-kill-focus]
	    [super-pre-on-char pre-on-char]
	    [super-pre-on-event pre-on-event])
    (private
      [pre-wx->proxy (lambda (w k) ; MacOS: w may not be something the user knows
		       (if w
			   (if (is-a? w wx/proxy<%>)
			       (k (wx->proxy w))
			       (pre-wx->proxy (send w get-parent) k))
			   #f))]
      [old-w -1]
      [old-h -1]
      [old-x -1]
      [old-y -1])
    (override
      [on-drop-file (lambda (f)
		      (send proxy on-drop-file f))]
      [on-size (lambda (w h)
		 (super-on-size w h)
		 ; Delay callback to make sure X structures (position) are updated, first
		 (queue-window-callback
		  this
		  (lambda ()
		    (when mred 
		      (let* ([w (get-width)]
			     [h (get-height)])
			(when (not (and (= w old-w) (= h old-h)))
			  (set! old-w w)
			  (set! old-h h)
			  (send mred on-size w h)))
		      (let* ([p (area-parent)]
			     [x (- (get-x) (or (and p (send p dx)) 0))]
			     [y (- (get-y) (or (and p (send p dy)) 0))])
			(when (not (and (= x old-x) (= y old-y)))
			  (set! old-x x)
			  (set! old-y y)
			  (send mred on-move x y)))))))]
      [on-set-focus (lambda ()
		      (super-on-set-focus)
		      (send proxy on-focus #t))]
      [on-kill-focus (lambda ()
		      (super-on-kill-focus)
		      (send proxy on-focus #f))]
      [pre-on-char (lambda (w e)
		     (super-pre-on-char w e)
		     (pre-wx->proxy w (lambda (m) (send proxy on-subwindow-char m e))))]
      [pre-on-event (lambda (w e)
		      (super-pre-on-event w e)
		      (pre-wx->proxy w (lambda (m) (send proxy on-subwindow-event m e))))])
    (sequence (apply super-init mred proxy args))))

(define (make-container-glue% %)
  (class % (mred proxy . args)
    (inherit do-place-children)
    (override
      [place-children (lambda (l w h) (cond
				       [(null? l) null]
				       [mred (send mred place-children l w h)]
				       [else (do-place-children l w h)]))])
    (sequence
      (apply super-init mred proxy args))))

(define active-frame #f)

(wx:application-file-handler (lambda (f)
			       (when active-frame
				 (queue-window-callback
				  active-frame
				  (lambda () (when (ivar active-frame accept-drag?)
					       (send active-frame on-drop-file f)))))))

(define (make-top-level-window-glue% %) ; implies make-window-glue%
  (class (make-window-glue% %) (mred proxy . args)
    (rename [super-on-activate on-activate])
    (public [act-date/seconds 0] [act-date/milliseconds 0] [act-on? #f])
    (override
      [on-close (lambda ()
		  (if mred
		      (if (send mred can-close?)
			  (begin
			    (send mred on-close)
			    #t)
			  #f)
		      #t))]
      [on-activate (lambda (on?)
		     (set! act-on? on?)
		     (when on?
		       (set! act-date/seconds (current-seconds))
		       (set! act-date/milliseconds (current-milliseconds))
		       (set! active-frame this))
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
      [do-on-char (lambda (e) (super-on-char e))]
      [do-on-event (lambda (e) (super-on-event e))]
      [do-on-scroll (lambda (e) (super-on-scroll e))]
      [do-on-paint (lambda () (super-on-paint))])
    (override
      [on-char (lambda (e)
		 (if mred
		     (send mred on-char e)
		     (super-on-char e)))]
      [on-event (lambda (e)
		 (if mred
		     (send mred on-event e)
		     (super-on-event e)))]
      [on-scroll (lambda (e)
		 (if mred
		     ; Delay callback for windows scrollbar grab
		     (queue-window-callback
		      this
		      (lambda ()
			(send mred on-scroll e)))
		     (super-on-scroll e)))]
      [on-paint (lambda ()
		 (if mred
		     (send mred on-paint)
		     (super-on-paint)))])
    (sequence (apply super-init mred proxy args))))

;------------- Create the actual wx classes -----------------

(define wx-frame%
  (make-top-level-window-glue% 
   (class (make-top-container% wx:frame%) args
     (rename [super-set-menu-bar set-menu-bar])
     (public
       [menu-bar #f])
     (override
       [set-menu-bar
	(lambda (mb)
	  (when mb (set! menu-bar mb))
	  (super-set-menu-bar mb))]
       [on-menu-command
	(lambda (id)
	  (let ([wx (wx:id-to-menu-item id)])
	    (send (wx->mred wx) go)))])
     (public
       [handle-menu-key
	(lambda (event)
	  (and menu-bar (send menu-bar handle-key event)))])
     (sequence
       (apply super-init args)))))

(define wx-dialog%
  (make-top-level-window-glue% 
   (class (make-top-container% wx:dialog%) args
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
     (override
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

(define (make-editor-canvas% %)
  (class % (parent x y w h name style spp init-buffer)
    (inherit get-editor force-redraw
	     call-as-primary-owner min-height get-size
	     hard-min-height set-min-height)
    (rename [super-set-editor set-editor]
	    [super-on-set-focus on-set-focus])
    (private
      [fixed-height? #f]
      [fixed-height-lines 0]
      [orig-hard #f])
    (override
      [on-container-resize (lambda ()
			     (let ([edit (get-editor)])
			       (when edit
				 (send edit on-display-size))))]
      [on-set-focus
       (lambda ()
	 (super-on-set-focus)
	 (let ([m (get-editor)])
	   (when m 
	     (let ([mred (wx->mred this)])
	       (when mred
		 (send m set-active-canvas mred))))))]
      [set-editor
       (letrec ([l (case-lambda
		    [(edit) (l edit #t)]
		    [(edit redraw?)
		     (super-set-editor edit redraw?)
		     
		     (let ([mred (wx->mred this)])
		       (when mred
			 (send edit add-canvas mred)))

		     (update-size)
		     
		     ; force-redraw causes on-container-resize to be called,
		     ;  but only when the size of the canvas really matters
		     ;  (i.e., when it is shown)
		     (force-redraw)])])
	 l)])
    (public
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
	 (let ([edit (get-editor)])
	   (when (and edit fixed-height?)
	     (let* ([top (send edit line-location 0 #t)]
		    [bottom (send edit line-location 0 #f)]
		    [height (- bottom top)])
	       (let* ([ch (box 0)]
		      [h (box 0)])
		 (call-as-primary-owner
		  (lambda ()
		    (send (send edit get-admin) 
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

(define wx-editor-canvas% (make-canvas-glue%
			   (make-editor-canvas% (make-control% wx:editor-canvas%
							       0 0 #t #t))))

(define internal-editor<%> (interface ()))
(define editor<%> (interface (wx:editor<%>)
		    get-canvases
		    get-active-canvas set-active-canvas
		    get-canvas
		    add-canvas remove-canvas
		    auto-wrap))
		    
(define (make-editor-buffer% % can-wrap?)
  ; >>> This class is instantiated directly by the end-user <<<
  (class* % (editor<%> internal-editor<%>) args
    (inherit get-max-width set-max-width get-admin)
    (rename [super-on-display-size on-display-size])
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
      [set-active-canvas
       (lambda (new-canvas)
	 (check-instance '(method editor<%> set-active-canvas) editor-canvas% "editor-canvas" #t new-canvas)
	 (set! active-canvas (mred->wx new-canvas)))]

      [add-canvas
       (lambda (new-canvas)
	 (check-instance '(method editor<%> add-canvas) editor-canvas% "editor-canvas" #f new-canvas)
	 (let ([new-canvas (mred->wx new-canvas)])
	   (unless (memq new-canvas canvases)
	     (set! canvases (cons new-canvas canvases)))))]

      [remove-canvas
       (lambda (old-canvas)
	 (check-instance '(method editor<%> remove-canvas) editor-canvas% "editor-canvas" #f old-canvas)
	 (let ([old-canvas (mred->wx old-canvas)])
	   (when (eq? old-canvas active-canvas)
	     (set! active-canvas #f))
	   (set! canvases (remq old-canvas canvases))))]

      [auto-wrap (case-lambda
		  [() auto-set-wrap?]
		  [(on?) (set! auto-set-wrap? (and on? #t))
			 (on-display-size)])])
    (override
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
	 (unless (memq type '(text pasetboard))
	   (raise-type-error (who->name '(method editor<%> on-new-box)) "symbol: text or pasteboard" type))
	 (make-object editor-snip%
		      (make-object (cond
				    [(eq? type 'pasteboard) pasteboard%]
				    [else text%]))))])

    (sequence (apply super-init args))))

(define text% (class (make-editor-buffer% wx:text% #t) args
		(sequence (apply super-init args))))
(define pasteboard% (class (make-editor-buffer% wx:pasteboard% #f) args
		      (sequence (apply super-init args))))

(define editor-snip% (class wx:editor-snip% ([edit #f] . args)
			(sequence
			  (apply super-init (or edit (make-object text%)) args))))

;--------------------- wx Panel Classes -------------------------

(define wx:windowless-panel%
  (class null (parent x y w h style)
    (private
      [pos-x 0] [pos-y 0] [width 1] [height 1])
    (public
      [drag-accept-files void]
      [on-drop-file void]
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
  (class (wx-make-container% (make-item% wx:panel% 0 0 #t #t)) (parent style)
    (inherit get-x get-y get-width get-height
	     min-width min-height set-min-width set-min-height
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
      
      ; Not used by linear panels
      [h-align 'center] [v-align 'center]

      ; Needed for windowless panes
      [move-children? #f]

      [ignore-redraw-request? #f])
    
    (override
      [set-focus ; dispatch focus to a child panel
       (lambda ()
	 (if (null? children)
	     (super-set-focus)
	     (send (car children) set-focus)))])
    
    (public
      [need-move-children (lambda () (set! move-children? #t))]

      [border
       (let ([curr-border const-default-border])
	 (case-lambda
	  [() curr-border]
	  [(new-val)
	   (check-reasonable-margin 'border new-val)
	   (set! curr-border new-val)
	   (force-redraw)]))]

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
		     "children of the container ~e; list: ~e")
		    this new-children))
	   ; show all new children, hide all deleted children.
	   (let ([added-children (list-diff new-children children)]
		 [removed-children (list-diff children new-children)])
	     (unless (andmap (lambda (child)
			     (is-a? wx:window% child))
			     removed-children)
	       (error 'change-children
		      "Cannot make non-window areas inactive in ~e"
		      this))
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
      
      [child-redraw-request
       (lambda (from)
	 (unless (or ignore-redraw-request?
		     (not (memq from children)))
	   (force-redraw)))]
      
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
			    (child-info-y-min (car kid-info)))))))])
      
    (override
     [force-redraw
       (lambda ()
	 (set! children-info #f)
	 (set! curr-width #f)
	 (let ([parent (area-parent)])
	   (send parent child-redraw-request this)))]

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
			(= curr-height client-height)
			(not move-children?))
	     (set! curr-width client-width)
	     (set! curr-height client-height)
	     (set! move-children? #f)
	     (redraw client-width client-height))))]

      [init-min (lambda (x) 0)])
      
    (public
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
		   0 0
		   (car curr-info) ; child-info-x-min
		   (cadr curr-info)) ; child-info-y-min
		  (loop (cdr children-info)))))))]
      
      [spacing ; does nothing!
       (let ([curr-spacing const-default-spacing])
	 (case-lambda
	  [() curr-spacing]
	  [(new-val) (set! curr-spacing new-val)]))]

      [do-align (lambda (h v set-h set-v)
		  (unless (memq h '(left center right))
		    (raise-type-error 'alignment "horizontal alignment symbol: left, center, or right" h))
		  (unless (memq v '(top center bottom))
		    (raise-type-error 'alignment "vertical alignment symbol: top, center, or bottom" v))
		  (set-h h)
		  (set-v (case v [(top) 'left] [(center) 'center] [(bottom) 'right])))]
      [alignment (lambda (h v) (do-align h v (lambda (h) (set! h-align h)) (lambda (h) (set! v-align v))))]
      [get-alignment (lambda () (values h-align v-align))]

      ; redraw: redraws panel and all children
      ; input: width, height: size of area area in panel.
      ; returns: nothing
      ; effects: places children at default positions in panel.
      [redraw
       (lambda (width height)
	 (let ([children-info (get-children-info)])
	   (panel-redraw children children-info
			 (place-children (map (lambda (i)
						(list (child-info-x-min i) (child-info-y-min i)
						      (child-info-x-stretch i) (child-info-y-stretch i)))
					      children-info)
					 width height))))]
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
      (super-init parent -1 -1 0 0 style))))

(define (wx-make-pane% wx:panel%)
  (class (make-container-glue% (make-glue% (wx-make-basic-panel% wx:panel%))) args
    (inherit get-parent get-x get-y need-move-children)
    (rename [super-set-size set-size])
    (override
      [get-window (lambda () (send (get-parent) get-window))]
      [set-size (lambda (x y w h) 
		  (super-set-size x y w h)
		  (need-move-children))]
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
    (override
      [spacing
       (let ([curr-spacing const-default-spacing])
	 (case-lambda
	  [() curr-spacing]
	  [(new-val)
	   (check-reasonable-margin 'spacing new-val)
	   (set! curr-spacing new-val)
	   (force-redraw)]))])
    (public
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
      
      [do-get-alignment (lambda (pick) (values (pick major-align-pos minor-align-pos)
					       (case (pick minor-align-pos major-align-pos)
						 [(top) 'left] [(center) 'center] [(right) 'bottom])))]

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
    (override
      [alignment (lambda (h v) (do-align h v major-align minor-align))]
      [get-alignment (lambda () (do-get-alignment (lambda (x y) x)))]
    
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
				car    ; child-info-x-min
				caddr  ; child-info-x-stretch
				major-offset
				cadr   ; child-info-y-min
				cadddr ; child-info-y-stretch
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
    (override
      [alignment (lambda (h v) (do-align h v minor-align major-align))]
      [get-alignment (lambda () (do-get-alignment (lambda (x y) y)))]

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
				cadr   ; child-info-y-min
				cadddr ; child-info-y-stretch
				major-offset
				car    ; child-info-x-min
				caddr  ; child-info-x-stretch
				minor-offset
				(lambda (width height) height)
				(lambda (width height) width)
				(lambda (major minor) minor)
				(lambda (major minor) major)))])
    (sequence (apply super-init args))))

(define wx-panel% (wx-make-panel% wx:panel%))
(define wx-linear-panel% (wx-make-linear-panel% wx-panel%))
(define wx-horizontal-panel% (wx-make-horizontal-panel% wx-linear-panel%))
(define wx-vertical-panel% (wx-make-vertical-panel% wx-linear-panel%))

(define wx-pane% (wx-make-pane% wx:windowless-panel%))
(define wx-linear-pane% (wx-make-linear-panel% wx-pane%))
(define wx-horizontal-pane% (wx-make-horizontal-panel% wx-linear-pane%))
(define wx-vertical-pane% (wx-make-vertical-panel% wx-linear-pane%))

;-------------------- Text control simulation -------------------------

(define wx-text-text-editor% 
  (class text% (cb return-cb control)
      (rename [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-on-char on-char])
      (inherit get-text last-position)
      (private
	[block-callback 1]
	[callback
	 (lambda (type)
	   (when (zero? block-callback)
	     (let ([e (make-object wx:control-event% type)])
	       (cb control e))))])
      (override
	[on-char
	 (lambda (e)
	   (let ([c (send e get-key-code)])
	     (unless (and (or (eq? c #\return) (eq? c #\newline))
			  return-cb
			  (return-cb (lambda () (callback 'text-field-enter) #t)))
	       (super-on-char e))))]
	[after-insert
	 (lambda args
	   (apply super-after-insert args)
	   (callback 'text-field))]
	[after-delete
	 (lambda args
	   (apply super-after-delete args)
	   (callback 'text-field))])
      (public
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
  
(define wx-text-editor-canvas% 
  (class wx-editor-canvas% (mred proxy control parent style)
    (rename [super-on-char on-char])
    (override
      [on-char (lambda (e) (send control on-char e))])
    (public
      [continue-on-char (lambda (e) (super-on-char e))])
    (sequence
      (super-init mred proxy parent -1 -1 100 30 #f style 100 #f))))
  
(define (font->delta f)
  (define d (make-object wx:style-delta%))
  (let ([v (send f get-face)])
    (if v
	(send d set-delta-face v)
	(send d set-delta 'change-family (send f get-family))))
  (send d set-delta 'change-size (send f get-point-size))
  (send d set-delta 'change-style (send f get-style))
  (send d set-delta 'change-weight (send f get-weight))
  (send d set-delta 'change-underline (send f get-underlined))
  d)

(define wx-text-field%
  (class wx-horizontal-panel% (mred proxy parent func label value style)
    (inherit alignment stretchable-in-y get-control-font)
    (rename [super-place-children place-children])
    (sequence
      (super-init #f proxy parent null))
    (private
      [multi? (memq 'multiple style)]
      [horiz? (eq? (send (send parent get-window) get-label-position) 'horizontal)]
      [p (if horiz?
	     this
	     (make-object wx-vertical-pane% #f proxy this null))]
      [l (and label
	      (make-object wx-message% #f proxy p label -1 -1 null))]
      [c (make-object wx-text-editor-canvas% #f proxy this p
		      (if multi?
			  (if (memq 'hscroll style)
			      null
			      '(hide-hscroll))
			  '(hide-vscroll hide-hscroll)))]
      [e (make-object wx-text-text-editor%
		      func
		      (lambda (do-cb)
			(if multi?
			    #f
			    (do-cb)))
		      this)]
      [dy 0])
    (public
      [get-editor (lambda () e)]
      
      [get-value (lambda () (send e get-text))]
      [set-value (lambda (v) (send e without-callback
				   (lambda () (send e insert v 0 (send e last-position)))))]

      [get-label (lambda () (send l get-label))]
      [set-label (lambda (str) (send l set-label str))])
    (override
      [set-cursor (lambda (c) (send e set-cursor c #t))]
      [on-char (lambda (ev) (send c continue-on-char ev))]
      [set-focus (lambda () (send c set-focus))]
	
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
      (send e auto-wrap (and multi? (not (memq 'hscroll style))))
      (let ([f (get-control-font)]
	    [s (send (send e get-style-list) find-named-style "Standard")])
	(send s set-delta (font->delta f)))
      (send c set-editor e)
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

(define (constructor-name who)
  (string->symbol (format "initialization for ~a%" who)))

(define (check-container-parent who p)
  (unless (is-a? p internal-container<%>)
    (raise-type-error (string->symbol (constructor-name who))
		      "built-in container<%> object" p)))

(define (check-top-level-parent/false who p)
  (unless (or (not p) (is-a? p frame%) (is-a? p dialog%))
    (raise-type-error (who->name who) "frame% or dialog% object or #f" p)))

(define (check-orientation who l)
  (check-style `(constructor-name ,who) '(vertical horizontal) null l))

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
      (lambda (w e) (cb (wx->proxy w) e))
      cb))

(define mred-get-low-level-window (make-generic mred% get-low-level-window))
(define wx-key (gensym))
(define (mred->wx w) ((mred-get-low-level-window w) wx-key))

(define (mred->wx-container w) (send (mred->wx w) get-container))

;---------------- Window interfaces and base classes ------------

(define area<%>
  (interface ()
    get-parent get-top-level-window
    min-width min-height
    stretchable-width stretchable-height
    get-low-level-window))

(define area%
  (class* mred% (area<%>) (mk-wx get-wx-panel parent)
    (public
      [get-parent (lambda () parent)]
      [get-top-level-window (lambda () (wx->mred (send wx get-top-level)))]
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

(define area-container<%> 
  (interface (area<%>) 
    get-children change-children place-children
    add-child delete-child
    border spacing 
    set-alignment get-alignment))

(define internal-container<%> (interface ()))

(define (make-container% %) ; % implements area<%>
  (class* % (area-container<%> internal-container<%>) (mk-wx get-wx-panel parent) 
    (public
      [get-children (lambda () (map wx->mred (ivar (get-wx-panel) children)))]
      [border (param get-wx-panel 'border)]
      [spacing (param get-wx-panel 'spacing)]
      [set-alignment (lambda (h v) (send (get-wx-panel) alignment h v))]
      [get-alignment (lambda () (send (get-wx-panel) get-alignment))]
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

(define window<%>
  (interface (area<%>)
    on-focus focus has-focus?
    on-size on-move
    accept-drop-files on-drop-file
    on-subwindow-char on-subwindow-event
    client->screen screen->client
    enable is-enabled?
    get-label set-label get-plain-label
    get-client-size get-size get-width get-height get-x get-y
    get-cursor set-cursor 
    show is-shown?
    refresh))

(define (make-window% top? %) ; % implements area<%>
  (class* % (window<%>) (mk-wx get-wx-panel label parent cursor)
    (public
      [on-focus void]
      [on-size void]
      [on-move void]
      [on-subwindow-char (lambda (w e) #f)]
      [on-subwindow-event (lambda (w e) #f)]
      [on-drop-file void]

      [focus (lambda () (send wx set-focus))]
      [has-focus? (lambda () (send wx has-focus?))]
      [enable (lambda (on?) (send wx enable on?))]
      [is-enabled? (lambda () (send wx is-enabled?))]
      
      [get-label (lambda () label)]
      [set-label (lambda (l) (set! label l))]
      [get-plain-label (lambda () (wx:label->plain-label label))]

      [accept-drop-files
       (case-lambda
	[() (ivar wx accept-drag?)]
	[(on?) (send wx drag-accept-files on?)])]
      
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
      [get-size (lambda ()
		  (double-boxed
		   0 0
		   (lambda (x y) (send wx get-size x y))))]
      
      [get-width (lambda () (send wx get-width))]
      [get-height (lambda () (send wx get-height))]
      [get-x (lambda () (- (send wx get-x) (if top? 0 (send (send wx get-parent) dx))))]
      [get-y (lambda () (- (send wx get-y) (if top? (send (send wx get-parent) dy))))]
      
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
  (interface (window<%> area-container<%>)
    set-control-font get-control-font
    set-label-font get-label-font
    set-label-position get-label-position))

(define (make-area-container-window% %) ; % implements window<%> (and carea-ontainer<%>)
  (class* % (area-container-window<%>) (mk-wx get-wx-panel label parent cursor) 
    (public
      [get-control-font (lambda () (send (get-wx-panel) get-control-font))]
      [set-control-font (lambda (x) (send (get-wx-panel) set-control-font x))]
      [get-label-font (lambda () (send (get-wx-panel) get-label-font))]
      [set-label-font (lambda (x) (send (get-wx-panel) set-label-font x))]
      [get-label-position (lambda () (send (get-wx-panel) get-label-position))]
      [set-label-position (lambda (x) (send (get-wx-panel) set-label-position x))])
    (sequence
      (super-init mk-wx get-wx-panel label parent cursor))))

(define top-level-window<%>
  (interface (area-container-window<%>)
    get-eventspace
    on-activate
    can-close? on-close
    get-focus-window get-edit-target-window
    get-focus-object get-edit-target-object
    center move resize))

(define basic-top-level-window%
  (class* (make-area-container-window% (make-window% #t (make-container% area%))) (top-level-window<%>) (mk-wx label parent)
    (rename [super-set-label set-label])
    (private
      [wx-object->proxy
       (lambda (o)
	 (if (is-a? o wx:window%)
	     (wx->proxy o)
	     o))])
    (override
      [set-label (lambda (l)
		   (send wx set-title (wx:label->plain-label l))
		   (super-set-label l))])
    (public
      [get-eventspace (lambda () (ivar wx eventspace))]
      [can-close? (lambda () #t)]
      [on-close void]
      [on-activate void]
      [center (case-lambda
	       [() (send wx center 'both)]
	       [(dir) (send wx center dir)])]
      [move (lambda (x y)
	      (send wx move x y))]
      [resize (lambda (w h)
		(send wx set-size -1 -1 w h))]

      [get-focus-window (lambda () (let ([w (send wx get-focus-window)])
				     (and w (wx->proxy w))))]
      [get-edit-target-window (lambda () (let ([w (send wx get-edit-target-window)])
					   (and w (wx->proxy w))))]
      [get-focus-object (lambda () (let ([o (send wx get-focus-object)])
				     (and o (wx-object->proxy o))))]
      [get-edit-target-object (lambda () (let ([o (send wx get-edit-target-object)])
					   (and o (wx-object->proxy o))))])
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
  (class* (make-window% #f (make-subarea% area%)) (control<%>) (mk-wx label parent cursor)
    (rename [super-set-label set-label])
    (override
      [set-label (lambda (l)
		   (send wx set-label l)
		   (super-set-label l))])
    (public
      [command (lambda (e) (send wx command e))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () (set! wx (mk-wx)) wx) (lambda () wx) label parent cursor))))

;--------------------- Final mred class construction --------------------
    
(define frame%
  (class basic-top-level-window% (label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null])
    (sequence
      (let ([cwho '(constructor frame)])
	(check-string cwho label)
	(check-top-level-parent/false cwho parent)
	(for-each (lambda (x) (check-dimension cwho x)) (list width height x y))
	(check-style cwho #f '(no-thick-border no-resize-border no-caption no-system-menu
					       iconize maximize mdi-parent mdi-child) 
		     style)))
    (private
      [wx #f]
      [status-line? #f])
    (override
      [on-subwindow-char (lambda (w event) (send wx handle-menu-key event))])
    (public
      [create-status-line (lambda () (unless status-line? (send wx create-status-line) (set! status-line? #t)))]
      [set-status-text (lambda (s) (send wx set-status-text s))]
      [has-status-line? (lambda () status-line?)]
      [iconize (lambda (on?) (send wx iconize on?))]
      [is-iconized? (lambda () (send wx iconized?))]
      [set-icon (lambda (i) (send wx set-icon i))]
      [maximize (lambda () (send wx maximize))]
      [get-menu-bar (lambda () (let ([mb (ivar wx menu-bar)])
				 (and mb (wx->mred mb))))])
    (sequence
      (super-init (lambda (finish) 
		    (set! wx (finish (make-object wx-frame% this this
						  (and parent (mred->wx parent)) (wx:label->plain-label label)
						  (or x -1) (or y -1) (or width -1) (or height -1)
						  style)))
		    wx)
		  label parent))))

(define dialog%
  (class basic-top-level-window% (label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null])
    (sequence
      (let ([cwho '(constructor dialog)])
	(check-string cwho label)
	(check-top-level-parent/false cwho parent)
	(for-each (lambda (x) (check-dimension cwho x)) (list width height x y))
	(check-style cwho #f '(no-caption) style))
      (super-init (lambda (finish) (finish (make-object wx-dialog% this this
							(and parent (mred->wx parent)) (wx:label->plain-label label) #t
							(or x -1) (or y -1) (or width 0) (or height 0)
							style)))
		  label parent))))

(define (get-top-level-windows)
  (map wx->mred (wx:get-top-level-windows)))

(define (get-top-level-focus-window)
  (ormap (lambda (f) (and (ivar f act-on?) (wx->mred f))) (wx:get-top-level-windows)))

(define (get-top-level-edit-target-window)
  (let loop ([l (wx:get-top-level-windows)][f #f][s 0][ms 0])
    (if (null? l)
	(and f (wx->mred f))
	(let* ([f2 (car l)]
	       [s2 (ivar f2 act-date/seconds)]
	       [ms2 (ivar f2 act-date/milliseconds)])
	  (if (or (not f)
		  (> s2 s)
		  (and (= s2 s) (> ms2 ms)))
	      (loop (cdr l) f2 s2 ms2)
	      (loop (cdr l) f s ms))))))

(define message%
  (class basic-control% (label parent [style null])
    (sequence
      (check-string-or-bitmap '(constructor message) label)
      (check-container-parent 'message parent)
      (check-style '(constructor message) #f null style)
      (super-init (lambda () (make-object wx-message% this this
					  (mred->wx-container parent)
					  label -1 -1 style))
		  label parent #f))))

(define button%
  (class basic-control% (label parent callback [style null])
    (sequence
      (check-string-or-bitmap '(constructor button) label)
      (check-container-parent 'button parent)
      (check-callback '(constructor button) callback)
      (check-style '(constructor button) #f '(border) style)
      (super-init (lambda () (make-object wx-button% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label -1 -1 -1 -1 style))
		  label parent #f))))

(define check-box%
  (class basic-control% (label parent callback [style null])
    (sequence
      (check-string-or-bitmap '(constructor check-box) label)
      (check-container-parent 'check-box parent)
      (check-callback '(constructor check-box) callback)
      (check-style '(constructor check-box) #f null style))
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
    (sequence 
      (check-string/false '(constructor radio-box) label)
      (unless (and (list? choices) (pair? choices)
		   (or (andmap string? choices)
		       (andmap (lambda (x) (is-a? x wx:bitmap%)) choices)))
	(raise-type-error (constructor-name 'radio-box) "non-empty list of strings or bitmap% objects" choices))
      (check-container-parent 'radio-box parent)
      (check-callback '(constructor radio-box) callback)
      (check-orientation 'radio-box style))
    (private
      [wx #f])
    (override
      [enable (case-lambda
	       [(on?) (send wx enable on?)]
	       [(which on?) (send wx enable which on?)])]
      [is-enabled? (case-lambda
		    [() (send wx is-enabled?)]
		    [(which) (send wx is-enabled? which)])])
    (public
      [get-number (lambda () (length choices))]
      [get-item-label (lambda (n) 
			(and (< -1 n (get-number))
			     (list-ref choices n)))]
      [get-item-plain-label (lambda (n) 
			      (and (< -1 n (get-number))
				   (wx:label->plain-label (list-ref choices n))))]
       
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
    (sequence 
      (check-string/false '(constructor slider) label)
      (check-slider-integer '(constructor slider) min-val)
      (check-slider-integer '(constructor slider) max-val)
      (check-container-parent 'slider parent) 
      (check-callback '(constructor slider) callback)
      (check-slider-integer '(constructor slider) value)
      (check-orientation 'slider style))
    (private
      [wx #f])
    (public
      [get-value (lambda () (send wx get-value))]
      [set-value (lambda (v)
		   (check-slider-integer '(method slider% set-value) v)
		   (send wx set-value v))])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-slider% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label value min-val max-val style))
		    wx)
		  label parent #f))))

(define gauge%
  (class basic-control% (label range parent [style '(horizontal)])
    (sequence 
      (check-string/false '(constructor gauge) label)
      (check-container-parent 'gauge parent) 
      (check-range-integer '(constructor gauge) range)
      (check-orientation 'gauge style))
    (private
      [wx #f])
    (public
      [get-value (lambda () (send wx get-value))]
      [set-value (lambda (v)
		   (check-range-integer '(method gauge% set-value) v)
		   (send wx set-value v))]
      [get-range (lambda () (send wx get-range))]
      [set-range (lambda (v)
		   (check-range-integer '(method gauge% set-range) v)
		   (send wx set-range v))])
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

(define (-1=>false v) (if (negative? v) #f v))

(define basic-list-control%
  (class* basic-control% (list-control<%>) (mk-wx label parent)
    (public
      [append (lambda (i) (send wx append i))]
      [clear (lambda () (send wx clear))]
      [get-number (lambda () (send wx number))]
      [get-string (lambda (n) (send wx get-string n))]
      [get-selection (lambda () (and (positive? (get-number)) (-1=>false (send wx get-selection))))]
      [get-string-selection (lambda () (and (positive? (get-number)) (send wx get-string-selection)))]
      [set-selection (lambda (s) (send wx set-selection s))]
      [set-string-selection (lambda (s) (send wx set-string-selection s))]
      [find-string (lambda (x) (-1=>false (send wx find-string x)))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () (set! wx (mk-wx)) wx) label parent #f))))

(define (check-list-control-args who label choices parent callback)
  (let ([cwho `(constructor-name ,who)])
    (check-string/false cwho label)
    (unless (and (list? choices) (andmap string? choices))
      (raise-type-error (who->name cwho) "list of strings" choices))
    (check-container-parent who parent)
    (check-callback cwho callback)))

(define choice%
  (class basic-list-control% (label choices parent callback [style null])
    (sequence
      (check-list-control-args 'choice label choices parent callback)
      (check-style '(constructor choice) #f null style)
      (super-init (lambda () (make-object wx-choice% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label -1 -1 -1 -1 choices style))
		  label parent))))

(define list-box%
  (class basic-list-control% (label choices parent callback [style '(single)])
    (sequence 
      (check-list-control-args 'list-box label choices parent callback)
      (check-style '(constructor list-box) '(single multiple extended) null style))
    (rename [super-append append])
    (override
      [append (case-lambda
	       [(i) (super-append i)]
	       [(i d) (send wx append i d)])])
    (public
      [delete (lambda (n) (send wx delete n))]
      [get-data (lambda (n) (send wx get-data n))]
      [get-selections (lambda () (send wx get-selections))]
      [number-of-visible-items (lambda () (send wx number-of-visible-items))]
      [is-selected? (lambda (n) (send wx selected? n))]
      [set (lambda (l) (send wx set l))]
      [set-string (lambda (n d) (send wx set-string n d))]
      [set-data (lambda (n d) (send wx set-data n d))]
      [get-first-visible-item (lambda () (send wx get-first-item))]
      [set-first-visible-item (lambda (n) (send wx set-first-item n))]
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

(define text-field%
  (class* basic-control% () (label parent callback [init-val ""] [style '(single)])
    (sequence 
      (let ([cwho '(constructor-name text-field)])
	(check-string/false cwho label)
	(check-container-parent 'text-field parent)
	(check-callback cwho callback)
	(check-string cwho init-val)
	(check-style cwho '(single multiple) '(hscroll) style)))
    (private
      [wx #f])
    (public
      [get-editor (lambda () (send wx get-editor))]
      [get-value (lambda () (send wx get-value))]
      [set-value (lambda (v) 
		   (check-string '(method text-control<%> set-value) v)
		   (send wx set-value v))])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-text-field% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label init-val style))
		    wx)
		  label parent ibeam))))

;-------------------- Canvas class constructions --------------------

(define canvas-default-size 20) ; an arbitrary default size for canvases to avoid initial size problems

(define canvas<%>
  (interface (subwindow<%>)
    min-client-width min-client-height
    on-char on-event on-paint on-scroll
    popup-menu warp-pointer get-dc))

(define basic-canvas%
  (class* (make-window% #f (make-subarea% area%)) (canvas<%>) (mk-wx parent)
    (public
      [on-char (lambda (e) (send wx do-on-char e))]
      [on-event (lambda (e) (send wx do-on-event e))]
      [on-paint (lambda () (send wx do-on-paint))]
      [on-scroll (lambda (e) (send wx do-on-scroll e))]
      
      [min-client-width (param (lambda () wx) 'min-client-width)]
      [min-client-height (param (lambda () wx) 'min-client-height)]

      [popup-menu (lambda (m x y) 
		    (check-instance '(method canvas<%> popup-menu) popup-menu% "popup-menu" #f m)
		    (send wx popup-menu (mred->wx m) x y))]
      [warp-pointer (lambda (x y) (send wx warp-pointer x y))]

      [get-dc (lambda () (send wx get-dc))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () (set! wx (mk-wx)) wx) (lambda () wx) #f parent #f))))

(define canvas%
  (class basic-canvas% (parent [style null])
    (sequence 
      (check-container-parent 'canvas parent)
      (check-style '(constructor canvas) #f '(border hscroll vscroll) style))
    (public
      [get-virtual-size (lambda () (double-boxed
				    0 0
				    (lambda (x y) (send wx get-virtual-size x y))))]
      [get-view-start (lambda () (double-boxed
				  0 0
				  (lambda (x y) (send wx view-start x y))))]

      [scroll (lambda (x y) (send wx scroll x y))]

      [set-scrollbars (letrec ([set-scrollbars
				(case-lambda 
				 [(h-pixels v-pixels x-len y-len x-page y-page x-val y-val)
				  (set-scrollbars h-pixels v-pixels x-len y-len x-page y-page x-val y-val #f)]
				 [(h-pixels v-pixels x-len y-len x-page y-page x-val y-val man?)
				  (send wx set-scrollbars 
					h-pixels v-pixels x-len y-len x-page y-page x-val y-val man?)])])
			set-scrollbars)]

      [get-scroll-pos (lambda (d) (send wx get-scroll-pos d))]
      [set-scroll-pos (lambda (d v) (send wx set-scroll-pos d v))]
      [get-scroll-range (lambda (d) (send wx get-scroll-range d))]
      [set-scroll-range (lambda (d v) (send wx set-scroll-range d v))]
      [get-scroll-page (lambda (d) (send wx get-scroll-page d))]
      [set-scroll-page (lambda (d v) (send wx set-scroll-page d v))])
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
    
(define editor-canvas%
  (class basic-canvas% (parent [buffer #f] [style null] [scrolls-per-page 100])
    (sequence 
      (check-container-parent 'editor-canvas parent)
      (check-instance '(constructor editor-canvas) internal-editor<%> "text% or pasteboard" #t buffer)
      (check-style '(constructor editor-canvas) #f '(hide-vscroll hide-hscroll no-vscroll no-hscroll) style))
    (private
      [force-focus? #f]
      [scroll-to-last? #f]
      [scroll-bottom? #f])
    (public
      [call-as-primary-owner (lambda (f) (send wx call-as-primary-owner f))]
      [allow-scroll-to-last
       (case-lambda
	[() scroll-to-last?]
	[(on?) (set! scroll-to-last? (and on? #t))
	       (send wx allow-scroll-to-last on?)])]
      [scroll-with-bottom-base
       (case-lambda
	[() scroll-bottom?]
	[(on?) (set! scroll-bottom? (and on? #t))
	       (send wx scroll-with-bottom-base on?)])]
      [lazy-refresh
       (case-lambda
	[() (send wx get-lazy-refresh)]
	[(on?) (send wx set-lazy-refresh)])]
      [force-display-focus
       (case-lambda
	[() force-focus?]
	[(on?) (set! force-focus? (and on? #t))
	       (send wx force-display-focus on?)])]

      [set-line-count
       (lambda (n) 
	 (unless (or (not n) (and (number? n) (integer? n) (<= 1 100)))
	   (raise-type-error (who->name '(method editor-canvas% set-line-count))
			     "integer in [1, 100]"
			     n))
	 (send wx set-line-count n))]

      [get-editor (lambda () (send wx get-editor))]
      [set-editor (lambda (m) (send wx set-editor m))])
    (private
      [wx #f])
    (sequence
      (super-init (lambda () 
		    (set! wx (make-object wx-editor-canvas% this this
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
(define pane% (class (make-pane% 'pane basic-pane% wx-pane%) args
		      (sequence (apply super-init args))))
(define vertical-pane% (class (make-pane% 'vertical-pane basic-pane% wx-vertical-pane%) args
		      (sequence (apply super-init args))))
(define horizontal-pane% (class (make-pane% 'horizontal-pane basic-pane% wx-horizontal-pane%) args
		      (sequence (apply super-init args))))

(define (make-panel% who panel% wx-panel%)
  (class panel% (parent [style null])
    (private [wx #f])
    (sequence 
      (check-container-parent who parent)
      (check-style '(constructor panel) #f '(border) style)
      (super-init (lambda () (set! wx (make-object wx-panel% this this (mred->wx-container parent) style)) wx)
		  (lambda () wx) #f parent #f))))


(define basic-panel% (make-area-container-window% (make-window% #f (make-subarea% (make-container% area%)))))
(define panel% (class (make-panel% 'panel basic-panel% wx-panel%)  args
		      (sequence (apply super-init args))))
(define vertical-panel% (class (make-panel% 'vertical-panel basic-panel% wx-vertical-panel%) args
		      (sequence (apply super-init args))))
(define horizontal-panel% (class (make-panel% 'horizontal-panel basic-panel% wx-horizontal-panel%) args
		      (sequence (apply super-init args))))

;;;;;;;;;;;;;;;;;;;;;; Menu classes ;;;;;;;;;;;;;;;;;;;;;;

(define (find-pos l i eq?)
  (let loop ([l l][n 0])
    (cond
     [(null? l) #f]
     [(eq? (car l) i) n]
     [else (loop (cdr l) (add1 n))])))

(define (menu-parent-only who p)
  (unless (is-a? p internal-menu<%>)
    (raise-type-error (constructor-name who) "parent menu% or popup-menu% object" p)))

(define (menu-or-bar-parent who p)
  (unless (or (is-a? p internal-menu<%>) (is-a? p menu-bar%))
    (raise-type-error (constructor-name who) "built-in menu-item-container<%> object" p)))

(define (barless-frame-parent p)
  (unless (is-a? p frame%)
    (raise-type-error (constructor-name 'menu-bar) "frame% object" p))
  (when (send (mred->wx p) get-menu-bar)
    (error (constructor-name 'menu-bar) "the specified frame already has a menu bar")))

(define wx-menu-item%
  (class* wx:menu-item% (wx<%>) (mred)
    (private 
      [keymap #f])
    (public
      [get-keymap (lambda () keymap)]
      [set-keymap (lambda (k) (set! keymap k))]
      [swap-keymap (lambda (parent k) 
		     (send (mred->wx parent) swap-item-keymap keymap k) 
		     (set-keymap k))]
      [get-mred (lambda () mred)])
    (sequence
      (super-init))))

(define wx-menu-bar%
  (class* wx:menu-bar% (wx<%>) (mred)
    (inherit delete)
    (rename [super-append append])
    (private
      [items null]
      [keymap (make-object wx:keymap%)])
    (public
      [handle-key (lambda (event) (send keymap handle-key-event this event))]
      [get-mred (lambda () mred)]
      [get-items (lambda () items)]
      [append-item (lambda (item menu title)
		     (super-append menu title)
		     (set! items (append items (list item)))
		     (send keymap chain-to-keymap (send (mred->wx item) get-keymap) #f))]
      [delete-item (lambda (i)
		     (let ([p (position-of i)])
		       (set! items (remq i items))
		       (delete #f p)
		       (send keymap remove-chained-keymap (send (mred->wx i) get-keymap))))]
      [position-of (lambda (i) (find-pos items i eq?))])
    (sequence
      (super-init null null))))

(define wx-menu%
  (class* wx:menu% (wx<%>) (mred popup-label popup-callback)
    (private
      [items null]
      [keymap (make-object wx:keymap%)])
    (inherit delete-by-position)
    (rename [super-delete delete])
    (public
      [get-keymap (lambda () keymap)]
      [get-mred (lambda () mred)]
      [get-items (lambda () items)]
      [append-item (lambda (i) 
		     (set! items (append items (list i)))
		     (let ([k (send (mred->wx i) get-keymap)])
		       (when k
			 (send keymap chain-to-keymap k #f))))]
      [delete-sep (lambda (i) 
		    (delete-by-position (find-pos items i eq?))
		    (set! items (remq i items)))]
      [swap-item-keymap (lambda (old-k new-k)
			  (when old-k (send keymap remove-chained-keymap old-k))
			  (when new-k (send keymap chain-to-keymap new-k #f)))])
    (override
      [delete (lambda (id i) 
		(super-delete id) 
		(set! items (remq i items))
		(let ([k (send (mred->wx i) get-keymap)])
		  (when k
		    (send keymap remove-chained-keymap k))))])
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
    delete restore is-deleted?
    get-low-level-window))

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
		(when shown?
		  (send wx-parent delete-sep this)
		  (set! shown? #f)))]
      [is-deleted? (lambda () (not shown?))])
    (sequence
      (super-init wx)
      (restore))))

(define (strip-tab s) (car (regexp-match (format "^[^~a]*" #\tab) s)))

(define basic-labelled-menu-item%
  (class* mred% (labelled-menu-item<%>) (parent label help-string submenu checkable? keymap set-wx)
    (private
      [wx (set-wx (make-object wx-menu-item% this))]
      [wx-parent (mred->wx parent)]
      [plain-label (wx:label->plain-label label)]
      [in-menu? (is-a? parent basic-menu%)]
      [shown? #f]
      [enabled? #t]
      [do-enable (lambda (on?)
		   (when shown?
		     (if in-menu?
			 (send wx-parent enable (send wx id) on?)
			 (send wx-parent enable-top (send wx-parent position-of this) on?)))
		   (set! enabled? (and on? #t)))])
    (public
      [get-parent (lambda () parent)]
      [get-label (lambda () label)]
      [set-label (letrec ([set-label
			   (case-lambda
			    [(keep-l set-l)
			     (set! label keep-l)
			     (set! plain-label (wx:label->plain-label keep-l))
			     (when shown?
			       (if in-menu?
				   (send wx-parent set-label (send wx id) set-l)
				   (send wx-parent set-label-top (send wx-parent position-of this) plain-label)))]
			    [(l) (set-label l l)])])
		   set-label)]
      [get-plain-label (lambda () plain-label)]
      [get-help-string (lambda () help-string)]
      [set-help-string (lambda (s) 
			 (check-string/false '(method labelled-menu-item<%> set-help-string) s)
			 (set! help-string s)
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
      (when keymap (send wx set-keymap keymap))
      (restore))))

(define shortcut-menu-item<%>
  (interface (labelled-menu-item<%>)
    go
    get-shortcut set-shortcut
    get-x-shortcut-prefix set-x-shortcut-prefix))

(define basic-shortcut-menu-item%
  (class* basic-labelled-menu-item% (shortcut-menu-item<%>) (label checkable? menu callback shortcut help-string set-wx)
    (rename [super-restore restore] [super-set-label set-label])
    (inherit is-deleted? get-label)
    (private
      [wx #f])
    (public
      [go (lambda () (void (callback this (make-object wx:control-event% 'menu))))])
    (private
      [x-prefix 'meta]
      [calc-labels (lambda (label)
		     (let* ([new-label (if shortcut
					   (string-append
					    (strip-tab label)
					    (case (system-type)
					      [(unix) (format "~a~a~a" #\tab 
							      (case x-prefix
								[(meta) "Meta+"]
								[(alt) "Alt+"]
								[(ctl-m) "Ctl+M "]
								[(ctl) "Ctl+"])
							      (char-upcase shortcut))]
					      [(windows) (format "~aCtl+~a" #\tab (char-upcase shortcut))]
					      [(macos) (format "~aCmd+~a" #\tab (char-upcase shortcut))]))
					   (strip-tab label))]
			    [key-binding (and shortcut
					      (case (system-type)
						[(unix) (format "~a~a" 
								(case x-prefix
								  [(meta) "m:"]
								  [(alt) "a:"]
								  [(ctl-m) "c:m;"]
								  [(ctl) "c:"])
								(char-downcase shortcut))]
						[(windows) (format "c:~a" (char-downcase shortcut))]
						[(macos) (format "d:~a" (char-downcase shortcut))]))]
			    [keymap (and key-binding
					 (let ([keymap (make-object wx:keymap%)])
					   (send keymap add-key-function "menu-item" (lambda (edit event) (go)))
					   (send keymap map-function key-binding "menu-item")
					   keymap))])
		       (values new-label keymap)))])
    (override
      [set-label (lambda (l) 
		   (let-values ([(new-label keymap) (calc-labels l)])
		     (super-set-label new-label)
		     (if (is-deleted?)
			 (send wx set-keymap keymap)
			 (send wx swap-keymap menu keymap))))])
    (public
      [set-shortcut (lambda (c) 
		      (check-char/false '(method shortcut-menu-item<%> set-shortcut))
		      (set! shortcut c) (set-label (get-label)))]
      [get-shortcut (lambda () shortcut)]
      [get-x-shortcut-prefix (lambda () x-prefix)]
      [set-x-shortcut-prefix (lambda (p) 
			       (unless (memq p '(meta alt ctl-m ctl))
				 (raise-type-error (who->name '(method shortcut-menu-item<%> set-x-shortcut-prefix))
						   "symbol: meta, alt, ctl-m, or ctl" p))
			       (set! x-prefix p) (set-label (get-label)))])
    (sequence
      (let-values ([(new-label keymap) (calc-labels label)])
	(super-init menu new-label help-string #f checkable? keymap (lambda (x) (set! wx x) (set-wx x)))))))

(define (check-shortcut-args who label menu callback shortcut help-string)
  (let ([cwho `(constructor ,who)])
    (check-string cwho label)
    (menu-parent-only who menu)
    (check-callback cwho callback)
    (check-char/false cwho shortcut)
    (check-string/false cwho help-string)))

(define menu-item%
  (class basic-shortcut-menu-item% (label menu callback [shortcut #f] [help-string #f])
    (sequence 
      (check-shortcut-args 'menu-item label menu callback shortcut help-string)
      (super-init label #f menu callback shortcut help-string (lambda (x) x)))))

(define checkable-menu-item%
  (class basic-shortcut-menu-item% (label menu callback [shortcut #f] [help-string #f])
    (sequence
      (check-shortcut-args 'checkable-menu-item label menu callback shortcut help-string))
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
      (super-init parent label help-string menu #f (send (mred->wx menu) get-keymap) (lambda (x) x)))))

(define menu-item-container<%> (interface () get-items get-low-level-window))
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
      (check-string '(constructor menu) label)
      (menu-or-bar-parent 'menu parent)
      (check-string/false '(constructor menu) help-string)
      (super-init #f void))
    (private
      [item (make-object sub-menu-item% this label parent help-string)])
    (public
      [get-item (lambda () item)])))

(define popup-menu%
  (class basic-menu% ([title #f])
    (sequence
      (check-string/false '(constructor popup-menu) title)
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
      [get-frame (lambda () parent)]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphical-read-eval-print-loop)
  ;; The REPL buffer class
  (define esq:text%
    (class text% ()
      (inherit insert last-position get-text erase change-style clear-undos)
      (rename [super-on-char on-char])
      (private [prompt-pos 0] [locked? #f])
      (override
	[on-insert (lambda (start end) (and (>= start prompt-pos) (not locked?)))]
	[on-delete (lambda (start end) (and (>= start prompt-pos) (not locked?)))]
	[on-char (lambda (c)
		   (super-on-char c)
		   (when (and (memq (send c get-key-code) '(#\return #\newline #\003))
			      (not locked?))
		     (set! locked? #t)
		     (evaluate (get-text prompt-pos (last-position)))))])
      (public
	[new-prompt (lambda ()
		      (output "> ")
		      (set! prompt-pos (last-position))
		      (set! locked? #f)
		      (clear-undos))]
	[output (lambda (str)
		  (let ([l? locked?])
		    (set! locked? #f)
		    (insert str)
		    (set! locked? l?)))]
	[reset (lambda ()
		 (set! locked? #f)
		 (set! prompt-pos 0)
		 (erase)
		 (new-prompt))])
      (sequence 
	(super-init)
	(let ([s (last-position)])
	  (insert (format "Welcome to MrEd version ~a." (version)))
	  (let ([e (last-position)])
	    (insert #\newline)
	    (change-style (send (make-object wx:style-delta% 'change-bold) set-delta-foreground "BLUE") s e)))
	(output (format "Copyright (c) 1995-98 PLT (Matthew Flatt and Robby Findler)~n"))
	(insert "This is a simple window for evaluating MrEd Scheme expressions.") (insert #\newline)
	(let ([s (last-position)])
	  (insert "Quit now and run DrScheme to get a better window.")
	  (let ([e (last-position)])
	    (insert #\newline)
	    (change-style
	     (send (make-object wx:style-delta% 'change-style 'slant) set-delta-foreground "RED")
	     s e)))
	(insert "The current input port always returns eof.") (insert #\newline)
	(new-prompt))))

  ;; GUI creation
  (define frame (make-object (class frame% args
			       (override [on-close (lambda () 
						     (custodian-shutdown-all user-custodian)
						     (semaphore-post waiting))])
			       (sequence (apply super-init args)))
			     "MrEd REPL" #f 500 400))
  (define repl-buffer (make-object esq:text%))
  (define repl-display-canvas (make-object editor-canvas% frame))

  ;; User space initialization
  (define user-custodian (make-custodian))
  
  (define user-eventspace
    (parameterize ([current-custodian user-custodian])
      (wx:make-eventspace)))
  (define user-parameterization (wx:eventspace-parameterization user-eventspace))
  
  (define user-output-port
    (make-output-port (lambda (s) (send repl-buffer output s))
		      (lambda () 'nothing-to-do)))
  
  ;; Evaluation and resetting
  
  (define (evaluate expr-str)
    (parameterize ([wx:current-eventspace user-eventspace])
      (wx:queue-callback
       (lambda ()
	 (current-parameterization user-parameterization)
	 (dynamic-wind
	  void
	  (lambda () 
	    (write (eval (read (open-input-string expr-str))))
	    (newline))
	  (lambda ()
	    (send repl-buffer new-prompt)))))))

  (define waiting (make-semaphore 0))

  (let ([mb (make-object menu-bar% frame)])
    (let ([m (make-object menu% "File" mb)])
      (make-object menu-item% "Quit" m (lambda (i e) (send frame on-close) (send frame show #f)) #\q))
    (let ([m (make-object menu% "Edit" mb)])
      (append-editor-operation-menu-items m)))

  ;; Just a few extra key bindings:
  (let* ([k (send repl-buffer get-keymap)]
	 [mouse-paste (lambda (edit event) 
			(when (send event button-down?)
			  (send edit set-position (send edit last-position))
			  (send edit paste)))])
    (wx:add-text-keymap-functions k)
    (send k add-mouse-function "mouse-paste" mouse-paste)
    (map
     (lambda (key func) (send k map-function key func))
     (append
      (case (system-type)
	[(windows) '("c:c" "c:x" "c:v" "c:k")]
	[(macos) '("d:c" "d:x" "d:v" "d:k")]
	[(unix) '("m:w" "c:w" "c:y" "c:k")])
      '("middlebutton"))
     '("copy-clipboard" "cut-clipboard" "paste-clipboard" "delete-to-end-of-line" "mouse-paste")))
  (send repl-buffer auto-wrap #t)

  ;; Go
  ((in-parameterization user-parameterization current-output-port) user-output-port)
  ((in-parameterization user-parameterization current-error-port) user-output-port)
  ((in-parameterization user-parameterization current-input-port) (make-input-port (lambda () eof) void void))
  ((in-parameterization user-parameterization current-custodian) user-custodian)
  (send repl-display-canvas set-editor repl-buffer)
  (send frame show #t)

  (send repl-display-canvas focus)

  (wx:yield waiting))

(define box-width 300)
(define (no-stretch a) (send a stretchable-width #f) (send a stretchable-height #f))

(define message-box
  (case-lambda
   [(title message) (message-box title message #f '(ok))]
   [(title message parent) (message-box title message parent '(ok))]
   [(title message parent style)
    (check-string 'message-box title)
    (check-string/false 'message-box message)
    (check-top-level-parent/false 'message-box parent)
    (check-style 'message-box '(ok ok-cancel yes-no) null style)

    (let* ([f (make-object dialog% title parent box-width)]
	   [result 'ok]
	   [strings (let loop ([s message])
		      (let ([m (regexp-match (let ([nl (string #\newline #\return)])
					       (format "([^~a]*)[~a](.*)" nl nl))
					     s)])
			(if m
			    (cons (cadr m) (loop (caddr m)))
			    (list s))))])
      (if (and (< (length strings) 10) (andmap (lambda (s) (< (string-length s) 60)) strings))
	  (begin
	    (send f set-alignment (if (= (length strings) 1) 'center 'left) 'top)
	    (for-each (lambda (s) (make-object message% s f)) strings)
	    (send f stretchable-width #f)
	    (send f stretchable-height #f))
	  (let ([m (make-object text-field% #f f void "" '(multiple))])
	    (send m set-value message)
	    (send (send m get-editor) lock #t)))
      (let* ([p (make-object horizontal-pane% f)]
	     [mk-button (lambda (title v default?) 
			  (let ([b (make-object button% title p (lambda (b e) (set! result v) (send f show #f))
						(if default? '(border) null))])
			    (when default? (send b focus))))])
	(send p stretchable-height #f)
	(send p stretchable-width #f)
	(case (car style)
	  [(ok) (mk-button "&Ok" 'ok #t)]
	  [(ok-cancel) (mk-button "&Cancel" 'cancel #f)
		       (mk-button "&Ok" 'ok #t)]
	  [(yes-no) (mk-button "&Yes" 'yes #f)
		    (mk-button "&No" 'no #f)]))
      (send f center)
      (send f show #t)
      result)]))

(define get-ps-setup-from-user
  (case-lambda
   [() (get-ps-setup-from-user #f #f #f null)]
   [(message) (get-ps-setup-from-user message #f #f null)]
   [(message parent) (get-ps-setup-from-user message parent #f null)]
   [(message parent pss) (get-ps-setup-from-user message parent pss null)]
   [(message parent pss-in style)
    (define _
      (begin
	(check-string/false 'get-ps-setup-from-user message)
	(check-top-level-parent/false 'get-ps-setup-from-user parent)
	(check-instance 'get-ps-setup-from-user wx:ps-setup% 'ps-setup #t pss-in)
	(check-style 'get-ps-setup-from-user #f null style)))
    
    (define pss (or pss-in (wx:current-ps-setup)))
    (define f (make-object dialog% "PostScript Setup" parent))
    (define papers 
      '("A4 210 x 297 mm" "A3 297 x 420 mm" "Letter 8 1/2 x 11 in" "Legal 8 1/2 x 14 in"))
    (define p (make-object horizontal-pane% f))
    (define paper (make-object choice% #f papers p void))
    (define _0 (make-object vertical-pane% p))
    (define ok (make-object button% "Ok" p (lambda (b e) (done #t)) '(border)))
    (define cancel (make-object button% "Cancel" p (lambda (b e) (done #f))))
    (define unix? (eq? (system-type) 'unix))
    (define dp (make-object horizontal-pane% f))
    (define orientation (make-object radio-box% "Orientation:" '("Portrait" "Landscape") dp void))
    (define destination (and unix? (make-object radio-box% "Destination:" 
						'("Printer" "Preview" "File") dp void)))
    (define cp (and unix? (make-object horizontal-pane% f)))
    (define command (and unix? (make-object text-field% "Printer Command:" cp void)))
    (define options (and unix? (make-object text-field% "Printer Options:" cp void)))

    (define ssp (make-object horizontal-pane% f))
    (define sp (make-object vertical-pane% ssp))
    (define def-scale "100.00")
    (define def-offset "0000.00")
    (define xscale (make-object text-field% "Horizontal Scale:" sp void def-scale))
    (define xoffset (make-object text-field% "Horizontal Translation:" sp void def-offset))
    (define sp2 (make-object vertical-pane% ssp))
    (define yscale (make-object text-field% "Vertical Scale:" sp2 void def-scale))
    (define yoffset (make-object text-field% "Vertical Translation:" sp2 void def-offset))

    (define l2 (make-object check-box% "PostScript Level 2" f void))

    (define (done ok?)
      (send f show #f)
      (set! ok ok?))

    (define-values (xsb ysb xtb ytb) (values (box 0) (box 0) (box 0) (box 0)))

    (send paper set-selection (or (find-pos papers (send pss get-paper-name) equal?) 0))
    (send orientation set-selection (if (eq? (send pss get-orientation) 'vertical) 1 0))
    (when unix?
      (send destination set-selection (case (send pss get-mode)
					[(printer) 0] [(preview) 1] [(file) 2]))
      (send command set-value (send pss get-command))
      (send options set-value (send pss get-options)))

    (send sp set-alignment 'right 'top)
    (send sp2 set-alignment 'right 'top)
    (send pss get-scaling xsb ysb)
    (send xscale set-value (number->string (unbox xsb)))
    (send yscale set-value (number->string (unbox ysb)))
    (send pss get-translation xtb ytb)
    (send xoffset set-value (number->string (unbox xtb)))
    (send yoffset set-value (number->string (unbox ytb)))
    (send xscale stretchable-width #f)
    (send yscale stretchable-width #f)
    (send xoffset stretchable-width #f)
    (send yoffset stretchable-width #f)

    (send l2 set-value (send pss get-level-2))

    (send f set-alignment 'center 'top)

    (map no-stretch (list f xscale yscale xoffset yoffset dp))

    (send f center)

    (send f show #t)

    (if ok
	(let ([s (make-object wx:ps-setup%)]
	      [gv (lambda (c b)
		    (or (string->number (send c get-value)) (unbox b)))])
	  (send s set-paper-name (send paper get-string-selection))
	  (send s set-orientation (if (positive? (send orientation get-selection))
				      'landscape
				      'portrait))
	  (when unix?
	    (send s set-mode (case (send destination get-selection)
			       [(0) 'printer]
			       [(1) 'preview]
			       [(2) 'file])))
	  (send s set-scaling (gv xscale xsb) (gv yscale ysb))
	  (send s set-translation (gv xoffset xtb) (gv yoffset ytb))
	  (send s set-level-2 (send l2 get-value))
	  
	  (when (eq? (system-type) 'unix)
	    (send s set-command (send command get-value))
	    (send s set-options (send options get-value)))

	  s)
	#f)]))

(define get-text-from-user
  (case-lambda
   [(title message) (get-text-from-user title message #f "" null)]
   [(title message parent) (get-text-from-user title message parent "" null)]
   [(title message parent init-val) (get-text-from-user title message parent init-val null)]
   [(title message parent init-val style)
    (check-string 'get-text-from-user title)
    (check-string/false 'get-text-from-user message)
    (check-top-level-parent/false 'get-text-from-user parent)
    (check-string 'get-text-from-user init-val)
    (check-style 'get-text-from-user #f null style)
    (let* ([f (make-object dialog% title parent box-width)]
	   [ok? #f]
	   [done (lambda (?) (lambda (b e) (set! ok? ?) (send f show #f)))])
      (send f set-label-position 'vertical)
      (let ([t (make-object text-field% message f (lambda (t e) (when (eq? (send e get-event-type) 'text-field-enter)
								  ((done #t) #f #f)))
			    init-val)]
	    [p (make-object horizontal-pane% f)])
	(send p set-alignment 'right 'center)
	(send p stretchable-height #f)
	(make-object button% "Cancel" p (done #f))
	(make-object button% "Ok" p (done #t) '(border))
	(send t focus)
	(send f show #t)
	(and ok? (send t get-value))))]))

(define get-choice-from-user
  (case-lambda
   [(title message choices) (get-choice-from-user title message choices null #f '(single))]
   [(title message choices parent) (get-choice-from-user title message choices parent null '(single))]
   [(title message choices parent init-vals) (get-choice-from-user title message choices parent init-vals '(single))]
   [(title message choices parent init-vals style)
    (check-string 'get-choice-from-user title)
    (check-string/false 'get-choice-from-user message)
    (unless (andmap string? choices)
      (raise-type-error 'get-choice-from-user parent "list of strings" choices))
    (check-top-level-parent/false 'get-choice-from-user parent)
    (check-style 'get-choice-from-user '(single multiple extended) null style)
    (let* ([f (make-object dialog% title parent box-width)]
	   [ok-button #f]
	   [update-ok (lambda (l) (send ok-button enable (not (null? (send l get-selections)))))]
	   [ok? #f]
	   [done (lambda (?) (lambda (b e) (set! ok? ?) (send f show #f)))])
      (send f set-label-position 'vertical)
      (let ([l (make-object list-box% message choices f
			    (lambda (l e)
			      (update-ok l)
			      (when (eq? (send e get-event-type) 'list-box-dclick)
				((done #t) #f #f)))
			    style)]
	    [p (make-object horizontal-pane% f)])
	(for-each (lambda (i) (send l select i #t)) init-vals)
	(send p set-alignment 'right 'center)
	(send p stretchable-height #f)
	(make-object button% "Cancel" p (done #f))
	(set! ok-button (make-object button% "Ok" p (done #t) '(border)))
	(update-ok l)
	(send f show #t)
	(and ok? (send l get-selections))))]))

(define last-visted-directory #f)

(define (mk-file-selector who put?)
  (letrec ([sel
	    (case-lambda
	     [() (sel #f #f #f #f #f null)]
	     [(message) (sel message #f #f #f #f null)]
	     [(message parent) (sel message parent #f #f #f null)]
	     [(message parent directory) (sel message parent directory #f #f null)]
	     [(message parent directory filename) (sel message parent directory filename #f null)]
	     [(message parent directory filename extension) (sel message parent directory filename extension null)]
	     [(message parent directory filename extension style)
	      (check-string/false who message)
	      (check-top-level-parent/false who parent)
	      (check-string/false who directory) (check-string/false who filename) (check-string/false who extension)
	      (check-style who #f null style)
	      (if (not (eq? (system-type) 'unix))
		  (wx:file-selector message directory filename extension "*.*" (if put? 'put 'get) parent)
		  (letrec ([ok? #t]
			   [typed-name #f]
			   [dir (or directory last-visted-directory (current-directory))]
			   [f (make-object dialog% (if put? "Put File" "Get File") parent 500 300)]
			   [__ (when message
				 (let ([p (make-object vertical-pane% f)])
				   (send p stretchable-height #f)
				   (make-object message% message p)))]
			   [m (make-object message% dir f)]
			   [lp (make-object horizontal-pane% f)]
			   [dirs (make-object list-box% #f null lp (lambda (d e)
								     (when (eq? (send e get-event-type) 'list-box-dclick)
								       (let ([sd (send d get-string-selection)])
									 (set! dir (simplify-path (build-path dir sd)))
									 (reset-directory)))))]
			   [files (make-object list-box% #f null lp (lambda (d e)
								      (update-ok)
								      (when (eq? (send e get-event-type) 'list-box-dclick)
									(done))))]
			   [do-text-name (lambda ()
					   (let ([v (send dir-text get-value)])
					     (if (directory-exists? v)
						 (begin
						   (set! dir v)
						   (reset-directory))
						 ; Maybe specifies a file:
						 (let-values ([(super file) 
							       (with-handlers ([void #f])
								 (let-values ([(base name dir?) (split-path v)])
								   (let ([super (and (not dir?) 
										     (or (and (string? base) 
											      (directory-exists? base)
											      base)
											 (and (eq? base 'relative) 
											      (directory-exists? dir) dir)))])
								     (if super
									 (values super name)
									 (values #f #f)))))])
						   (if super
						       (begin
							 (set! dir super)
							 (set! typed-name file)
							 (done))
						       (begin
							 (set! dir v)
							 (reset-directory)))))))]
			   [dir-text (make-object text-field% #f f (lambda (t e)
								     (if (eq? (send e get-event-type) 'text-field-enter)
									 (do-text-name)
									 (begin
									   ; typing in the box; disable the file list and enable ok
									   (send files enable #f)
									   (send ok-button enable #t)))))]
			   [bp (make-object horizontal-pane% f)]
			   [dot-check (make-object check-box% "Show files/directories that start with \".\"" bp (lambda (b e) (reset-directory)))]
			   [spacer (make-object vertical-pane% bp)]
			   [cancel-button (make-object button% "&Cancel" bp (lambda (b e) (set! ok? #f) (send f show #f)))]
			   [ok-button (make-object button% "&Ok" bp (lambda (b e) 
								      (if (send files is-enabled?)
									  (done) ; normal mode
									  (do-text-name))) ; handle typed text
						   '(border))]
			   [update-ok (lambda () (send ok-button enable (not (null? (send files get-selections)))))]
			   [reset-directory (lambda ()
					      (wx:begin-busy-cursor)
					      (send m set-label (if (directory-exists? dir)
								    (begin
								      (set! last-visted-directory dir)
								      dir)
								    (string-append "BAD DIRECTORY: " dir)))
					      (send dir-text set-value dir)
					      (let ([l (with-handlers ([void (lambda (x) null)])
							 (directory-list dir))]
						    [dot? (send dot-check get-value)])
						(letrec ([sort (lambda (l)
								 (if (or (null? l) (null? (cdr l)))
								     l
								     (let-values ([(l1 l2) (split l null null)])
								       (merge (sort l1) (sort l2)))))]
							 [split (lambda (l l1 l2)
								  (cond
								   [(null? l) (values l1 l2)]
								   [(null? (cdr l)) (values (cons (car l) l1) l2)]
								   [else (split (cddr l) (cons (car l) l1) (cons (cadr l) l2))]))]
							 [merge (lambda (l1 l2)
								  (cond
								   [(null? l1) l2]
								   [(null? l2) l1]
								   [(string<? (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2))]
								   [else (merge l2 l1)]))])
						  (let-values ([(ds fs)
								(let loop ([l l][ds null][fs null])
								  (cond
								   [(null? l) (values (cons ".." (sort (reverse! ds))) (sort (reverse! fs)))]
								   [(and (not dot?) (char=? (string-ref (car l) 0) #\.)) (loop (cdr l) ds fs)]
								   [(file-exists? (build-path dir (car l))) (loop (cdr l) ds (cons (car l) fs))]
								   [else (loop (cdr l) (cons (car l) ds) fs)]))])
						    (send dirs set ds)
						    (send files set fs)
						    (send files enable #t)
						    (update-ok)
						    (wx:end-busy-cursor)))))]
			   [get-filename (lambda () (and ok? (simplify-path (build-path dir (or typed-name (send files get-string-selection))))))]
			   [done (lambda ()
				   (let ([name (get-filename)])
				     (unless (and put? (file-exists? name)
						  (eq? (message-box "Warning" (format "Replace ~s?" name) f '(yes-no)) 'no)
						  (set! typed-name #f))
				       (send f show #f))))])
		    (send bp stretchable-height #f)
		    (send m stretchable-width #t)
		    (reset-directory)
		    (send f center)
		    (send f show #t)
		    (get-filename)))])])
    sel))

(define get-file (mk-file-selector 'get-file #f))
(define put-file (mk-file-selector 'put-file #t))

(define get-color-from-user 
  (if (not (eq? (system-type) 'unix))
      wx:get-color-from-user
      (case-lambda
       [() (get-color-from-user #f #f #f null)]
       [(message) (get-color-from-user message #f #f null)]
       [(message parent) (get-color-from-user message parent #f null)]
       [(message parent color) (get-color-from-user message parent #f null)]
       [(message parent color style)
	(check-string/false 'get-color-from-user message)
	(check-top-level-parent/false 'get-color-from-user parent)
	(check-instance 'get-color-from-user wx:color% 'color #t color)
	(check-style 'get-color-from-user #f null style)
	(let* ([ok? #t]
	       [f (make-object dialog% "Choose Color" parent)]
	       [done (lambda (ok) (lambda (b e) (set! ok? ok) (send f show #f)))]
	       [p (make-object vertical-pane% f)]
	       [make-color-slider (lambda (l) (make-object slider% l 0 255 p void))]
	       [red (make-color-slider "Red:")]
	       [green (make-color-slider "Green:")]
	       [blue (make-color-slider "Blue:")]
	       [bp (make-object horizontal-pane% f)])
	  (when color
	    (send red set-value (send color red))
	    (send green set-value (send color green))
	    (send blue set-value (send color blue)))
	  (make-object button% "Cancel" bp (done #f))
	  (make-object button% "Ok" bp (done #t) '(border))
	  (send bp set-alignment 'right 'center)
	  (send p set-alignment 'right 'center)
	  (send f show #t)
	  (and ok?
	       (make-object wx:color% 
                            (send red get-value)
                            (send green get-value)
			    (send blue get-value))))])))

(define get-font-from-user 
  (if (eq? (system-type) 'windows)
      wx:get-font-from-user
      (case-lambda
       [() (get-font-from-user #f #f #f null)]
       [(message) (get-font-from-user message #f #f null)]
       [(message parent) (get-font-from-user message parent #f null)]
       [(message parent font) (get-font-from-user message parent #f null)]
       [(message parent font style)
	(check-string/false 'get-font-from-user message)
	(check-top-level-parent/false 'get-font-from-user parent)
	(check-instance 'get-color-from-user wx:font% 'font #t font)
	(check-style 'get-font-from-user #f null style)
	(letrec ([ok? #f]
		 [f (make-object dialog% "Choose Font" parent 500 300)]
		 [refresh-sample (lambda (b e) (let ([f (get-font)])
						 (send ok-button enable f)
						 (when f
						   (let ([s (send (send edit get-style-list) find-named-style "Standard")])
						     (send s set-delta (font->delta f))))))]
		 [p (make-object horizontal-pane% f)]
		 [face (make-object list-box% "Font:" (wx:get-face-list) p refresh-sample)]
		 [p2 (make-object vertical-pane% p)]
		 [style (make-object radio-box% "Style:" '("Normal" "Italic" "Slant") p2 refresh-sample)]
		 [weight (make-object radio-box% "Weight:" '("Normal" "Bold" "Light") p2 refresh-sample)]
		 [underlined (make-object check-box% "Underlined" p2 refresh-sample)]
		 [size (make-object slider% "Size:" 4 127 p2 refresh-sample 12)]
		 [sample (make-object text-field% "Sample" f void "The quick brown fox jumped over the lazy dog" '(multiple))]
		 [edit (send sample get-editor)]
		 [done (lambda (ok) (lambda (b e) (set! ok? ok) (send f show #f)))]
		 [get-font (lambda () (let ([face (send face get-string-selection)])
					(and face
					     (make-object wx:font% (send size get-value) face 'default
							  (case (send style get-selection) [(0) 'normal] [(1) 'italic] [(2) 'slant])
							  (case (send weight get-selection) [(0) 'normal] [(1) 'bold] [(2) 'light])
							  (send underlined get-value)))))]
		 [bp (make-object horizontal-pane% f)]
		 [cancel-button (make-object button% "Cancel" bp (done #f))]
		 [ok-button (make-object button% "Ok" bp (done #t) '(border))])
	  (when font
	    (let ([f (send face find-string (send font get-face))])
	      (and f (>= f 0) (send face set-selection f)))
	    (send style set-selection (case (send font get-style) [(normal) 0] [(italic) 1] [(slant) 2]))
	    (send weight set-selection (case (send font get-weight) [(normal) 0] [(bold) 1] [(light) 2]))
	    (send underlined set-value (send font get-underlined))
	    (send size set-value (send font get-point-size)))
	  (send bp set-alignment 'right 'center)
	  (refresh-sample (void) (void))
	  (send f show #t)
	  (and ok? (get-font)))])))

(define (play-sound f async?)
  (if (not (eq? (system-type) 'unix))
      (wx:play-sound f async?)
      (begin
	(unless (string? f)
	  (raise-type-error 'play-sound "string" f))
	(let ([b (box "cat ~s > /dev/audio")])
	  (wx:get-resource "mred" "playcmd" b)
	  ((if async? (lambda (x) (process x) #t) system)
	   (format (unbox b) (expand-path f)))))))

(define (get-display-size)
  (let ([xb (box 0)]
	[yb (box 0)])
    (wx:display-size xb yb)
    (values (unbox xb) (unbox yb))))

(define (find-item-frame item)
  (let loop ([i item])
    (let ([p (send i get-parent)])
      (cond
       [(not p) #f]
       [(is-a? p menu%) (loop (send p get-item))]
       [else (send p get-frame)]))))

(define (append-editor-operation-menu-items m)
  (check-instance 'append-editor-operation-menu-items menu% 'menu #f m)
  (let ([mk (lambda (name key op)
	      (make-object menu-item% name m
			   (lambda (i e)
			     (let* ([f (find-item-frame i)]
				    [o (and f (send f get-edit-target-object))])
			       (and o (is-a? o wx:editor<%>)
				    (send o do-edit-operation op))))
			   key))]
	[mk-sep (lambda () (make-object separator-menu-item% m))])
    (mk "Undo" #\z 'undo)
    (mk "Redo" #f 'redo)
    (mk-sep)
    (mk "Clear" #f 'clear)
    (mk "Copy" #\c 'copy)
    (mk "Cut" #\x 'cut)
    (mk "Paste" #\v 'paste)
    (mk-sep)
    (mk "Insert Text Box" #f 'insert-text-box)
    (mk "Insert Pasteboard Box" #f 'insert-pasteboard-box)
    (mk "Insert Image..." #f 'insert-image)
    (void)))

(define (append-editor-font-menu-items m)
  (check-instance 'append-editor-font-menu-items menu% 'menu #f m)
  (let ([mk (lambda (name m cb)
	      (make-object menu-item% name m
			   (lambda (i e)
			     (let* ([f (find-item-frame i)]
				    [o (and f (send f get-edit-target-object))])
			       (and o (is-a? o wx:editor<%>)
				    (cb o))))))]
	[mk-sep (lambda (m) (make-object separator-menu-item% m))]
	[mk-menu (lambda (name) (make-object menu% name m))])
    (let ([family (mk-menu "Font")]
	  [size (mk-menu "Size")]
	  [style (mk-menu "Style")]
	  [weight (mk-menu "Weight")]
	  [underline (mk-menu "Underline")]
	  [alignment (mk-menu "Alignment")]
	  [color (mk-menu "Color")]
	  [background (mk-menu "Background")])
      
      ; Font menu 
      (for-each (lambda (l f)
		  (mk l family 
		      (lambda (e)
			(send e change-style (make-object wx:style-delta% 'change-family f)))))
		'("Standard" "Decorative" "Roman" "Script" "Swiss" "Fixed")
		'(default decorative roman script swiss fixed))
      (mk-sep family)
      (mk "Choose..." family (lambda (e) (let ([f (get-font-from-user)])
					   (when f
					     (send e change-style (font->delta f))))))

      ; Size menu
      (let ([bigger (make-object menu% "Bigger" size)]
	    [smaller (make-object menu% "Smaller" size)]
	    [add-change-size
	     (lambda (m ls dss xss)
	       (for-each (lambda (l ds xs)
			   (mk l m (lambda (e)
				     (let ([d (make-object wx:style-delta%)])
				       (send d set-size-add ds)
				       (send d set-size-mult xs)
				       (send e change-style d)))))
			 ls dss xss))])
	(add-change-size bigger
			 '("+1" "+2" "+4" "+8" "+16" "+32")
			 '(1 2 4 8 16 32)
			 '(1 1 1 1 1  1))
	(mk-sep bigger)
	(add-change-size bigger
			 '("x2" "x3" "x4" "x5")
			 '(0    0    0    0)
			 '(2    3    4    5))

	(add-change-size smaller
			 '("-1" "-2" "-4" "-8" "-16" "-32")
			 '(1 -2 -4 -8 -16 -32)
			 '(1 1   1  1  1  1))
	(mk-sep smaller)
	(add-change-size smaller
			 '("/2" "/3" "/5" "/5")
			 '(0    0    0    0)
			 '(#i1/2 #i1/3 #i1/4 #i1/5))
	
	(for-each (lambda (s)
		    (mk (number->string s) size (lambda (e)
						  (let ([d (make-object wx:style-delta%)])
						    (send d set-size-add s)
						    (send d set-size-mult 0)
						    (send e change-style d)))))
		  '(9 10 12 14 16 24 32 48)))

      
      (let ([mk-cg (lambda (cmd arg)
		     (lambda (e) (send e change-style (make-object wx:style-delta% cmd arg))))])

      ; Style
      (for-each (lambda (name s)
		  (mk name style (mk-cg 'change-style s)))
		'("Normal" "Italic" "Slant")
		'(normal italic slant))
      
      ; Weight
      (for-each (lambda (name s)
		  (mk name weight (mk-cg 'change-weight s)))
		'("Normal" "Bold" "Light")
		'(normal bold light))
      
      ; Underline
      (mk "No Underline" underline (mk-cg 'change-underline #f))
      (mk "Underline" underline (mk-cg 'change-underline #t))
      (mk "Toggle" underline (lambda (e) (send e change-style (make-object wx:style-delta% 'change-toggle-underline))))

      ; Alignment
      (for-each (lambda (name s)
		  (mk name alignment (mk-cg 'change-weight s)))
		'("Top" "Center" "Bottom")
		'(top center bottom))

      (let ([colors '("Black" "White" "Red" "Orange" "Yellow" "Green" "Blue" "Purple" "Cyan" "Magenta" "Grey")])

	; Colors
	(for-each (lambda (c)
		    (mk c color (lambda (e) (let ([d (make-object wx:style-delta%)])
					      (send d set-delta-foreground c)
					      (send e change-style d)))))
		  colors)

	; Background
	(mk "Transparent" background (lambda (e) (let ([d (make-object wx:style-delta%)])
						   (send d set-transparent-text-backing-on #t)
						   (send e change-style d))))
	(for-each (lambda (c)
		    (mk c background (lambda (e) (let ([d (make-object wx:style-delta%)])
						   (send d set-delta-background c)
						   (send e change-style d)))))
		  colors))))))

(define (who->name who)
  (cond
   [(symbol? who) who]
   [(eq? (car who) 'method) (string->symbol (format "~a in ~a" (caddr who) (cadr who)))]
   [else (constructor-name (cadr who))]))

(define (check-instance who class class-name false-ok? v)
  (unless (or (and false-ok? (not v)) (is-a? v class))
    (raise-type-error (who->name who) (format "~a% object~a" class-name (if false-ok? " or #f" "")) v)))

(define (check-string/false who str)
  (unless (or (not str) (string? str))
    (raise-type-error (who->name who) "string or #f" str)))

(define (check-string who str)
  (unless (string? str)
    (raise-type-error (who->name who) "string" str)))

(define (check-char/false who c)
  (unless (or (not c) (char? c))
    (raise-type-error (who->name who) "character or #f" c)))

(define (check-callback who callback)
  (unless (and (procedure? callback)
	       (procedure-arity-includes? callback 2))
    (raise-type-error (who->name who) "procedure of arity 2" callback)))

(define (check-range-integer who range)
  (unless (and (number? range) (integer? range) (<= 0 range 10000))
    (raise-type-error (who->name who) "integer in [0, 10000]" range)))

(define (check-slider-integer who range)
  (unless (and (number? range) (integer? range) (<= -10000 range 10000))
    (raise-type-error (who->name who) "integer in [-10000, 10000]" range)))

(define (check-dimension who d)
  (when d (check-range-integer who d)))

(define (check-string-or-bitmap who label)
  (unless (or (string? label) (is-a? label wx:bitmap%))
    (raise-type-error (who->name who) "string or bitmap% object" label)))

(define (check-style who reqd other-allowed style)
  (unless (and (list? style) (andmap symbol? style))
    (raise-type-error (who->name who) "list of style symbols" style))
  (when reqd
    (unless (ormap (lambda (i) (memq i reqd)) style)
      (raise-type-error (who->name who)
			(format "style list containing ~a"
				(if (= (length reqd) 1)
				    (car reqd)
				    (string-append
				     "one of "
				     (let loop ([l reqd])
				       (if (null? (cdr l))
					   (format "or ~a" (car l))
					   (format "~a, ~a" (car l) (loop (cdr l))))))))
			style)))
  (if (and (not reqd) (null? other-allowed))
      (unless (null? style)
	(raise-type-error (who->name who) "empty style list" style))
      (let* ([l (append (or reqd null) other-allowed)]
	     [bad (ormap (lambda (x) (if (memq x l) #f x)) style)])
	(when bad
	  (raise-type-error (who->name who) (format "style list, ~a not allowed" bad) style))
	(let loop ([l style])
	  (unless (null? l)
	    (when (memq (car l) (cdr l))
	      (raise-type-error (who->name who) (format "style list, ~a allowed only once" (car l)) style))
	    (loop (cdr l)))))))

  
