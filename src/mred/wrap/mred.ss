

; default spacing between items.
(define const-default-spacing 0)

; default margins:
(define const-default-x-margin 2)
(define const-default-y-margin 2)

; default spacing around edge of panel
(define const-default-border 0)

; the maximum hard-min-width of a gauge
(define const-max-gauge-length 150)

; this structure holds the information that a child will need to send
; to its parent when the parent must resize itself.
;  x-posn/y-posn: numbers which indicate the default position of the
;    child. 
;  x-min/y-min: numbers which indicate the minimum size of the child.
;  x-stretch/y-stretch: booleans which indicate whether the child can
;    stretch in the appropriate direction to fill otherwise empty
;    space. 
(define-struct child-info (x-posn y-posn 
				  x-min y-min ; includes margins!
				  x-margin y-margin
				  x-stretch y-stretch))

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

(define max-min 10000)

(define (check-reasonable-min who v)
  (unless (<= 1 v max-min)
    (error who "not a reasaonable minimum width: ~a" v)))

(define (check-reasonable-margin who v)
  (unless (<= 0 v 1000)
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



(define (make-container% %)
  (class % args
    (private
      [focus this])
    (public
      [set-focused-window
       (lambda (w)
	 (set! focus w))]
      [get-focused-window
       (lambda () focus)])
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
     (class item% args
       (rename [super-on-set-focus on-set-focus]
	       [super-on-kill-focus on-kill-focus])
       (private
	 [top-level #f]
	 [get-top-level
	  (lambda ()
	    (unless top-level
	      (let loop ([window this])
		(cond
		 [(or (is-a? window wx:frame%)
		      (is-a? window wx:dialog-box%)) 
		  (set! top-level window)]
		 [else (loop (send window get-parent))])))
	    top-level)])
       (public
	 [on-set-focus
	  (lambda ()
	    (send (get-frame) set-focused-window this)
	    (super-on-set-focus))]
	 [on-kill-focus
	  (lambda ()
	    (send (get-frame) set-focused-window #f)
	    (super-on-kill-focus))])
       (inherit get-width get-height get-x get-y
		get-parent get-client-size get-size)
       (rename [super-enable enable])
       (private [enabled? #t])
       (public
	 [enable
	  (lambda (b)
	    (set! enabled? (and b #t))
	    (apply super-enable x))]
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
		  (get-client-size cw-box ch-box)
		  (set! h (- (get-height) (unbox h)))
		  (set! w (- (get-width) (unbox w)))))
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
		    (unless (eq? v2 v)
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
	  
	  ; force-redraw: unconditionally trigger redraw.
	  ; input: none
	  ; returns: nothing
	  ; effects: forces the item's parent (if it exists) to redraw
	  ;   itself. This will recompute the min-size cache if it is
	  ;   invalid.
	  [force-redraw
	   (lambda ()
	     (let ([parent (get-parent)])
	       (unless parent
		 (send parent child-redraw-request this))))]
	  
	  ; set-size: caches calls to set-size to avoid unnecessary work.
	  ; input: x/y: new position for object
	  ;        width/height: new size for object
	  ; returns: nothing
	  ; effect: if arguments mark a different geometry than the object's
	  ;   current geometry, passes args to super-class's set-size.
	  ;   Otherwise, does nothing.
	  [set-size
	   (lambda (x y width height)
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
	   (apply super-init args)
	   (set-min-width (get-width))
	   (set-min-height (get-height))

	   (send (get-parent) add-child this))))))

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

(define (make-window-glue% %)
  (class % (mred . args)
    (rename [super-on-size on-size]
	    [super-on-set-focus on-set-focus]
	    [super-on-kill-focus on-kill-focus]
	    [super-pre-on-char pre-on-char]
	    [super-pre-on-event pre-on-event])
    (public
      [get-mred (lambda () mred)]
      [on-size (lambda (x y)
		 (super-on-size x y)
		 (send mred on-size x y))]
      [on-set-focus (lambda ()
		      (super-on-set-focus)
		      (send mred on-focus #t))]
      [on-kill-focus (lambda ()
		      (super-on-kill-focus)
		      (send mred on-focus #f))]
      [pre-on-char (lambda (w e)
		     (super-pre-on-char w e)
		     (send mred pre-on-char w e))]
      [pre-on-event (lambda (w e)
		      (super-pre-on-event w e)
		      (send mred pre-on-event w e))])
    (sequence (apply super-init args))))

(define (make-top-level-window-glue% %)
  (class (make-window-glue% %) (mred . args)
    (rename [super-on-activate on-activate])
    (public
      [on-activate (lambda (on?)
		     (super-on-activate on?)
		     (send mred on-activate on?))])
    (sequence (apply super-init mred args))))

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

      (if (eq? 'horizontal style)
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
     (inherit number)
     (rename [super-enable enable]
	     [super-is-enabled? is-enabled?])
     (public
       [enable
	(case-lambda
	 [(on?) (super-enable on?)]
	 [(which on?) (when (< -1 which (number))
			(vector-set! enable-vector which (and on? #t)))])]
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
       
       (let-values ([(client-w client-h)
		     (get-two-int-values get-client-size)])
	 (let ([range (* pixels-per-value (add1 (- max-val min-val)))]
	       [horizontal? (eq? 'horizontal style)])
	   ((if horizonal? set-min-width set-max-width) (min const-max-gauge-length range))))))))

(define wx-canvas% (make-control% wx:canvas% 0 0 #t #t))
(define wx-media-canvas% (make-control% wx:media-canvas% 0 0 #t #t))

(define (make-panel-glue% %)
  (class (make-window-glue% %) (mred . args)
    (public
      [place-children (lambda (l) (send mred place-children l))])
    (sequence
      (apply super-init mred args))))

(define wx-panel%
 (make-panel-glue% 
  (class (make-container% (make-item% wx:panel% 0 0 #t #t)) (parent style)
    (inherit get-x get-y get-width get-height
	     min-width min-height
	     x-margin y-margin
	     get-client-size get-parent)
    
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
	 (unless (eq? this (send new-child get-parent))
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
			     (eq? this (send child get-parent)))
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
			    (let loop ([l child-list])
			      (cond
			       [(null? l) l]
			       [(eq? (car l) child) (cdr l)]
			       [else (cons (car l) (loop (cdr l)))])))))]
      
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
	 (let ([parent (get-parent)])
	   (send parent child-redraw-request this)))]

      ; make-get-graphical-size: creates a function which returns the minimum
      ;   possible size for a horizontal-panel% or vertical-panel% object.
      ; input: compute-x/compute-y: functions which take the current x/y
      ;          location, the amount of spacing which will come after the
      ;          current object, and the list of child-info structs beginning
      ;          with the current object, and return the new x/y locations.
      ; returns: a thunk which returns the minimum possible size of the
      ;   entire panel (not just client) as a list of two elements:
      ;   (min-x min-y). 
      [make-get-graphical-size
	(lambda (compute-x compute-y)
	  (letrec ([gms-help
		    (lambda (kid-info x-accum y-accum)
		      (if (null? kid-info)
			  (list x-accum y-accum)
			  (gms-help
			   (cdr kid-info)
			   (compute-x x-accum kid-info)
			   (compute-y y-accum kid-info))))])
	    (lambda ()
	      (let-values ([(client-w client-h)
			    (get-two-int-values get-client-size)])
		(let* ([border (border)]
		       [min-client-size
			(gms-help (get-children-info)
				  (* 2 border) (* 2 border))]
		       [delta-w (- (send container get-width) client-w)]
		       [delta-h (- (send container get-height) client-h)])
		  (list (+ delta-w (car min-client-size))
			(+ delta-h (cadr min-client-size))))))))]
      
      ; get-min-graphical-size: poll children and return minimum possible
      ;   size, as required by the graphical representation of the tree,
      ;   of the panel.
      ; input: none
      ; returns: minimum full size (as a list, width & height) of the
      ;   container.
      ; effects: none
      [get-graphical-min-size
       (make-get-graphical-size 
	(lambda (x-accum kid-info)
	  (max x-accum (+ (* 2 (border))
			  (child-info-x-min (car kid-info)))))
	(lambda (y-accum kid-info)
	  (max y-accum (+ (* 2 (border))
			  (child-info-y-min (car kid-info))))))]
      
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
      ; input: children-info: list of mred:child-info structs
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
		  (loop (cdr children-info) width height))))))]
      
      ; redraw: redraws panel and all children
      ; input: width, height: size of drawable area in panel.
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
      (super-init parent -1 -1 -1 -1 style)))))


; make-place-children: creates place-children functions for
; horizontal-panel% or vertical-panel% classes.
; input: container: pointer to the panel% object which uses
;          the resulting function.
;        child-major-size: function which takes a child-info struct
;          and returns the child's minimum size in the major direction
;          of the panel.
;        child-major-stretch: function which takes a mred;child-info
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
(define make-place-children
  (lambda (container child-major-size
		     child-major-stretch
		     child-major-offset
		     child-minor-size
		     child-minor-stretch
		     child-minor-position
		     major-dim minor-dim
		     get-x-info get-y-info)
    (lambda (kid-info width height)
      (letrec ([count-stretchable
		(lambda (kid-info)
		  (if (null? kid-info)
		      0
		      (let ([curr-info (car kid-info)])
			(if (child-major-stretch curr-info)
			    (add1 (count-stretchable (cdr kid-info)))
			    (count-stretchable (cdr kid-info))))))])
	(let* ([spacing (send container spacing)]
	       [border (send container border)]
	       [full-w (send container get-width)]
	       [full-h (send container get-height)]
	       [delta-list (list
			    (- full-w width)
			    (- full-h height))]
	       [num-stretchable (count-stretchable kid-info)]
	       [extra-space (- (major-dim width height)
			       (- (apply 
				   major-dim
				   (send container get-graphical-min-size))
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
	    
	    (pc-help kid-info (+ border major-offset) leftover)))))))

(define linear-panel%
  (class wx-panel% args
    (private
      [major-align-pos 'left]
      [minor-align-pos 'center]
      
      [child-align
       (lambda (width size align)
	 (case child-align
	   [(center) (/ (- width size) 2)]
	   [(left) 0]
	   [(right) (- width size)]))]
      [child-offset
       (lambda (total-offset align)
	 (case align
	   [(center) (/ total-offset 2)]
	   [(left) 0]
	   [(right) total-offset]))])
    
    (inherit force-redraw)
    (public
      [mk-minor-align (lambda (a) (lambda () (set! minor-align-pos 'left) (force-redraw)))]
      [mk-major-align (lambda (a) (lambda () (set! major-align-pos 'left) (force-redraw)))]
      [major-offset (lambda (space) (child-offset space major-align-pos))]
      [minor-offset (lambda (width size) (child-align width size minor-align-pos))]
      
      [default-spacing-width const-default-spacing]
      
      [spacing
       (let ([curr-spacing const-default-spacing])
	 (case-lambda
	  [() curr-spacing]
	  [(new-val)
	   (check-reasonable-margin 'spacing new-val)
	   (set! curr-spacing new-val)
	   (force-redraw)]))]
      
      [border (make-border this)])
    (sequence (apply super-init args))))

; horizontal-panel%: a panel which arranges its children in an evenly
; spaced horizontal row.  Items are vertically centered (or stretched
; to fit the dialog box if they are stretchable).  The items are evenly
; spaced horizontally, with any extra space divided evenly among the
; stretchable items. 
(define wx-horizontal-panel%
  (class-asi linear-panel%
	     (inherit mk-major-align mk-minor-align major-offset minor-offset
		      spacing border make-get-graphical-size)
	     (public
	       [minor-align-top (mk-minor-align 'left)]
	       [minor-align-center (mk-minor-align 'center)]
	       [minor-align-bottom (mk-minor-align 'right)]
	       [major-align-left (mk-major-align 'left)]
	       [major-align-center (mk-major-align 'center)]
	       [major-align-right (mk-major-align 'right)])
	     
	     (public
	       [get-graphical-min-size
		(make-get-graphical-size this
					 (lambda (x-accum kid-info)
					   (+ x-accum (child-info-x-min (car kid-info))
					      (if (null? (cdr kid-info))
						  0
						  (spacing))))
					 (lambda (y-accum kid-info)
					   (max y-accum
						(+ (child-info-y-min (car kid-info))
						   (* 2 (border))))))]
	       [do-place-children
		(make-place-children this
				     child-info-x-min
				     child-info-x-stretch
				     major-offset
				     child-info-y-min
				     child-info-y-stretch
				     minor-offset
				     (lambda (width height) width)
				     (lambda (width height) height)
				     (lambda (major minor) major)
				     (lambda (major minor) minor))])))

; vertical-panel%.  See horizontal-panel%, but reverse
; "horizontal" and "vertical."
(define wx-vertical-panel%
  (class-asi linear-panel%
	     (inherit mk-major-align mk-minor-align major-offset minor-offset
		      spacing border make-get-graphical-size)
	     (public
	       [minor-align-left (mk-minor-align 'left)]
	       [minor-align-center (mk-minor-align 'center)]
	       [minor-align-right (mk-minor-align 'right)]
	       [major-align-top (mk-major-align 'left)]
	       [major-align-center (mk-major-align 'center)]
	       [major-align-bottom (mk-major-align 'right)])
	     
	     (public
	       [get-graphical-min-size
		(make-get-graphical-size this
					 (lambda (x-accum kid-info)
					   (max x-accum
						(+ (child-info-x-min (car kid-info))
						   (* 2 (border)))))
					 (lambda (y-accum kid-info)
					   (+ y-accum (child-info-y-min (car kid-info))
					      (if (null? (cdr kid-info))
						  0
						  (spacing)))))]
	       
	       [do-place-children
		(make-place-children this
				     child-info-y-min
				     child-info-y-stretch
				     major-offset
				     child-info-x-min
				     child-info-x-stretch
				     minor-offset
				     (lambda (width height) height)
				     (lambda (width height) width)
				     (lambda (major minor) minor)
				     (lambda (major minor) major))])))

(define add-at-end
  (lambda (object)
    (lambda (list-of-kids)
      (append list-of-kids (list object)))))

; implement a panel which can hold multiple objects but only displays
; one at a time.  The size of the panel is the smallest size possible
; for displaying each of the panel's children.
(define single-panel%
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
			     (eq? this (send child get-parent)))
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
		     (eq? this (send new-child get-parent)))
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





; make-top-container%: adds the necessary functionality to wx:frame% and 
; wx:dialog-box%.
; input: base%: the base class from which to descend the new mred class.
;          Intended to be either wx:frame% or wx:dialog-box%, but can
;          be anything which contains all methods in the inherit section
;          below.
; returns: a new class, descended from base%, which possesses the added
;            capabilities necessary to serve as the frame/dialog which
;            contains mred container classes.
(define (make-top-container% base%)
  (class base% args
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
      
      [enabled? #f])
    
    (public
      [enable
       (lambda (b)
	 (set! enabled? (and b #t))
	 (apply super-enable x))]
      [is-enabled?
       (lambda () enabled?)]

      ; add-child: update panel pointer.
      ; input: new-panel: panel in frame (descendant of
      ;   mred:panel%) 
      ; returns: nothing
      ; effects: sets panel to new-panel
      ;          if new-panel is not a descendant of
      ;            mred:panel%, calls error; panel not updated.
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

(define wx-frame%
  (make-top-level-window-glue% 
   (class (make-top-container% wx:frame%) args
     (sequence
       (apply super-init args)))))

(define wx-dialog-box%
  (make-top-level-window-glue% 
   (class (make-top-container% wx:dialog-box%) args
     (sequence
       (apply super-init args)))))
  
(define window<%>
  (interface ()
   on-focus set-focus
   on-size
   pre-on-char pre-on-event
   client-to-screen screen-to-client
   enable is-enabled?
   
   get-label set-label
   get-parent
   get-client-size get-geometry
   get-width get-height get-x get-y
   get-text-extent
   
   get-cursor set-cursor 
   show is-shown?
   refresh))

(define wx-key (gensym))
(define (mred->wx w) (send w get-low-level-window wx-key))
(define (wx->mred w) (send w get-mred))

(define basic-window%
  (class* null (window<%>) (wx label cursor)
    (private
      [double-boxed
       (lambda (x y f values)
	 (let ([x (box x)][y (box y)])
	   (f x y)
	   (values (unbox x) (unbox y))))])
    (public
      [get-low-level-window (lambda (key)
			      (unless (eq? key wx-key)
				(error 'get-low-level-window "bad key"))
			      wx)]
      [on-focus void]
      [on-size void]
      [pre-on-char void]
      [pre-on-event void]

      [set-focus (ivar wx set-focus)]
      [enable (ivar wx enable)]
      [is-enabled? (ivar wx is-enabled?)]
      [get-parent (lambda ()
		    (let ([p (send wx get-parent)])
		      (and p (wx->mred p))))]

      [get-label (lambda () label)]
      [set-label (lambda (l) (set! label l))]
      
      [client-to-screen (lambda (x y)
			  (double-boxed
			   x y
			   (lambda (x y) (send wx client-to-screen x y))))]
      [screen-to-client (lambda (x y)
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

      [get-width (ivar wx get-width)]
      [get-height (ivar wx get-height)]
      [get-x (ivar wx get-x)]
      [get-y (ivar wx get-y)]

      [get-text-extent (ivar wx get-text-extent)]

      [get-cursor (lambda () cursor)]
      [set-cursor (lambda (x)
		    (send wx set-cursor x)
		    (set! cursor x))]

      [show (ivar wx show)]
      [is-shown? (ivar wx is-shown?)]

      [refresh (ivar wx refresh)])))

(define top-level-window<%>
  (interface (window<%>)
	     center
	     move
	     resize
	     on-activate
	     get-panel))

(define container-window<%>
  (interface (window<%>)))

(define basic-top-level-window%
  (class* basic-window% (top-level-window<%> container-window<%>) (wx label)
    (rename [super-set-label set-label])
    (public
      [get-panel (lambda ()
		   (let ([p (send wx get-top-panel)])
		     (and p (wx->mred p))))]
      [on-activate void]
      [center (ivar wx center)]
      [set-label (lambda (l)
		   (send wx set-title l)
		   (super-set-label))]
      [move (lambda (x y)
	      (send wx move x y))]
      [resize (lambda (w h)
		(send wx set-size -1 -1 w h))])
    (sequence (super-init wx label #f))))

(define child-window<%>
  (interface (window<%>)
    min-width min-height
    horiz-margin vert-margin
    horiz-stretchable vert-stretchable))

(define basic-child-window%
  (class* basic-window% (child-window<%>) (wx label cursor)
    (public
      [min-width (ivar wx min-width)]
      [min-height (ivar wx min-height)]
      [horiz-margin (ivar wx x-margin)]
      [vert-margin (ivar wx y-margin)]
      [horiz-stretchable (ivar wx stretchable-in-x)]
      [vert-stretchable (ivar wx stretchable-in-y)])
    (sequence (super-init wx label cursor))))

(define control<%>
  (interface (child-window<%>)))

(define (panel-parent-only who p)
  (unless (is-a? p panel%)
    (raise-type-error (string->symbol (format "~a-constructor" who))
		      "parent mred:panel% object" p)))

(define (any-legal-parent p)
  (cond
   [(is-a? p panel%) (void)]
   [(or (is-a? p frame%) (is-a? p dialog-box%))
    (when (send p get-panel)
      (error 'panel-constructor "the specified top-level window already has a panel"))]
   [else
    (raise-type-error 'panel-constructor "parent mred:panel%, mred:frame%, or mred:dialog-box% object" p)]))

(define basic-control%
  (class* basic-child-window% (control<%>) (wx label cursor)
    (rename [super-set-label set-label])
    (public
      [set-label (lambda (l)
		   (send wx set-label l)
		   (super-set-label l))])
    (sequence
      (super-init wx label cursor))))
    
(define frame%
  (class basic-top-level-window% (parent label [x #f] [y #f] [width #f] [height #f] [style null])
    (sequence
      (super-init (make-object wx-frame% this
			       (and parent (mred->wx parent)) label 
			       (or x -1) (or y -1) (or width -1) (or height -1)
			       style)
		  label))))

(define dialog-box%
  (class basic-top-level-window% (parent label [modal? #f] [x #f] [y #f] [width #f] [height #f] [style null])
    (sequence
      (super-init (make-object wx-dialog-box% this
			       (and parent (mred->wx parent)) label modal?
			       (or x -1) (or y -1) (or width -1) (or height -1)
			       style)
		  label))))

(define panel<%>
  (interface (child-window<%> container-window<%>)
    set-control-font get-control-font
    set-label-font get-label-font
    set-label-position get-label-position
    change-children place-children add-child delete-child
    border))

(define basic-panel%
  (class* basic-child-window% (panel<%>) (wx) 
    (public
      [get-control-font (ivar wx get-button-font)]
      [set-control-font (ivar wx set-button-font)]
      [get-label-font (ivar wx get-label-font)]
      [set-label-font (ivar wx set-label-font)]
      [get-label-position (ivar wx get-label-position)]
      [set-label-position (ivar wx set-label-position)]
      [border (ivar wx border)]
      [change-children (lambda (f)
			 (map mred->wx
			      (send wx change-children
				    (lambda (kids)
				      (f (mape wx->mred kids))))))]
      [place-children (ivar wx do-place-children)]
      [add-child (lambda (c) (send wx add-child (mred->wx c)))]
      [delete-child (lambda (c) (send wx delete-child (mred->wx c)))])
    (sequence
      (super-init wx #f #f))))

(define (make-a-panel% panel% wx-panel%)
  (class panel% (parent [style null])
    (sequence (any-legal-parent parent))
    (private
      [wx (make-object wx-panel% this (mred->wx parent) style)])
    (sequence
      (super-init wx))))

(define panel% (make-a-panel% basic-panel% wx-panel%))

(define linear-panel<%>
  (interface (panel<%>)
    spacing))

(define basic-linear-panel%
  (class basic-panel% (wx) 
    (public
      [spacing (ivar wx spacing)])
    (sequence
      (super-init wx))))

(define vertical-panel% (make-a-panel% basic-linear-panel% wx-vertical-panel%))
(define horizontal-panel% (make-a-panel% basic-linear-panel% wx-horizontal-panel%))

(define (wrap-callback cb)
  (lambda (cb)
    (lambda (b e)
      (cb e))))

(define button%
  (class basic-control% (parent callback label [style null])
    (sequence
      (panel-parent-only 'button parent)
      (super-init (make-object wx-button% this
			       (mred->wx parent) (wrap-callback callback)
			       label -1 -1 -1 -1 style)
		  label #f))))

(define check-box%
  (class basic-control% (parent callback label [style null])
    (sequence (panel-parent-only 'check-box parent))
    (private
      [wx (make-object wx-check-box% this
		       (mred->wx parent) (wrap-callback callback)
		       label -1 -1 -1 -1 style)])
    (public
      [get-value (ivar wx get-value)]
      [set-value (ivar wx set-value)])
    (sequence
      (super-init wx label #f))))


(define top-level-window<%>
  (interface (window<%>) center))

(define canvas-default-size 20)
; an arbitrary default size for canvases to avoid initial size problems
; under xt.


