(unit/sig framework:panel^
  (import mred^
	  [mzlib:function : mzlib:function^])
  
  (rename [-editor<%> editor<%>])

  (define single<%> (interface (area-container<%>) active-child))
  (define single-mixin
    (mixin (area-container<%>) (single<%>) args
      (inherit get-alignment)
      (rename [super-after-new-child after-new-child])
      (override
	[after-new-child
	 (lambda (c)
	   (if current-active-child
	       (send c show #f)
	       (set! current-active-child c)))]
       [container-size
	(lambda (l)
	  (if (null? l)
	      (values 0 0)
	      (values (apply max (map car l)) (apply max (map cadr l)))))]
       [place-children
	(lambda (l width height)
	  (let-values ([(h-align-spec v-align-spec) (get-alignment)])
	    (let ([align
		   (lambda (total-size spec item-size)
		     (floor
		      (case spec
			[(center) (- (/ total-size 2) (/ item-size 2))]
			[(left top) 0]
			[(right bottom) (- total-size item-size)]
			[else (error 'place-children "alignment spec is unknown ~a~n" spec)])))])
	      (map (lambda (l) 
		     (let*-values ([(min-width min-height v-stretch? h-stretch?) (apply values l)]
				   [(x this-width) (if h-stretch?
						       (values 0 width)
						       (values (align width h-align-spec min-width) min-width))]
				   [(y this-height) (if v-stretch?
							(values 0 height)
							(values (align height v-align-spec min-height) min-height))])
		       (list x y this-width this-height)))
		   l))))])
      
      (inherit get-children)
      (private [current-active-child #f])
      (public
	[active-child
	 (case-lambda
	  [() current-active-child]
	  [(x) 
	   (unless (eq? x current-active-child)
	     (for-each (lambda (x) (send x show #f))
		       (get-children))
	     (set! current-active-child x)
	     (send current-active-child show #t))])])
      (sequence
	(apply super-init args))))

  (define single-window<%> (interface (single<%> window<%>)))
  (define single-window-mixin
    (mixin (single<%> window<%>) (single-window<%>) args
      (inherit get-client-size get-size)
      (rename [super-container-size container-size])
      (override
       [container-size
	(lambda (l)
	  (let-values ([(super-width super-height) (super-container-size l)]
		       [(client-width client-height) (get-client-size)]
		       [(window-width window-height) (get-size)]
		       [(calc-size)
			(lambda (super client window)
			  (+ super (max 0 (- window client))))])
			 
	    (values
	     (calc-size super-width client-width window-width)
	     (calc-size super-height client-height window-height))))])
      (sequence
	(apply super-init args))))

  (define multi-view<%>
    (interface (area-container<%>)
      split-vertically
      split-horizontally
      collapse))

  (define multi-view-mixin
    (mixin (area-container<%>) (multi-view<%>) (parent editor)
      (public
	[get-editor-canvas%
	 (lambda ()
	   editor-canvas%)]
	[get-vertical%
	 (lambda ()
	   vertical-panel%)]
	[get-horizontal%
	 (lambda ()
	   horizontal-panel%)])

      (private
	[split
	 (lambda (p%)
	   (let ([canvas (send (send parent get-top-level-window) get-edit-target-window)]
		 [ec% (get-editor-canvas%)])
	     (when (and canvas
			(is-a? canvas ec%)
			(eq? (send canvas get-editor) editor))
	       (let ([p (send canvas get-parent)])
		 (send p change-children (lambda (x) null))
		 (let ([pc (make-object p% p)])
		   (send (make-object ec% (make-object vertical-panel% pc) editor) focus)
		   (make-object ec% (make-object vertical-panel% pc) editor))))))])

      (public
	[collapse
	 (lambda ()
	   (let ([canvas (send (send parent get-top-level-window) get-edit-target-window)]
		 [ec% (get-editor-canvas%)])
	     (when (and canvas
			(is-a? canvas ec%)
			(eq? (send canvas get-editor) editor))
	       (let ([p (send canvas get-parent)])
		 (if (eq? p this)
		     (bell)
		     (let* ([sp (send p get-parent)]
			    [p-to-remain (send sp get-parent)])
		       (send p-to-remain change-children (lambda (x) null))
		       (send (make-object ec% p-to-remain editor) focus)))))))])

      (public
	[split-vertically
	 (lambda ()
	   (split (get-vertical%)))]
	[split-horizontally
	 (lambda ()
	   (split (get-horizontal%)))])
      (sequence
	(super-init parent)
	(make-object (get-editor-canvas%) this editor))))

  (define single% (single-window-mixin (single-mixin panel%)))
  (define single-pane% (single-mixin pane%))
  (define multi-view% (multi-view-mixin vertical-panel%))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                          ;;
  ;;                 split panel              ;;
  ;;                                          ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (refresh-panel panel)
    (let-values ([(ha va) (send panel get-alignment)])
      (send panel set-alignment ha va)))

  (define thumb-canvas%
    (class canvas% (parent get-top-min get-bot-min)
      (private
	[percentage 1/2])
      (public
	[get-percentage (lambda () percentage)]
	[set-percentage (lambda (_p) 
			  (set! percentage _p)
			  (on-paint))])
      (private
	[gray-region 18]
	[canvas-width 18]
	[thumb-height 16]
	[thumb-min 16])
      
      (private
	[grabbed? #f]
	[get-thumb-middle
	 (lambda ()
	   (let-values ([(w h) (get-client-size)])
	     (floor (* h percentage))))]
	[get-thumb-top
	 (lambda ()
	   (- (get-thumb-middle) (/ thumb-height 2)))]
	[get-thumb-bottom
	 (lambda ()
	   (+ (get-thumb-top) thumb-height))]
	[update-percentage/draw
	 (lambda (mouse-evt)
	   (let-values ([(w h) (get-client-size)])
	     (let* ([y (inexact->exact (send mouse-evt get-y))]
		    [y-min (max thumb-min (get-top-min))]
		    [y-max (- h (max thumb-min (get-bot-min)))])
	       (set! percentage (/ (min (max y-min y) y-max) h))
	       (on-paint))))])
      (inherit get-dc get-client-size)
      (rename [super-on-event on-event])
      (override
       [on-event
	(lambda (evt)
	  (cond
	   [(send evt button-down?)
	    (set! grabbed? #t)
	    (update-percentage/draw evt)]
	   [(and grabbed? (send evt button-up?))
	    (set! grabbed? #f)
	    (update-percentage/draw evt)
	    (refresh-panel parent)]
	   [(and grabbed? (send evt moving?))
	    (update-percentage/draw evt)]
	   [else (super-on-event evt)]))]
       [on-paint
	(lambda ()
	  (let ([dc (get-dc)]
		[panel-color (get-panel-background)])
	    (let-values ([(w h) (get-client-size)])
	      (send dc set-pen (send the-pen-list find-or-create-pen panel-color 1 'solid))
	      (send dc set-brush (send the-brush-list find-or-create-brush panel-color 'solid))
	      (send dc draw-rectangle 0 0 w h)
	      
	      (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
	      (if grabbed?
		  (send dc set-brush (send the-brush-list find-or-create-brush "blue" 'solid))
		  (send dc set-brush (send the-brush-list find-or-create-brush "black" 'solid)))
	      (send dc draw-polygon
		    (list (make-object point% 2 (get-thumb-middle))
			  (make-object point% (- w 1) (get-thumb-top))
			  (make-object point% (- w 1) (get-thumb-bottom)))))))])
      
      (inherit min-width stretchable-width)
      (sequence 
	(super-init parent)
	(min-width canvas-width)
	(stretchable-width #f))))

  (define vertical-resizable<%>
    (interface (area-container<%>)
      set-percentage))
      
  (define vertical-resizable-mixin
    (mixin (area-container<%>) (vertical-resizable<%>) args

      ;; preserve the invariant that the thumb-canvas is
      ;; the first child.
      (rename [super-change-children change-children])
      (override
       [change-children
	(lambda (f)
	  (super-change-children
	   (lambda (l)
	     (let ([res (cons
			 thumb-canvas
			 (mzlib:function:filter
			  (lambda (c) (not (eq? c thumb-canvas)))
			  (f l)))])
	       (printf "change-children to ~s~n" res)
	       res))))])

      (override
       [container-size
	(lambda (_lst)
	  ;; remove the thumb canvas from the computation
	  (printf "container-size: ~n")
	  (let ([lst (if (null? _lst) null (cdr _lst))])
	    (values
	     (apply + (map car lst))
	     (cond
	      [(null? lst) 0]
	      [(null? (cdr lst)) (cadr (car lst))]
	      [else
	       (+ (send thumb-canvas min-width)
		  (apply max (map cadr lst)))]))))]
       [place-children
	(lambda (info width height)
	  (let* ([percentage (send thumb-canvas get-percentage)]
		 [first (floor (* percentage height))]
		 [second (- height first)]
		 [main-width (- width (send thumb-canvas min-width))]
		 [res
		  (cond
		   [(null? info) null]
		   [(null? (cdr info)) (list (list 0 0 0 0))]
		   [(null? (cdr (cdr info)))
		    (list (list 0 0 0 0)
			  (list 0 0 width height))]
		   [else
		    (list* (list (- width (send thumb-canvas min-width)) 0
				 (send thumb-canvas min-width)
				 height)
			   (list 0 0 main-width first)
			   (list 0 first main-width second)
			   (map (lambda (x) (list 0 0 0 0)) (cdddr info)))])])

	    (printf "place-children: ~s~n" res)
	    res))])
      (inherit reflow-container get-top-level-window set-alignment get-alignment)
      (public
	[set-percentage
	 (lambda (p)
	   (send thumb-canvas set-percentage p)
	   (refresh-panel this))])
      
      (sequence
	(apply super-init args))
      (inherit get-children)
      (private
	[make-get-min
	 (lambda (index)
	   (lambda ()
	     (let* ([children (get-children)])
	       (if (< index (length children))
		   (send (list-ref children index) min-height)
		   0))))]
	[thumb-canvas (make-object thumb-canvas% this (make-get-min 2) (make-get-min 3))])))

  (define vertical-resizable% (vertical-resizable-mixin panel%))
  (define vertical-resizable-pane% (vertical-resizable-mixin pane%)))