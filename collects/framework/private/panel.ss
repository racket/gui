(module panel mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   "sig"
	   "../macro.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "list.ss")
	   (lib "etc.ss"))

  (provide panel@)

  (define panel@
(unit/sig framework:panel^
  (import mred^)
  
  (rename [-editor<%> editor<%>])

  (define (list-set! _list _i ele)
    (let loop ([lst _list]
               [i _i])
      (cond
        [(null? lst) (error 'list-set! "index too large for list, args: ~e ~e ~e"
                            _list _i ele)]
        [(zero? i) (set-car! lst ele)]
        [else (loop (cdr lst) (- i 1))])))

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
      (private-field [current-active-child #f])
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
    (mixin (area-container<%>) (multi-view<%>) (_parent _editor)
      
      (private-field [parent _parent]
                     [editor _editor])
      
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
    (class canvas% (parent)
      (private

	;; (listof number)
	;; the length of the list is equal to the number of children
	;; of the panel. Each entry in the list is the percentage
	;; of the window that child occupies.
	[percentages (list 1/2 1/2)])
      (public
	[get-percentages (lambda () percentages)]
	[set-percentages (lambda (_p) 
			  (set! percentages _p)
			  (on-paint))])
      (private
	[thumb-height 12]
	[canvas-width (+ thumb-height 3)]
        [thumb-min thumb-height])
      
      (private
	
        ;; (union #f num)
        ;; if num, ranges between 0 and (- (length percentages) 2)
        ;; indicates the thumb that is currently grabbed. Since there
        ;; is one fewer thumb than window, it is bounded by the
        ;; length of the percentages minus 2.
        ;; 0 corresponds to the first thumb, which is between the 
        ;; 0th and 1st percentage (in the percentage list)
        ;; 1 corresponds to the first thumb, which is between the 
        ;; 1st and 2nd percentage, etc.
        [grabbed #f]
        
	[get-thumb-middle
	 (lambda (percentage)
	   (let-values ([(w h) (get-client-size)])
	     (floor (* h percentage))))]
	[get-thumb-top
	 (lambda (percentage)
	   (- (get-thumb-middle percentage) (/ thumb-height 2)))]
	[get-thumb-bottom
	 (lambda (percentage)
	   (+ (get-thumb-top percentage) thumb-height))]
        [between-click?
         (lambda (evt)
	   (and (not (null? percentages))
		(let ([y (send evt get-y)])
		  (let-values ([(w h) (get-client-size)])
		    (let loop ([percentages (cdr percentages)]
			       [sum (car percentages)]
			       [n 0])
		      (cond
		       [(null? percentages)
			(list n (/ y h))]
		       [else (let ([thumb-top (get-thumb-top sum)]
				   [thumb-bottom (get-thumb-bottom sum)])
			       (cond
				[(<= y thumb-top)
				 (list n (/ y h))]
				[(<= thumb-top y thumb-bottom) #f]
				[else (loop (cdr percentages)
					    (+ (car percentages) sum)
					    (+ n 1))]))]))))))]
        [update-grabbed
         (lambda (mouse-evt)
           (unless (null? percentages)
             (let loop ([percentages (cdr percentages)]
                        [n 0]
                        [sofar (car percentages)])
               (cond
                 [(null? percentages) (void)]
                 [else
                  (let ([percentage (car percentages)])
                    (if (<= (get-thumb-top sofar)
                            (send mouse-evt get-y)
                            (get-thumb-bottom sofar))
                        (set! grabbed n)
                        (loop (cdr percentages)
                              (+ n 1)
                              (+ sofar (car percentages)))))]))))]
	[force-between (lambda (low x hi) (min (max low x) hi))]
        [sum-percentages
         (lambda (i)
           (let loop ([percentages percentages]
                      [i i])
             (cond
               [(null? percentages) (error 'panel:vertical-resizable "internal error: sub-percentages")]
               [(= i 0) (car percentages)]
               [else (+ (car percentages) (loop (cdr percentages) (- i 1)))])))]
        [update-percentage/draw
	 (lambda (mouse-evt)
           (when (and (not (null? percentages))
                      grabbed)
             (let-values ([(w h) (get-client-size)])
               (let* ([y (inexact->exact (send mouse-evt get-y))]
                      [y-min 
                       (let ([min-child-height
                              (max thumb-height
                                   (let-values ([(w h) (send (list-ref (send parent get-children) (+ grabbed 1)) get-graphical-min-size)])
                                     h))])
                         ;(printf "min-child-height: ~s ~s ~s~n" min-child-height grabbed (send parent get-children))
                         (if (= grabbed 0)
                             min-child-height
                             (+ (get-thumb-middle (sum-percentages (- grabbed 1)))
                                min-child-height)))]
                      [y-max (if (= grabbed (- (length percentages) 2))
                                 (- h thumb-min)
                                 (- (get-thumb-middle (sum-percentages (+ grabbed 1)))
				    thumb-height))])
                 (let ([old-percentage (list-ref percentages grabbed)]
                       [new-percentage (/ (- (force-between y-min y y-max)
					     (if (= grabbed 0)
						 0
						 (get-thumb-middle (sum-percentages (- grabbed 1)))))
					  h)])
                   (list-set! percentages grabbed new-percentage)
                   (list-set! percentages (+ grabbed 1) 
                              (+ (list-ref percentages (+ grabbed 1))
                                 (- old-percentage new-percentage))))
                 (send parent on-percentage-change)
                 (on-paint)))))])
      (private
        [point1 (make-object point% 0 0)]
        [point2 (make-object point% 0 0)]
        [point3 (make-object point% 0 0)]
        [points (list point1 point2 point3)]
        [grab-brush (send the-brush-list find-or-create-brush "blue" 'solid)]
        [reg-brush (send the-brush-list find-or-create-brush "black" 'solid)]
        [reg-pen (send the-pen-list find-or-create-pen "black" 1 'solid)])
      (inherit get-dc get-client-size get-top-level-window)
      (rename [super-on-event on-event])
      (override
       [on-event
	(lambda (evt)
	  (cond
            [(send evt button-down?)
             (cond
               [(between-click? evt)
                =>
                (lambda (lst)
                  (send parent on-between-click
			(car lst)
			(cadr lst)))]
               [else
                (update-grabbed evt)
                (update-percentage/draw evt)])]
            [(and grabbed (send evt button-up?))
             (set! grabbed #f)
             (update-percentage/draw evt)
             (refresh-panel parent)]
            [(and grabbed (send evt moving?))
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
	      
	      (send dc set-pen reg-pen)
	      (let loop ([percentages percentages]
			 [percentage-total 0]
                         [n 0])
                (if (equal? n grabbed)
                    (send dc set-brush grab-brush)
                    (send dc set-brush reg-brush))
		(cond
		 [(null? percentages) (void)]
                 [(null? (cdr percentages)) (void)]
		 [else
		  (let ([percentage-total (+ (car percentages) percentage-total)])
		    (send point1 set-x 2)
		    (send point1 set-y (get-thumb-middle percentage-total))
		    (send point2 set-x (- w 1))
		    (send point2 set-y (get-thumb-top percentage-total))
		    (send point3 set-x (- w 1))
		    (send point3 set-y (get-thumb-bottom percentage-total))
		    (send dc draw-polygon points)
		    (loop (cdr percentages) percentage-total (+ n 1)))])))))])
      
      (inherit min-width stretchable-width)
      (sequence 
	(super-init parent)
	(min-width canvas-width)
	(stretchable-width #f))))

  (define vertical-resizable<%>
    (interface (area-container<%>)
      on-percentage-change
      get-percentages
      set-percentages))
      
  (define vertical-resizable-mixin
    (mixin (area-container<%>) (vertical-resizable<%>) args
      (inherit get-children)

      (private [thumb-canvas #f])
      (public
        [on-between-click
         (lambda (num pct)
	   (void))])

      ;; preserve the invariant that the thumb-canvas is
      ;; the first child and that the thumb-canvas percentages
      ;; match up with the children
      (private
	[fix-percentage-length
	 (lambda (children)
	   (let ([len (length children)])
	     (unless (= (- len 1) (length (send thumb-canvas get-percentages)))
	       (send thumb-canvas set-percentages 
		     (build-list
		      (- len 1)
		      (lambda (i) (/ 1 (- len 1))))))))])
      (rename [super-change-children change-children])
      (override
       [change-children
	(lambda (f)
	  (super-change-children
	   (lambda (l)
             (if thumb-canvas
                 (let* ([res (cons
                              thumb-canvas
                              (filter
                               (lambda (c) (not (eq? c thumb-canvas)))
                               (f l)))])
                   (fix-percentage-length res)
                   res)
                 (f l)))))]
       [after-new-child
        (lambda (child)
          (when thumb-canvas
	    (fix-percentage-length (get-children))))])
      
      (override
       [container-size
	(lambda (_lst)
	  ;; remove the thumb canvas from the computation
	  (let ([lst (if (null? _lst) null (cdr _lst))])
	    (values
	     (cond
               [(null? lst) 0]
               [(null? (cdr lst)) (cadr (car lst))]
               [else
                (+ (send thumb-canvas min-width)
                   (apply max (map car lst)))])
             (apply + (map cadr lst)))))]
       [place-children
	(lambda (_infos width height)
	  (cond
	   [(null? _infos) null]
	   [(null? (cdr _infos)) (list (list 0 0 0 0))]
	   [(null? (cdr (cdr _infos)))
	    (list (list 0 0 0 0)
		  (list 0 0 width height))]
	   [else
	    (fix-percentage-length (get-children))
	    (cons
	     (list (- width (send thumb-canvas min-width)) 0
		   (send thumb-canvas min-width)
		   height)
	     (let ([main-width (- width (send thumb-canvas min-width))]
                   [show-error
                    (lambda ()
                      (error 'panel:vertical-resizable-mixin:place-children
                             "expected children list(~a) to be one longer than percentage list(~a), info: ~e percentages ~e"
                             (length _infos) (length (send thumb-canvas get-percentages))
                             _infos (send thumb-canvas get-percentages)))])
	       (let loop ([percentages (send thumb-canvas get-percentages)]
			  [infos (cdr _infos)]
			  [y 0])
		 (cond
                   [(null? percentages)
                    (unless (null? infos) (show-error))
                    null]
                   [(null? (cdr percentages))
                    (when (null? infos) (show-error))
                    (unless (null? (cdr infos)) (show-error))
                    (list (list 0 y main-width (- height y)))]
                   [else
                    (when (null? infos) (show-error))
                    (let* ([info (car infos)]
                           [percentage (car percentages)]
                           [this-space (floor (* percentage height))])
                      (cons (list 0 y main-width this-space)
                            (loop (cdr percentages)
                                  (cdr infos)
                                  (+ y this-space))))]))))]))])
      (inherit reflow-container get-top-level-window set-alignment get-alignment)
      (public
        [on-percentage-change (lambda () (void))]
	[get-percentages (lambda () (send thumb-canvas get-percentages))]
	[set-percentages
	 (lambda (p)
	   (send thumb-canvas set-percentages p)
	   (refresh-panel this))])
      
      (sequence
	(apply super-init args)
        (set! thumb-canvas (make-object thumb-canvas% this)))))

  (define vertical-resizable% (vertical-resizable-mixin panel%))
  (define vertical-resizable-pane% (vertical-resizable-mixin pane%)))))