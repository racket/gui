
(module mred mzscheme
  (require (prefix wx: (lib "kernel.ss" "mred" "private")))
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "file.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   (lib "process.ss")
	   (lib "port.ss")
	   (lib "moddep.ss" "syntax")
           "private/seqcontract.ss"
	   "afm.ss")

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

(define o (current-output-port))

(define no-val (gensym)) ; indicates init arg not supplied

;;;;;;;;;;;;;;; Thread Safety ;;;;;;;;;;;;;;;;;;;;

;; When the user creates an object or calls a method, or when the
;; system invokes a callback, many steps may be required to initialize
;; or reset fields to maintain invariants. To ensure that other
;; threads do not call methods during a time when invariants do not
;; hold, we force all of the following code to be executed in a single
;; threaded manner, and we temporarily disable breaks. This accompiled
;; with a single monitor: all entry points into the code use
;; `entry-point' or `as-entry', and all points with this code that
;; call back out to user code uses `as-exit'.

;; If an exception is raised within an `enter'ed area, control is
;; moved back outside by the exception handler, and then the exception
;; is re-raised. The user can't tell that the exception was caught an
;; re-raised. But without the catch-and-reraise, the user's exception
;; handler might try to use GUI elements from a different thread,
;; leading to deadlock.

(define monitor-sema (make-semaphore 1))
(define monitor-owner #f)

;; An exception may be constructed while we're entered:
(define entered-err-string-handler
  (lambda (s n)
    (as-exit
     (lambda ()
       ((error-value->string-handler) s n)))))

(define old-paramz #f)
(define old-break-paramz #f)

(define (as-entry f)
  (cond
   [(eq? monitor-owner (current-thread))
    (f)]
   [else
    ((let/ec k
       (dynamic-wind
	   (lambda () 
	     (wx:in-atomic-region monitor-sema)	     
	     (set! monitor-owner (current-thread)))
	   (lambda () 
	     (set! old-paramz (current-parameterization))
	     (set! old-break-paramz (current-break-parameterization))
	     (parameterize ([error-value->string-handler entered-err-string-handler]
			    [current-exception-handler
			     (lambda (exn)
			       ;; Get out of atomic region before letting
			       ;;  an exception handler work
			       (k (lambda () (raise exn))))])
	       (parameterize-break #f
		 (call-with-values 
		     f 
		   (lambda args (lambda () (apply values args)))))))
	   (lambda ()
	     (set! monitor-owner #f)
	     (semaphore-post monitor-sema)
	     (wx:in-atomic-region #f)))))]))

; entry-point macros in macros.ss

(define (as-exit f)
  ; (unless (eq? monitor-owner (current-thread)) (error 'monitor-exit "not in monitored area"))
  (call-with-parameterization
   old-paramz
   (lambda ()
     (call-with-break-parameterization
      old-break-paramz
      (lambda ()
	(dynamic-wind
	    (lambda ()
	      (set! monitor-owner #f)	 
	      (semaphore-post monitor-sema)
	      (wx:in-atomic-region #f))
	    f
	    (lambda ()
	      (wx:in-atomic-region monitor-sema)
	      (set! monitor-owner (current-thread)))))))))

(define-syntax entry-point 
  (lambda (stx)
    (syntax-case stx (lambda case-lambda)
      [(_ (lambda args body1 body ...))
       (syntax (lambda args (as-entry (lambda () body1 body ...))))]
      [(_ (case-lambda [vars body1 body ...] ...))
       (syntax (case-lambda 
		[vars (as-entry (lambda () body1 body ...))]
		...))])))

(define-syntax mk-param
  (lambda (stx)
    (syntax-case stx ()
      [(_ val filter check force-redraw)
       (syntax
	(case-lambda
	 [() val]
	 [(v) (check v)
	      (let ([v2 (filter v)])
		(unless (eq? v2 val)
		  (set! val v2)
		  (force-redraw)))]))])))
	 
	 
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

;; list-diff: computes the difference between two lists
;; input: l1, l2: two lists
;; returns:  a list of all elements in l1 which are not in l2.
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

(define ibeam (make-object wx:cursor% 'ibeam))
(define arrow-cursor (make-object wx:cursor% 'arrow))

(define top-x 1)
(define top-y 1)

(define top-level-windows (make-hash-table 'weak))

(define (key-regexp c)
  (regexp (format "(^|[^&])&[~a~a]" (char-downcase c) (char-upcase c))))


(define (do-command c e)
  (as-exit (lambda () (send c command e))))

;;;;;;;;;;;;;;; Focus-tabbing helpers ;;;;;;;;;;;;;;;;;;;;

(define (traverse x y w h dir dests)
  ;; x, y : real = starting positions
  ;; dir : one of 'left, 'right, 'up, 'next, 'prev = desried move
  ;; dests : list of (cons key x y w h) = destinations
  ;; returns key or #f
  (case dir
    [(next prev)
     (letrec ([get-x cadr]
	      [get-w cadddr]
	      [get-y caddr]
	      [get-h (lambda (x) (caddr (cddr x)))]
	      [backward? (eq? dir 'prev)]
	      [fail-start (if backward?
			      1000000000
			      0)]
	      [find-stripe (lambda (t stripes)
			     (let loop ([s stripes])
			       (cond
				[(null? s) #f]
				[(and (<= (caar s) t) (< t (cdar s)))
				 (car s)]
				[else (loop (cdr s))])))]
	      [mk-stripes
	       (lambda (get-y get-h stripes dests)
		 (let loop ([l (append (map (lambda (x) (cons (car x) (- (cdr x) (car x))))
					    stripes)
				       (map (lambda (x) 
					      (cons (get-y x) (get-h x)))
					    dests))])
		   (if (null? l)
		       null
		       ;; Find longest top-most
		       (let* ([top (let loop ([l (cdr l)][best (car l)])
				     (cond
				      [(null? l) best]
				      [(or (< (caar l) (car best)) ; topmost
					   (and (= (caar l) (car best)) ; at least as top
						(> (cdar l) (cdr best)))) ; longer
				       (loop (cdr l) (car l))]
				      [else (loop (cdr l) best)]))]
			      [t (car top)]
			      [b (+ t (cdr top))])
			 ;; Stripe is anything that starts before the end of `top'
			 (let ([remaining (let loop ([l l])
					    (cond
					     [(null? l) null]
					     [(find-stripe (caar l) (list (cons t b)))
					      (loop (cdr l))]
					     [else (cons (car l) (loop (cdr l)))]))])
			   (cons (cons t b) (loop remaining)))))))]
	      [in-stripe (lambda (stripe dests get-y get-h)
			   (let loop ([l dests])
			     (cond
			      [(null? l) null]
			      [(find-stripe (get-y (car l)) (list stripe))
			       (cons (car l) (loop (cdr l)))]
			      [else (loop (cdr l))])))]
	      [next-stripe (lambda (stripe stripes)
			     (let loop ([s stripes][best #f])
			       (cond
				[(null? s) best]
				[(and (or (not stripe)
					  (if backward?
					      (<= (cdar s) (car stripe))
					      (>= (caar s) (cdr stripe))))
				      (or (not best)
					  (if backward?
					      (> (cdar s) (cdr best))
					      (< (caar s) (cdr best)))))
				 (loop (cdr s) (car s))]
				[else (loop (cdr s) best)])))]
	      [find (lambda (get-x get-w get-y get-h use-x? x w use-y? y h dests fail)
		      ;; find's variable names correspond to an h-stripe view, but everything is
		      ;;  flipped to v-stripes if the args are flipped
		      (let ([h-stripes (mk-stripes get-y get-h 
						   (if use-y? (list (cons y (+ y h))) null)
						   dests)])

			;; find the initial h-stripe
			(let sel-h-stripe-loop ([init-h-stripe (if use-y?
								   (find-stripe y h-stripes)
								   (next-stripe #f h-stripes))]
						[x x][w w][use-x? use-x?])
			  
			  ;; find items in the initial stripe
			  (let ([in-init-h-stripe (in-stripe init-h-stripe dests get-y get-h)]
				[next (lambda ()
					(let ([s (next-stripe init-h-stripe h-stripes)])
					  (if s
					      (sel-h-stripe-loop s fail-start fail-start #f)
					      (fail))))])

			    (if (null? in-init-h-stripe)

				;; no items in this stripe; try the next one
				(next)
				
				;; Non-empty h-stripe; now look for items in the same or later v-stripe
				(if (null? (cdr in-init-h-stripe))
				    
				    ;; one item in the stripe; take it unless we're using x and it's
				    ;;  before x:
				    (if (or (not use-x?)
					    ((if backward? < >) (get-x (car in-init-h-stripe)) x))
					(car in-init-h-stripe)
					
					;; Only item is no good; try the next stripe
					(next))
				    
				    ;; Recur to work with v-stripes
				    (find get-y get-h get-x get-w use-y? y h use-x? x w in-init-h-stripe next)))))))])
       (if (null? dests)
	   #f
	   (car (find get-x get-w get-y get-h #t x w #t y h dests
		      (lambda ()
			(find get-x get-w get-y get-h 
			      #f fail-start fail-start 
			      #f fail-start fail-start 
			      dests void))))))]
    [else
     (let ([v (let loop ([d dests])
		(if (null? d)
		    #f
		    (let* ([best (loop (cdr d))]
			   [this (car d)]
			   [diff (lambda (v l x w)
				   (cond
				    [(< (+ v l) x) (- x (+ v l))]
				    [(< (+ x w) v) (- (+ x w) v)]
				    [else 0]))])
		      (let* ([get-x cadr]
			     [get-w cadddr]
			     [get-y caddr]
			     [get-h (lambda (x) (caddr (cddr x)))]
			     [tdx (diff x w (get-x this) (get-w this))]
			     [tdy (diff y h (get-y this) (get-h this))]
			     [bdx (and best (diff x w (get-x best) (get-w best)))]
			     [bdy (and best (diff y h (get-y best) (get-h best)))]
			     [better (lambda (tdx tdy bdy negative?)
				       (if (and (zero? tdx) (negative? tdy)
						(or (not best) 
						    (< (abs tdy) (abs bdy))))
					   this
					   best))])
			(case dir
			  [(up) (better tdx tdy bdy negative?)]
			  [(down) (better tdx tdy bdy positive?)]
			  [(left) (better tdy tdx bdx negative?)]
			  [(right) (better tdy tdx bdx positive?)])))))])
       (and v (car v)))]))

(define (object->position o)
  (let-values ([(x y) (double-boxed 0 0 (lambda (x y) (send o client-to-screen x y)))]
	       [(w h) (double-boxed 0 0 (lambda (x y) (send o get-client-size x y)))])
    (list o x y w h)))

(define (container->children f except must-focus?)
  (apply
   append
   (map
    (lambda (i)
      (cond
       [(is-a? i wx-basic-panel<%>) 
	(if (or (is-a? i wx:windowless-panel%)
		(send i is-shown?))
	    (container->children i except must-focus?)
	    null)]
       [(or (eq? i except) 
	    (and must-focus? (not (send i gets-focus?)))
	    (not (send i is-enabled?))
	    (not (send i is-shown?)))
	null]
       [else (list i)]))
    (send f get-children))))

(define (filter-overlapping l)
  (if (null? l)
      null
      (let* ([rest (filter-overlapping (cdr l))]
	     [first (car l)]
	     [f (cdr first)]
	     [x (car f)]
	     [y (cadr f)]
	     [x2 (+ x (caddr f))]
	     [y2 (+ y (cadddr f))])
	(if (ormap (lambda (other)
		     (let* ([p (cdr other)]
			    [px (car p)]
			    [py (cadr p)]
			    [px2 (+ px (caddr p))]
			    [py2 (+ py (cadddr p))])
		       (and (or (<= x px x2) (<= x px2 x2)
				(<= px x px2) (<= px x2 px2))
			    (or (<= y py y2) (<= y py2 y2)
				(<= py y py2) (<= py y2 py2)))))
		   rest)
	    rest
	    (cons first rest)))))

;;;;;;;;;;;;;;; wx- Class Construction ;;;;;;;;;;;;;;;;;;;;

;; ------------- Mixins for common functionality --------------


(define wx-make-window%
  (lambda (% top?)
    (class100 % args
      (rename [super-on-set-focus on-set-focus]
	      [super-on-kill-focus on-kill-focus]
	      [super-drag-accept-files drag-accept-files]
	      [super-show show]
	      [super-enable enable])
      (inherit is-shown-to-root? is-enabled-to-root?)
      (private-field
	[top-level #f]
	[focus? #f]
	[container this]
	[visible? #f]
	[active? #f])
      (public
	[on-visible
	 (lambda ()
	   (let ([vis? (is-shown-to-root?)])
	     (unless (eq? vis? visible?)
	       (set! visible? vis?)
	       (as-exit
		(lambda ()
		  (send (wx->proxy this) on-superwindow-show vis?))))))]
	[queue-visible
	 (lambda ()
	   (parameterize ([wx:current-eventspace (send (get-top-level) get-eventspace)])
	     (wx:queue-callback (entry-point (lambda () (on-visible))) wx:middle-queue-key)))])
      (public
	[on-active
	 (lambda ()
	   (let ([act? (is-enabled-to-root?)])
	     (unless (eq? act? active?)
	       (set! active? act?)
	       (as-exit
		(lambda ()
		  (send (wx->proxy this) on-superwindow-enable act?))))))]
	[queue-active
	 (lambda ()
	   (parameterize ([wx:current-eventspace (send (get-top-level) get-eventspace)])
	     (wx:queue-callback (entry-point (lambda () (on-active))) wx:middle-queue-key)))]

	;; Needed for radio boxes:
	[orig-enable
	 (lambda args (super-enable . args))])
      
      (private-field
	[can-accept-drag? #f])

      (public
	[accept-drag? (lambda () can-accept-drag?)]
	[get-container (lambda () container)]
	[set-container (lambda (c) (set! container c))]
	[get-window (lambda () this)]
	[dx (lambda () 0)]
	[dy (lambda () 0)]
	[ext-dx (lambda () (dx))]
	[ext-dy (lambda () (dy))]
	[handles-key-code (lambda (x alpha? meta?) #f)]
	[char-to (lambda () (void))]
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
	[show
	 (lambda (on?)
	   (queue-visible)
	   (super-show on?))]
	[enable
	 (lambda (on?)
	   (queue-active)
	   (super-enable on?))]

	[drag-accept-files
	 (lambda (on?)
	   (set! can-accept-drag? (and on? #t))
	   (super-drag-accept-files on?))]
	[on-set-focus
	 (entry-point
	  (lambda ()
	    (send (get-top-level) set-focus-window this)
	    (set! focus? #t)
	    (as-exit (lambda () (super-on-set-focus)))))]
	[on-kill-focus
	 (entry-point
	  (lambda ()
	    (send (get-top-level) set-focus-window #f)
	    (set! focus? #f)
	    (as-exit (lambda () (super-on-kill-focus)))))])
      (public
	[has-focus? (lambda () focus?)])
      (sequence 
	(apply super-init args)
	(unless top?
	  (set! visible? (is-shown-to-root?))
	  (set! active? (is-enabled-to-root?)))))))

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
(define (make-top-container% base% dlg?)
  (class100 (wx-make-container% (wx-make-window% base% #t)) (parent . args)
    (inherit get-x get-y get-width get-height set-size
	     get-client-size is-shown? on-close enforce-size)
    (rename [super-show show] [super-move move] [super-center center]
	    [super-on-size on-size]
	    [super-enable enable]
	    [super-on-visible on-visible]
	    [super-on-active on-active])
    (private-field
      ; have we had any redraw requests while the window has been
      ; hidden?
      [pending-redraws? #t]

      [perform-updates? #t]
      [seq-count 0]
      
      [ignore-redraw-request? #f]
      
      [already-trying? #f]
      [was-bad? #f] ; hack around min-frame-size limitations
      [last-width -1]
      [last-height -1]
      
      ; pointer to panel in the frame for use in on-size
      [panel #f]

      [use-default-position? (and (= -11111 (list-ref args 2))
				  (= -11111 (list-ref args (if dlg? 3 1))))]
      
      [enabled? #t]
      [focus #f]
      [target #f])
    
    (override
      [enable
       (lambda (b)
	 (set! enabled? (and b #t))
	 (super-enable b))])
    (private-field
      [eventspace (if parent
		      (send parent get-eventspace)
		      (wx:current-eventspace))])

    (public
      [get-eventspace (lambda () eventspace)]

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
	 (set! pending-redraws? #t)
	 (let-values ([(client-w client-h)
		       (get-two-int-values (lambda (a b) (get-client-size a b)))])
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
      [begin-container-sequence
       (lambda ()
	 (when (zero? seq-count)
	   (delay-updates #t))
	 (set! seq-count (add1 seq-count)))]
      [end-container-sequence
       (lambda ()
	 (set! seq-count (sub1 seq-count))
	 (when (zero? seq-count)
	   (delay-updates #f)))]


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
	 (if panel
	     (dynamic-wind
	      (lambda () (set! ignore-redraw-request? #t))
	      (lambda () (resized))
	      (lambda () (set! ignore-redraw-request? #f)))
	     
	     (set! pending-redraws? #f)))]

      [correct-size
       (lambda (frame-w frame-h)
	 (if (not panel)
	     (values frame-w frame-h)
	     (let-values ([(f-client-w f-client-h) (get-two-int-values 
						    (lambda (a b) (get-client-size a b)))])
	       (let* ([panel-info (send panel get-info)]
		      
		      ; difference between panel's full size & 
		      ; frame's full size
		      [delta-w (max 0 (- (get-width) f-client-w))]
		      [delta-h (max 0 (- (get-height) f-client-h))]

		      ; minimum frame size:
		      [min-w (+ delta-w (child-info-x-min panel-info))]
		      [min-h (+ delta-h (child-info-y-min panel-info))]
		      
		      ; correct size for frame
		      [new-w
		       (cond
			[(< frame-w min-w) min-w]
			[(and (> frame-w min-w) (not (child-info-x-stretch panel-info))) min-w]
			[else frame-w])]
		      [new-h
		       (cond
			[(< frame-h min-h) min-h]
			[(and (> frame-h min-h) (not (child-info-y-stretch panel-info))) min-h]
			[else frame-h])])
		 (values new-w new-h 
			 min-w min-h
			 (child-info-x-stretch panel-info) (child-info-y-stretch panel-info))))))]

      [set-panel-size
       (lambda ()
	 (when panel
	   (let-values ([(f-client-w f-client-h) (get-two-int-values 
						  (lambda (a b) (get-client-size a b)))]
			[(panel-info) (send panel get-info)]
			[(sel) (lambda (nsize psize stretch?)
				 (if stretch?
				     (max nsize psize)
				     psize))])
	     (send panel set-size 0 0 
		   (sel f-client-w (child-info-x-min panel-info)
			(child-info-x-stretch panel-info))
		   (sel f-client-h (child-info-y-min panel-info)
			(child-info-y-stretch panel-info)))
	     (set! pending-redraws? #f)
	     (send panel on-container-resize))))]


      [resized
       (entry-point
	(lambda ()
	  (unless already-trying?
	    (let ([new-width (get-width)]
		  [new-height (get-height)])
	      (let-values ([(correct-w correct-h min-w min-h sx? sy?) (correct-size new-width new-height)])
		(cond
		 [(and (= new-width correct-w) (= new-height correct-h))
		  ;; Good size; do panel
		  (set! was-bad? #f)
		  (enforce-size min-w min-h
				(if sx? -1 min-w) (if sy? -1 min-h)
				1 1)
		  (set-panel-size)]
		 [(and (= last-width correct-w) (= last-height correct-h)
		       was-bad?)
		  ;; We give up; do panel
		  (set-panel-size)]
		 [else
		  ;; Too large/small; try to fix it, but give up after a while
		  (set! was-bad? #t)
		  (set! last-width correct-w)
		  (set! last-height correct-h)
		  (set! already-trying? #t)
		  (enforce-size -1 -1 -1 -1 1 1)
		  (set-size -1 -1 correct-w correct-h)
		  (enforce-size min-w min-h
				(if sx? -1 min-w) (if sy? -1 min-h)
				1 1)
		  (set! already-trying? #f)
		  (resized)]))))))])
    
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
	 (if on?
	     (hash-table-put! top-level-windows this #t)
	     (hash-table-remove! top-level-windows this))
	 (as-exit ; as-exit because there's an implicit wx:yield for dialogs
	  (lambda () (super-show on?))))]

      [on-visible
       (lambda ()
	 (send panel queue-visible)
	 (super-on-visible))]
      [on-active
       (lambda ()
	 (send panel queue-active)
	 (super-on-active))]
      
      [move (lambda (x y) (set! use-default-position? #f) (super-move x y))]
      [center (lambda (dir)
		(when pending-redraws? (force-redraw))
		(set! use-default-position? #f)
		(super-center dir))]
      
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
       (lambda (bad-width bad-height)
	 (unless (and already-trying? (not (eq? 'unix (system-type))))
	   (parameterize ([wx:current-eventspace eventspace])
	     (wx:queue-callback (lambda () (resized)) #t))))])

    (public
      [handle-traverse-key
       (lambda (e)
	 (and panel
	      (let ([code (send e get-key-code)])
		(case code
		  [(#\return) 
		   (let ([o (get-focus-window)])
		     (if (and o (send o handles-key-code code #f #f))
			 #f
			 (let ([objs (container->children panel #f #f)])
			   (or (ormap
				(lambda (x)
				  (and (is-a? x wx:button%)
				       (send x has-border?)
				       (let ([v (make-object wx:control-event% 'button)])
					 (do-command x v)
					 #t)))
				objs)
			       (not (is-a? o wx-editor-canvas%))))))]
		  [(escape #\.)
		   (and (is-a? this wx:dialog%)
			(or (eq? code 'escape)
			    (and (memq (system-type) '(macos macosx))
				 (send e get-meta-down)))
			(let ([o (get-focus-window)])
			  (if (and o (send o handles-key-code code #f (send e get-meta-down)))
			      #f
			      (begin
				(when (on-close)
				  (show #f))
				#t))))]
		  [(#\space)
		   (let ([o (get-focus-window)])
		     (cond
		      [(is-a? o wx:button%)
		       (do-command o (make-object wx:control-event% 'button))
		       #t]
		      [(is-a? o wx:check-box%) 
		       (send o set-value (not (send o get-value)))
		       (do-command o (make-object wx:control-event% 'check-box))
		       #t]
		      [(is-a? o wx:radio-box%)
		       (let ([s (send o button-focus -1)])
			 (unless (negative? s)
			   (send o set-selection s)
			   (do-command o (make-object wx:control-event% 'radio-box))))
		       #t]
		      [else #f]))]
		  [(#\tab left up down right) 
		   (let ([o (get-focus-window)])
		     (if (and o (send o handles-key-code code #f #f))
			 #f
			 (let* ([shift? (send e get-shift-down)]
				[forward? (or (and (eq? code #\tab) (not shift?))
					      (memq code '(right down)))]
				[normal-move
				 (lambda ()
				   (let* ([o (if (or (is-a? o wx:canvas%) (is-a? o wx:item%)) 
						 (if (or (is-a? o wx-tab-group%)
							 (is-a? o wx-group-box%))
						     #f
						     o)
						 #f)]
					  [candidates 
					   (map object->position (container->children panel o #t))]
					  [dests (filter-overlapping candidates)]
					  [pos (if o (object->position o) (list 'x 0 0 1 1))]
					  [o (traverse (cadr pos) (caddr pos) (cadddr pos) (list-ref pos 4)
						       (case code
							 [(#\tab) (if shift? 'prev 'next)]
							 [else code])
						       dests)])
				     (when o
				       (if (is-a? o wx:radio-box%)
					   (send o button-focus (if forward? 0 (sub1 (send o number))))
					   (begin
					     (send o set-focus)
					     (if (and (is-a? o wx-text-editor-canvas%)
						      (send o is-single-line?))
						 (let ([e (send o get-editor)])
						   (as-exit
						    (lambda ()
						      (send e set-position 0 (send e last-position) #f #t 'local))))
						 ;; Not a text field; a canvas?
						 (when (or (is-a? o wx-canvas%)
							   (is-a? o wx-editor-canvas%))
						   (as-exit (lambda () (send o on-tab-in))))))))))])
			   (if (is-a? o wx:radio-box%)
			       (let ([n (send o number)]
				     [s (send o button-focus -1)]
				     [v-move? (memq code '(up down))]
				     [h-move? (memq code '(left right))]
				     [v? (send o vertical?)])
				 (cond
				  [(or (negative? s) 
				       (and v? h-move?) 
				       (and (not v?) v-move?))
				   (normal-move)]
				  [(and forward? (< s (sub1 n)))
				   (send o button-focus (add1 s))]
				  [(and (not forward?) (positive? s))
				   (send o button-focus (sub1 s))]
				  [else (normal-move)]))
			       (normal-move))
			   #t)))]
		  [else (if (and (wx:shortcut-visible-in-label?)
				 (char? code)
				 (or (char-alphabetic? code)
				     (char-numeric? code))
				 (not (send e get-shift-down))
				 (not (send e get-control-down))
				 (not (send e get-alt-down)))
			    (let ([o (get-focus-window)]
				  [meta? (send e get-meta-down)])
			      (if (and o (send o handles-key-code code #t meta?))
				  #f
				  ;; Move selection/hit control based on & shortcuts
				  (let* ([objs (container->children panel #f #t)]
					 [re (key-regexp code)])
				    (ormap
				     (lambda (o)
				       (let* ([win (wx->proxy o)]
					      [l (send win get-label)])
					 (cond
					  [(and (string? l)
						(regexp-match re l))
					   (send o set-focus)
					   (send o char-to)
					   #t]
					  [(is-a? o wx:radio-box%)
					   (let ([n (send o number)])
					     (let loop ([i 0])
					       (if (= i n)
						   #f
						   (let ([l (send o get-string i)])
						     (if (and (string? l)
							      (regexp-match re l))
							 (begin
							   (send o button-focus i)
							   (send o char-to-button i)
							   #t)
							 (loop (add1 i)))))))]
					  [else #f])))
				     objs))))
			    #f)]))))])
    
    (sequence
      (apply super-init parent args))))

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
     (class100 (wx-make-window% item% #f) (window-style . args)
       (rename [super-on-set-focus on-set-focus]
	       [super-on-kill-focus on-kill-focus])
       (inherit get-width get-height get-x get-y
		get-parent get-client-size)
       (rename [super-enable enable]
	       [super-set-size set-size])
       (private-field [enabled? #t])
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
	 [is-enabled?
	  (lambda () enabled?)])

       (private-field
	 ; Store minimum size of item.  
	 ; This will never change after the item is created.
	 hard-min-width
	 hard-min-height)
       (public
	 [set-min-height (lambda (v) (set! hard-min-height v) (min-height v))]
	 [set-min-width (lambda (v) (set! hard-min-width v) (min-width v))]
	 [get-hard-minimum-size (lambda () (values hard-min-width hard-min-height))]
	 
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
	   [(new-width)
	    (check-range-integer '(method canvas<%> min-client-width) new-width)
	    (min-width (+ new-width (client-inset #f)))])]
	 [min-client-height
	  (case-lambda 
	   [() (- (min-height) (client-inset #t))]
	   [(new-height) 
	    (check-range-integer '(method canvas<%> min-client-height) new-height)
	    (min-height (+ new-height (client-inset #t)))])])

       (private-field [-mw 0]
		      [-mh 0]
		      [-xm x-margin-w]
		      [-ym y-margin-h]
		      [-sx stretch-x]
		      [-sy stretch-y]
		      [first-arg (car args)])

       (public
	 [min-width
	  (mk-param
	   -mw identity
	   (lambda (v)
	     (check-range-integer '(method area<%> min-width) v))
	   force-redraw)]
	 [min-height
	  (mk-param
	   -mh identity
	   (lambda (v)
	     (check-range-integer '(method area<%> min-height) v))
	   force-redraw)]
	 
	 [x-margin
	  (mk-param
	   -xm identity
	   (lambda (v)
	     (check-margin-integer '(method subarea<%> horiz-margin) v)
	     v)
	   force-redraw)]
	 [y-margin
	  (mk-param
	   -ym identity
	   (lambda (v) 
	     (check-margin-integer '(method subarea<%> vert-margin) v)
	     v)
	   force-redraw)]

	 [stretchable-in-x
	  (mk-param -sx (lambda (x) (and x #t)) void force-redraw)]
	 [stretchable-in-y
	  (mk-param -sy (lambda (x) (and x #t)) void force-redraw)]
	 
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
	 
	 [area-parent (lambda () first-arg)]

	 ; force-redraw: unconditionally trigger redraw.
	 ; input: none
	 ; returns: nothing
	 ; effects: forces the item's parent (if it exists) to redraw
	 ;   itself. This will recompute the min-size cache if it is
	 ;   invalid.
	 [force-redraw
	  (lambda ()
	    (let ([parent (area-parent)])
	      (when parent
		(send parent child-redraw-request this))))]
	 
	 [on-container-resize (lambda () (void))] ; This object doesn't contain anything

	 [init-min (lambda (x) x)]
	 
	 ; get-min-size: computes the minimum size the item can
	 ;   reasonably assume.
	 ; input: none
	 ; returns: a list containing the minimum width & height.
	 [get-min-size
	  (lambda ()
	    (let ([w (+ (* 2 (x-margin)) (max hard-min-width (min-width)))]
		  [h (+ (* 2 (y-margin)) (max hard-min-height (min-height)))])
	      (list w h)))])
       
       (sequence
	 (apply super-init (send (car args) get-window) (cdr args))
	 (set-min-width (init-min (get-width)))
	 (set-min-height (init-min (get-height)))

	 (unless (memq 'deleted window-style)
	   ;; For a pane[l], the creator must call the equivalent of the following,
	   ;;  delaying to let the panel's wx field get initialized before
	   ;;  panel-sizing methods are called
	   (unless (is-a? this wx-basic-panel<%>)
	     (send (area-parent) add-child this)))))))

; make-control% - for non-panel items
(define (make-control% item% x-margin y-margin
		       stretch-x stretch-y)
  (class100 (make-item% item% x-margin y-margin stretch-x stretch-y) args
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
  (parameterize ([wx:current-eventspace (send (send w get-top-level) get-eventspace)])
    (wx:queue-callback cb wx:middle-queue-key)))

(define wx<%> (interface () get-mred))
(define wx/proxy<%> (interface (wx<%>) get-proxy))

(define (make-glue% %)
  (class100* % (wx/proxy<%>) (mr prxy . args)
    (private-field [mred mr]
	     [proxy prxy])
    (public
      [get-mred (lambda () mred)]
      [get-proxy (lambda () proxy)])
    (sequence (apply super-init args))))

(define (make-window-glue% %)  ; implies make-glue%
  (class100 (make-glue% %) (mred proxy . args)
    (inherit get-x get-y get-width get-height area-parent get-mred get-proxy)
    (rename [super-on-size on-size]
	    [super-on-set-focus on-set-focus]
	    [super-on-kill-focus on-kill-focus]
	    [super-pre-on-char pre-on-char])
    (private-field
      [pre-wx->proxy (lambda (orig-w e k)
		       ;; MacOS: w may not be something the user knows
		       ;; Look for a parent, and shift coordinates
		       (let loop ([w orig-w])
			 (if w
			     (if (is-a? w wx/proxy<%>)
				 (if (eq? w orig-w)
				     (k (wx->proxy w) e)
				     (let ([bx (box (send e get-x))]
					   [by (box (send e get-y))])
				       (send orig-w client-to-screen bx by)
				       (send w screen-to-client bx by)
				       (let ([new-e (if (e . is-a? . wx:key-event%)
							(instantiate wx:key-event% ()
								     [key-code (send e get-key-code)])
							(instantiate wx:mouse-event% ()
								     [event-type (send e get-event-type)]
								     [left-down (send e get-left-down)]
								     [right-down (send e get-right-down)]
								     [middle-down (send e get-middle-down)]))])
					 (when (e . is-a? . wx:key-event%)
					   (send new-e set-key-release-code (send e get-key-release-code)))
					 (send new-e set-time-stamp (send e get-time-stamp))
					 (send new-e set-alt-down (send e get-alt-down))
					 (send new-e set-control-down (send e get-control-down))
					 (send new-e set-meta-down (send e get-meta-down))
					 (send new-e set-shift-down (send e get-shift-down))
					 (send new-e set-x (unbox bx))
					 (send new-e set-y (unbox by))
					 (k (wx->proxy w) new-e))))
				 (loop (send w get-parent)))
			     #f)))]
      [old-w -1]
      [old-h -1]
      [old-x -1]
      [old-y -1])
    (override
      [on-drop-file (entry-point
		     (lambda (f)
		       (as-exit
			(lambda ()
			  (send (get-proxy) on-drop-file f)))))]
      [on-size (lambda (bad-w bad-h)
		 (super-on-size bad-w bad-h)
		 ;; Delay callback to make sure X structures (position) are updated, first.
		 ;; Also, Windows needs a trampoline.
		 (queue-window-callback
		  this
		  (entry-point
		   (lambda ()
		      (let ([mred (get-mred)])
			(when mred 
			  (let* ([w (get-width)]
				 [h (get-height)])
			    (when (not (and (= w old-w) (= h old-h)))
			      (set! old-w w)
			      (set! old-h h)
			      (as-exit (lambda () (send mred on-size w h)))))
			  (let* ([p (area-parent)]
				 [x (- (get-x) (or (and p (send p dx)) 0))]
				 [y (- (get-y) (or (and p (send p dy)) 0))])
			    (when (not (and (= x old-x) (= y old-y)))
			      (set! old-x x)
			      (set! old-y y)
			      (as-exit (lambda () (send mred on-move x y)))))))))))]
      [on-set-focus (entry-point
		     (lambda ()
		       ; Windows circumvents the event queue to call on-focus
		       ;  when you click on the window's icon in the task bar.
		       (queue-window-callback
			this 
			(lambda () (send (get-proxy) on-focus #t)))
		       (as-exit (lambda () (super-on-set-focus)))))]
      [on-kill-focus (entry-point
		      (lambda ()
			; see on-set-focus:
			(queue-window-callback
			 this
			 (lambda () (send (get-proxy) on-focus #f)))
			(as-exit (lambda () (super-on-kill-focus)))))]
      [pre-on-char (lambda (w e)
		     (or (super-pre-on-char w e)
			 (as-entry
			  (lambda ()
			    (pre-wx->proxy w e
					   (lambda (m e)
					     (as-exit (lambda () 
							(send (get-proxy) on-subwindow-char m e)))))))))]
      [pre-on-event (entry-point
		     (lambda (w e)
		       (pre-wx->proxy w e
				      (lambda (m e) 
					(as-exit (lambda () 
						   (send (get-proxy) on-subwindow-event m e)))))))])
    (sequence (apply super-init mred proxy args))))

(define (make-container-glue% %)
  (class100 % (mr prxy . args)
    (inherit do-place-children do-get-graphical-min-size get-children-info get-hidden-child)
    (private-field [mred mr][proxy prxy])
    (override
      [get-graphical-min-size (lambda () 
				(cond
				 [mred (let ([info
					      (map (lambda (i)
						     (list (child-info-x-min i) (child-info-y-min i)
							   (child-info-x-stretch i) (child-info-y-stretch i)))
						   (get-children-info))])
					 (let-values ([(w h) (as-exit (lambda () (send mred container-size 
										       (if (get-hidden-child)
											   (cdr info) ; hidden child is first
											   info))))])
					   (list w h)))]
				 [else (do-get-graphical-min-size)]))]
      [place-children (lambda (l w h) 
			(cond
			 [(null? l) null]
			 [mred (as-exit (lambda () (send mred place-children l w h)))]
			 [else (do-place-children l w h)]))])
    (sequence
      (apply super-init mred proxy args))))

;; Weak boxed:
(define active-main-frame (make-weak-box #f))

;; An app-handler record keeps a wrapped procedure with
;; its original wrappee.
(define-values (struct:app-handler
		make-app-handler
		app-handler?
		app-handler-ref
		app-handler-set!)
  (make-struct-type 'app-handler
		    #f 2 0
		    #f null (current-inspector)
		    0))
(define (app-handler-orig ah)
  (app-handler-ref ah 1))

(let* ([running-quit? #f]
       [f (entry-point
	   (lambda ()
	     (unless running-quit?
	       (let ([af (weak-box-value active-main-frame)])
		 (when af
		   (set! running-quit? #t)
		   (queue-window-callback
		    af
		    (entry-point
		     (lambda ()
		       (dynamic-wind
			   void
			   (lambda () (send af on-exit))
			   (lambda () (set! running-quit? #f)))))))))))])
  (wx:application-quit-handler (make-app-handler f f)))

(define (set-handler! who proc param arity)
  (when proc
    (unless (and (procedure? proc)
		 (procedure-arity-includes? proc arity))
      (raise-type-error who
			(format "procedure (arity ~a) or #f" arity)
			proc)))
  (let ([e (wx:current-eventspace)])
    (when (wx:main-eventspace? e)
      (param (make-app-handler
	      (lambda args
		(parameterize ([wx:current-eventspace e])
		  (wx:queue-callback
		   (lambda () (apply proc args))
		   wx:middle-queue-key)))
	      proc)))))

(define application-preferences-handler
  (case-lambda
   [() (and (wx:main-eventspace? (wx:current-eventspace))
	    (app-handler-orig (wx:application-pref-handler)))]
   [(proc)
    (set-handler! 'application-preferences-handler proc
		  wx:application-pref-handler
		  0)]))

(define application-about-handler
  (case-lambda
   [() (or (and (wx:main-eventspace? (wx:current-eventspace))
		(app-handler-orig (wx:application-about-handler)))
	   void)]
   [(proc)
    (set-handler! 'application-about-handler proc
		  wx:application-about-handler
		  0)]))

(define application-quit-handler
  (case-lambda
   [() (or (and (wx:main-eventspace? (wx:current-eventspace))
		(app-handler-orig (wx:application-quit-handler)))
	   void)]
   [(proc)
    (set-handler! 'application-quit-handler proc
		  wx:application-quit-handler
		  0)]))

(define default-application-file-handler
  (entry-point
   (lambda (f)
     (let ([af (weak-box-value active-main-frame)])
       (when af
	 (queue-window-callback
	  af
	  (entry-point
	   (lambda () (when (send af accept-drag?)
			(send af on-drop-file f))))))))))

(define (install-defh)
  (wx:application-file-handler (make-app-handler
				default-application-file-handler
				default-application-file-handler)))
(install-defh)

(define application-file-handler
  (case-lambda
   [() (or (and (wx:main-eventspace? (wx:current-eventspace))
		(app-handler-orig (wx:application-file-handler)))
	   void)]
   [(proc)
    ;; Special case for default-application-file-handler,
    ;; because it need not be constrained to the main eventspace:
    (if (eq? proc default-application-file-handler)
	(install-defh)
	(set-handler! 'application-file-handler proc
		      wx:application-file-handler
		      1))]))


(define (current-eventspace-has-standard-menus?)
  (and (eq? 'macosx (system-type))
       (wx:main-eventspace? (wx:current-eventspace))))

(define (current-eventspace-has-menu-root?)
  (and (memq (system-type) '(macos macosx))
       (wx:main-eventspace? (wx:current-eventspace))))

(define root-menu-frame #f)

(define (eventspace-handler-thread e)
  (let ([t (wx:eventspace-handler-thread e)])
    (or t
	;; eventspace dead, or just no thread, yet?
	(with-handlers ([exn:fail?
			 (lambda (x)
			   (if (wx:eventspace-shutdown? e)
			       (raise-mismatch-error
				'eventspace-handler-thread
				"eventspace is shutdown: "
				e)
			       (raise x)))])
	  (let ([done (make-semaphore)]
		[t #f])
	    (parameterize ([wx:current-eventspace e])
	      (wx:queue-callback
	       (lambda () 
		 (set! t (current-thread))
		 (semaphore-post done))
	       #t)
	      (if (sync/timeout 1.0 done)
		  t
		  ;; Weird - no response after 1 second.  Maybe
		  ;; someone killed the handler thread before it could
		  ;; do our work? Or shutdown the eventspace? Or the
		  ;; thread is busy? In any of those cases, we'll
		  ;; succeed on the next iteration.
		  (eventspace-handler-thread e))))))))

(define (make-top-level-window-glue% %) ; implies make-window-glue%
  (class100 (make-window-glue% %) (mred proxy . args)
    (inherit is-shown? get-mred queue-visible get-eventspace)
    (rename [super-on-activate on-activate])
    (private-field 
      [act-date/seconds 0] [act-date/milliseconds 0] [act-on? #f])
    (public 
      [on-exit (entry-point
		(lambda ()
		  (and (is-shown?)
		       (let ([mred (get-mred)])
			 (and (and mred (as-exit (lambda () (send mred can-exit?))))
			      (as-exit (lambda () (send mred on-exit))))))))])
    (override
      [on-close (entry-point
		 (lambda ()
		   (let ([mred (get-mred)])
		     (if mred
			 (if (as-exit (lambda () (send mred can-close?)))
			     (begin
			       (as-exit (lambda () (send mred on-close)))
			       (queue-visible)
			       #t)
			     #f)
			 #t))))]
      [on-activate (entry-point
		    (lambda (on?)
		      (set! act-on? on?)
		      (when on?
			(set! act-date/seconds (current-seconds))
			(set! act-date/milliseconds (current-milliseconds))
			(when (wx:main-eventspace? (get-eventspace))
			  (set! active-main-frame (make-weak-box this))))
		      ;; Windows needs trampoline:
		      (queue-window-callback
		       this
		       (lambda () (send (get-mred) on-activate on?)))
		      (as-exit
		       (lambda ()
			 (super-on-activate on?)))))])
    (public
      [is-act-on? (lambda () act-on?)]
      [get-act-date/seconds (lambda () act-date/seconds)]
      [get-act-date/milliseconds (lambda () act-date/milliseconds)])
    (sequence (apply super-init mred proxy args))))

(define (make-canvas-glue% %) ; implies make-window-glue%
  (class100 (make-window-glue% %) (mred proxy . args)
    (inherit get-mred get-top-level)
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
      [on-char (entry-point
		(lambda (e)
		  (let ([mred (get-mred)])
		    (if mred
			(as-exit (lambda () (send mred on-char e)))
			(super-on-char e)))))]
      [on-event (entry-point
		 (lambda (e)
		   (let ([mred (get-mred)])
		     (if mred
			 (as-exit (lambda () (send mred on-event e)))
			 (as-exit (lambda () (super-on-event e)))))))]
      [on-scroll (entry-point
		  (lambda (e)
		    (let ([mred (get-mred)])
		      (if mred
			  ;; Delay callback for Windows scrollbar 
			  ;; and Windows/Mac trampoiline
			  (queue-window-callback
			   this
			   (lambda () (send mred on-scroll e)))
			  (as-exit (lambda () (super-on-scroll e)))))))]
      [on-paint (entry-point
		 (lambda ()
		   (let ([mred (get-mred)])
		     (if mred
			 (if (and (eq? 'windows (system-type))
				  (not (eq? (wx:current-eventspace)
					    (send (get-top-level) get-eventspace))))
			     ;; Windows circumvented the event queue; delay
			     (queue-window-callback
			      this
			      (lambda () (send mred on-paint)))
			     (as-exit (lambda () (send mred on-paint))))
			 (as-exit (lambda () (super-on-paint)))))))])
    (sequence (apply super-init mred proxy args))))

;------------- Create the actual wx classes -----------------

(define wx-frame%
  (make-top-level-window-glue% 
   (class100 (make-top-container% wx:frame% #f) args
     (rename [super-set-menu-bar set-menu-bar])
     (private-field
       [menu-bar #f]
       [is-mdi-parent? #f])
     (public
       [get-the-menu-bar (lambda () menu-bar)]
       [get-mdi-parent (lambda (x) x)]
       [set-mdi-parent (lambda (x) (and (set! is-mdi-parent? x) #t))])
     (override
       [set-menu-bar
	(lambda (mb)
	  (when mb (set! menu-bar mb))
	  (super-set-menu-bar mb))]
       [on-menu-command
	(entry-point
	 (lambda (id)
	   (let ([wx (wx:id-to-menu-item id)])
	     (let ([go (lambda ()
			 (do-command (wx->mred wx) (make-object wx:control-event% 'menu)))])
	       (if (eq? 'windows (system-type))
		   ;; Windows: need trampoline
		   (wx:queue-callback 
		    (entry-point (lambda () (go)))
		    wx:middle-queue-key)
		   (go))))))]
       [on-menu-click
	(entry-point
	 (lambda ()
	   ;; Windows: no trampoline needed
	   (and menu-bar (send menu-bar on-demand))))]
       [on-toolbar-click 
	(entry-point
	 (lambda ()
	   (as-exit (lambda () (send (wx->mred this) on-toolbar-button-click)))))])
     (public
       [handle-menu-key
	(lambda (event)
	  (and menu-bar 
	       ;; It can't be a menu event without a
	       ;; control, meta, or alt key...
	       (or (send event get-control-down)
		   (send event get-meta-down)
		   (send event get-alt-down))
	       (begin
		 (send menu-bar on-demand)
		 (send menu-bar handle-key event))))])
     (sequence
       (apply super-init args)))))

(define wx-dialog%
  (make-top-level-window-glue% 
   (class100 (make-top-container% wx:dialog% #t) args
     (sequence
       (apply super-init args)))))

(define wx-button% (make-window-glue% 
		    (class100 (make-simple-control% wx:button%) (parent cb label x y w h style)
		      (inherit command)
		      (private-field [border? (memq 'border style)])
		      (public [has-border? (lambda () border?)])
		      (override
			[char-to (lambda ()
				   (as-exit
				    (lambda ()
				      (command (make-object wx:control-event% 'button)))))])
		      (sequence (super-init style parent cb label x y w h style)))))
(define wx-check-box% (class100 (make-window-glue% (make-simple-control% wx:check-box%)) (mred proxy parent cb label x y w h style)
			(inherit set-value get-value command)
			(override
			  [char-to (lambda ()
				     (as-exit
				      (lambda ()
					(set-value (not (get-value)))
					(command (make-object wx:control-event% 'check-box)))))])
			(sequence (super-init mred proxy style parent cb label x y w h style))))
(define wx-choice% (class100 (make-window-glue% (make-simple-control% wx:choice%)) (mred proxy parent cb label x y w h choices style)
		     (override 
		       [handles-key-code 
			(lambda (x alpha? meta?) 
			  (or (memq x '(up down))
			      (and alpha? (not meta?))))])
		     (sequence (super-init mred proxy style parent cb label x y w h choices style))))
(define wx-message% (class100 (make-window-glue% (make-simple-control% wx:message%)) (mred proxy parent label x y style)
		      (override [gets-focus? (lambda () #f)])
		      (sequence (super-init mred proxy style parent label x y style))))

(define wx-gauge%
 (make-window-glue% 
  (class100 (make-control% wx:gauge% 
			   const-default-x-margin const-default-y-margin 
			   #f #f)
      (parent label range style)
    (inherit get-client-size get-width get-height set-size 
	     stretchable-in-x stretchable-in-y set-min-height set-min-width
	     get-parent)
    (override [gets-focus? (lambda () #f)])
    (private-field
      ; # pixels per unit of value.
      [pixels-per-value 1])
    (sequence
      (super-init style parent label range -1 -1 -1 -1 style)

      (let-values ([(client-width client-height) (get-two-int-values 
						  (lambda (a b) (get-client-size a b)))])
	(let ([delta-w (- (get-width) client-width)]
	      [delta-h (- (get-height) client-height)]
	      [vertical-labels? (eq? (send (send (get-parent) get-window) get-label-position) 'vertical)]
	      [horizontal? (memq 'horizontal style)])
	  (set-min-width (if horizontal?
			     (let ([cw (min const-max-gauge-length
					    (* range pixels-per-value))])
			       (max (if vertical-labels?
					cw
					(+ cw delta-w))
				     (get-width)))
			     ; client-height is the default
			     ; dimension in the minor direction.
			     (+ client-width delta-w)))
	  (set-min-height (if horizontal?
			      (+ client-height delta-h)
			      (let ([ch (min const-max-gauge-length
					     (* range pixels-per-value))])
				(max (if vertical-labels?
					 (+ ch delta-h)
					 ch)
				     (get-height)))))))

      (if (memq 'horizontal style)
	  (begin
	    (stretchable-in-x #t)
	    (stretchable-in-y #f))
	  (begin
	    (stretchable-in-x #f)
	    (stretchable-in-y #t)))))))

(define list-box-wheel-step #f)

(define wx-list-box%
  (make-window-glue% 
   (class100 (make-control% wx:list-box%
			 const-default-x-margin const-default-y-margin 
			 #t #t) (parent cb label kind x y w h choices style)
     (rename
       [super-pre-on-char pre-on-char])
     (inherit get-first-item
	      set-first-visible-item)
     (private
       [scroll (lambda (dir)
		 (unless list-box-wheel-step
		   (set! list-box-wheel-step (get-preference '|MrEd:wheelStep| (lambda () 3)))
		   (unless (and (number? list-box-wheel-step)
				(exact? list-box-wheel-step)
				(integer? list-box-wheel-step)
				(<= 1 list-box-wheel-step 100))
		     (set! list-box-wheel-step 3)))
		 (let ([top (get-first-item)])
		   (set-first-visible-item (+ top (* list-box-wheel-step dir)))))])
     (override
       [handles-key-code (lambda (x alpha? meta?)
			   (case x
			     [(up down) #t]
			     [else (and alpha? (not meta?))]))]
       [pre-on-char (lambda (w e)
		      (or (super-pre-on-char w e)
			  (case (send e get-key-code)
			    [(wheel-up) (scroll -1) #t]
			    [(wheel-down) (scroll 1) #t]
			    [else #f])))])
     (sequence (super-init style parent cb label kind x y w h choices style)))))

(define wx-radio-box%
  (make-window-glue% 
   (class100 (make-simple-control% wx:radio-box%) (parent cb label x y w h choices major style)
     (inherit number orig-enable set-selection command)
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

     (private-field [is-vertical? (memq 'vertical style)])
     (public
       [vertical? (lambda () is-vertical?)]
       [char-to-button (lambda (i)
			 (as-exit
			  (lambda ()
			    (set-selection i)
			    (command (make-object wx:control-event% 'radio-box)))))])

     (sequence (super-init style parent cb label x y w h choices major style))

     (private-field [enable-vector (make-vector (number) #t)]))))

(define wx-slider%
  (make-window-glue% 
   (class100 (make-control% wx:slider% 
			 const-default-x-margin const-default-y-margin 
			 #f #f)
       (parent func label value min-val max-val style)
     (inherit set-min-width set-min-height stretchable-in-x stretchable-in-y
	      get-client-size get-width get-height get-parent)
     (private-field
       ; # pixels per possible setting.
       [pixels-per-value 3])
     ; 3 is good because with horizontal sliders under Xt, with 1 or 2
     ; pixels per value, the thumb is too small to display the number,
     ; which looks bad.
     
     (sequence
       (super-init style parent func label value min-val max-val -1 -1 -1 style)
       
       (let-values ([(client-w client-h) (get-two-int-values (lambda (a b)
							       (get-client-size a b)))])
	 (let* ([horizontal? (memq 'horizontal style)]
		[vertical-labels? (eq? (send (send (get-parent) get-window) get-label-position) 'vertical)]
		[range (+ (* pixels-per-value (add1 (- max-val min-val)))
			  (cond
			   [(and horizontal? (not vertical-labels?)) (- (get-width) client-w)]
			   [(and (not horizontal?) vertical-labels?) (- (get-height) client-h)]
			   [else 0]))])
	   ((if horizontal? (lambda (v) (set-min-width v)) (lambda (v) (set-min-height v)))
	    (max ((if horizontal? (lambda () (get-width)) (lambda () (get-height))))
		 (min const-max-gauge-length range)))
	   (stretchable-in-x horizontal?)
	   (stretchable-in-y (not horizontal?))))))))

(define wx-canvas% (make-canvas-glue%
		    (class100 (make-control% wx:canvas% 0 0 #t #t) (parent x y w h style)
		      (private-field
			[tabable? #f])
		      (public
			[on-tab-in (lambda () (send (wx->mred this) on-tab-in))]
			[get-tab-focus (lambda () tabable?)]
			[set-tab-focus (lambda (v) (set! tabable? v))])
		      (override
			[gets-focus? (lambda () tabable?)]
			[handles-key-code
			 (lambda (code alpha? meta?)
			   (or meta? (not tabable?)))])
		      (sequence
			(super-init style parent x y w h style)))))

;--------------------- tab group -------------------------

(define bg-color (wx:get-panel-background))
(define tab-v-space 2)
(define raise-h 2)

(define (scale-color c f)
  (make-object wx:color% 
	       (min 255 (floor (* f (send c red)))) 
	       (min 255 (floor (* f (send c green)))) 
	       (min 255 (floor (* f (send c blue))))))

(define trans-pen (send (wx:get-the-pen-list) find-or-create-pen "white" 0 'transparent))
(define light-pen (send (wx:get-the-pen-list) find-or-create-pen (scale-color bg-color #e1.35) 0 'solid))
(define dark-pen (send (wx:get-the-pen-list) find-or-create-pen (scale-color bg-color #e0.6) 0 'solid)) 
(define dark-brush (send (wx:get-the-brush-list) find-or-create-brush (scale-color bg-color #e0.8) 'solid))

(define canvas-based-tab-group%
  (class wx-canvas%
    (init mred proxy style parent call-back label tab-labels style-again)
    
    (define callback call-back)

    (define tabs (map wx:label->plain-label tab-labels))
    (define tab-widths #f)
    (define tab-height #f)
    
    (define font (let loop ([p parent])
		   (if (not (p . is-a? . wx:window%))
		       (loop (send p get-parent))
		       (send p get-control-font))))
    
    (inherit get-dc get-client-size get-mred
	     set-min-width set-min-height
	     set-tab-focus
	     set-background-to-gray)
    (rename [super-on-size on-size])
    
    (define selected 0)
    (define tracking-pos #f)
    (define tracking-hit? #f)
    
    (define regions #f)
    (define redo-regions? #f)

    (define border? (memq 'border style))
    
    (define/private (compute-sizes)
      (let ([dc (get-dc)])
	(let ([w+hs (map (lambda (lbl)
			   (let-values ([(w h d a) (send dc get-text-extent lbl font)])
			     (cons w h)))
			 tabs)])
	  (set! tab-widths (map car w+hs))
	  (let-values ([(sw sh sd sa) (send dc get-text-extent " " font)])
	    (let ([th (ceiling (+ (* 2 tab-v-space) (apply max 0 sh (map cdr w+hs))))])
	      (set! tab-height (if (even? th) th (add1 th))))))))
    
    (define/private (get-total-width)
      (apply + tab-height (* (length tabs) (+ raise-h raise-h tab-height)) tab-widths))
    
    (define/private (get-init-x)
      (let-values ([(w h) (my-get-client-size)]
		   [(tw) (get-total-width)])
	(/ (- w tw) 2)))

    (define/override (on-char e) (void))

    (define/override on-event
      (entry-point
       (lambda (e)
	 (cond
	  [(and (send e button-down?) tab-widths)
	   (set! tracking-pos (find-click (send e get-x) (send e get-y)))
	   (when tracking-pos
	     (set! tracking-hit? #t)
	     (update-tracking))]
	  [(and (send e dragging?) tracking-pos)
	   (let ([hit? (equal? tracking-pos (find-click (send e get-x) (send e get-y)))])
	     (unless (eq? tracking-hit? hit?)
	       (set! tracking-hit? hit?)
	       (update-tracking)))]
	  [(and (send e button-up?) tracking-pos
		(equal? tracking-pos (find-click (send e get-x) (send e get-y)))
		(not (= tracking-pos selected)))
	   ;; Button released for final selection
	   (let ([new tracking-pos])
	     (set! tracking-pos #f)
	     (set! tracking-hit? #f)
	     (set-selection new)
	     (as-exit
	      (lambda ()
		(callback this (make-object wx:control-event% 'tab-panel)))))]
	  ;; otherwise, turn off tracking...
	  [else
	   (when tracking-hit?
	     (set! tracking-hit? #f)
	     (update-tracking))
	   (set! tracking-pos #f)]))))
    
    (define/private (update-tracking)
      (let ([dc (get-dc)])
	(send dc set-clipping-region (list-ref regions tracking-pos))
	(on-paint)
	(send dc set-clipping-region #f)))
    
    (define tmp-rgn #f)
    
    (define/private (find-click x y)
      (ready-regions)
      (unless tmp-rgn
	(set! tmp-rgn (make-object wx:region% (get-dc))))
      (let loop ([rl regions][pos 0])
	(if (null? rl)
	    #f
	    (begin
	      (send tmp-rgn set-rectangle x y 1 1)
	      (send tmp-rgn intersect (car rl))
	      (if (send tmp-rgn is-empty?)
		  (loop (cdr rl) (add1 pos))
		  pos)))))
    
    (define/private (setup-regions)
      (let ([dc (get-dc)])
	(set! regions
	      (map (lambda (tpl r)
		     (let ([points (map (lambda (p) (make-object wx:point% (car p) (+ 2 raise-h (cadr p))))
					tpl)])
		       (send r set-polygon points))
		     r)
		   (draw-once #f 0 #f #f 0)
		   (if regions
		       regions
		       (map (lambda (x)
			      (make-object wx:region% dc))
			    tabs))))
	(set! redo-regions? #f)))

    (define/private (ready-regions)
      (unless (and regions (not redo-regions?)) 
	(setup-regions)))
    
    (define (draw-once dc w light? dark? inset)
      (let ([init-x (get-init-x)])
	(let loop ([x init-x][l tabs][wl tab-widths][pos 0])
	  (if (null? l)
	      null
	      (let ([next-x (+ x tab-height (car wl))]
		    [-sel-d (if (= pos selected) (- raise-h) 0)])
		(cons
		 (append
		  ;; start point
		  (list (list (+ x tab-height -sel-d inset) (+ 2 tab-height (- inset))))
		  ;; left line
		  (begin
		    (when (= pos selected)
		      (when light?
			(send dc draw-line 0 tab-height x tab-height)
			(send dc draw-line 0 (add1 tab-height) x (add1 tab-height))))
		    (let ([short (if (or (= pos 0) (= pos selected))
				     0
				     (+ (/ tab-height 2) 
					(if (= selected (sub1 pos))
					    raise-h
					    0)))])
		      (when light?
			(send dc draw-line (+ x short -sel-d) (- tab-height short) (+ x tab-height) -sel-d)
			(send dc draw-line (+ x short -sel-d 1) (- tab-height short) (+ x tab-height 1) -sel-d))
		      (list (list (+ x short -sel-d -2 inset) (- tab-height short -2 inset))
			    (list (+ x tab-height inset) (+ -sel-d inset)))))
		  ;; top line
		  (begin
		    (when light?
		      (send dc draw-line (+ x tab-height) -sel-d next-x -sel-d)
		      (send dc draw-line (+ x tab-height) (+ 1 -sel-d) next-x (+ 1 -sel-d)))
		    (list (list (+ 1 next-x (- inset)) (+ inset -sel-d))))
		  ;; right line
		  (let* ([short (if (= (add1 pos) selected)
				    (+ (/ tab-height 2) (sub1 raise-h))
				    0)]
			 [short-d (if (zero? short) 0 -1)])
		    (when dark?
		      (send dc draw-line (+ 1 next-x) (+ -sel-d 1) (- (+ next-x tab-height) short 1 -sel-d) (- tab-height short 1))
		      (send dc draw-line next-x (+ -sel-d 1) 
			    (- (+ next-x tab-height) short 2 -sel-d short-d) (- tab-height short 1 short-d)))
		    (list (list (- (+ next-x tab-height) -sel-d short (- short-d) -2 inset) (- tab-height short -2 inset))))
		  ;; end point
		  (begin
		    (when light?
		      (when (= pos selected)
			(send dc draw-line (+ next-x tab-height) tab-height w tab-height)
			(send dc draw-line (+ next-x tab-height) (add1 tab-height) w (add1 tab-height)))
		      (send dc draw-text (car l) (+ x tab-height) (- tab-v-space (if (= pos selected) raise-h 0))))
		    (list (list (+ next-x inset (if (= selected (add1 pos)) -2 0)) (+ 2 tab-height (- inset))))))
		 (loop next-x (cdr l) (cdr wl) (add1 pos))))))))

    
    (define/override on-paint
      (entry-point
       (lambda ()
	 (compute-sizes)
	 (let ([dc (get-dc)])
	   (send dc set-background bg-color)
	   (send dc set-font font)
	   (send dc clear)
	   (send dc set-origin 0 (+ 2 raise-h))
	   (when (and tracking-pos tracking-hit?)
	     (let ([b (send dc get-brush)])
	       (send dc set-brush dark-brush)
	       (send dc set-pen trans-pen)
	       (send dc draw-polygon (map (lambda (x) (make-object wx:point% (car x) (cadr x)))
					  (list-ref (draw-once #f 0 #f #f 1) tracking-pos)))
	       (send dc set-brush b)))
	   (let-values ([(w h) (my-get-client-size)])
	     (send dc set-pen light-pen)
	     (draw-once dc w #t #f 0)
	     (when border?
	       (when (> h tab-height)
		 (send dc draw-line 0 tab-height 0 h)
		 (send dc draw-line 1 tab-height 1 h)))
	     (send dc set-pen dark-pen)
	     (draw-once dc w #f #t 0)
	     (when border?
	       (when (> h tab-height)
		 (send dc draw-line (- w 1) tab-height (- w 1) (- h raise-h))
		 (send dc draw-line (- w 2) (+ 1 tab-height) (- w 2) (- h raise-h))
		 (send dc draw-line 0 (- h 3 raise-h) w (- h 3 raise-h))
		 (send dc draw-line 1 (- h 4 raise-h) w (- h 4 raise-h)))))
	   (send dc set-origin 0 0)))))    

    (define/override (on-size w h)
      (set! redo-regions? #t)
      (super-on-size w h))

    (define/private (my-get-client-size)
      (get-two-int-values (lambda (a b) (get-client-size a b))))
    
    (define/public (get-selection)
      selected)

    (define/public (set-selection i)
      (as-entry
       (lambda ()
	 (ready-regions)
	 (when (< -1 i (length regions))
	   (let* ([dc (get-dc)]
		  [r (make-object wx:region% dc)]
		  [old-rgn (list-ref regions selected)])
	     (set! selected i)
	     (send r union old-rgn)
	     (setup-regions)
	     (let ([new-rgn (list-ref regions selected)])
	       ;; Union the new and old regions and repaint:
	       (send r union new-rgn)
	       (send dc set-clipping-region r)
	       (on-paint)
	       (send dc set-clipping-region #f)))))))

    (define -append
      (entry-point
       (lambda (s)
	 (set! tabs (append tabs (list (wx:label->plain-label s))))
	 (set! tab-widths #f)
	 (set! regions #f)
	 (on-paint))))
    (public (-append append))

    (define/public (delete i)
      (as-entry
       (lambda ()
	 (set! tabs (let loop ([pos 0][tabs tabs])
		      (if (= i pos)
			  (cdr tabs)
			  (cons (car tabs) (loop (add1 pos) (cdr tabs))))))
	 (set! selected (min selected (max 0 (sub1 (length tabs)))))
	 (set! regions #f)
	 (set! tab-widths #f)
	 (on-paint))))

    (define/override (handles-key-code code alpha? meta?) 
      #f)

    (super-instantiate (mred proxy parent -1 -1 -1 -1 '(transparent)))

    (set-background-to-gray)

    (compute-sizes)
    (set-min-width (inexact->exact (ceiling (get-total-width))))
    (set-min-height (inexact->exact (ceiling (+ tab-height 9 raise-h))))
    (set-tab-focus #f)))

(define wx-tab-group% 
  (if (eq? 'unix (system-type))
      canvas-based-tab-group%
      (class (make-window-glue%
	      (make-control% wx:tab-group% 0 0 #t #t))
	     (define/override (gets-focus?) #f)
	     (super-instantiate ()))))

(define group-right-inset 4)

(define canvas-based-group-box%
  (class wx-canvas%
    (init mred proxy style parent label style-again)

    (define font (let loop ([p parent])
		   (if (not (p . is-a? . wx:window%))
		       (loop (send p get-parent))
		       (send p get-control-font))))
    
    (inherit get-dc get-client-size get-mred
	     set-min-width set-min-height
	     set-tab-focus
	     set-background-to-gray)
    (rename [super-on-size on-size])
    
    (define lbl label)

    (define lbl-w 0)
    (define lbl-h 0)

    (define/private (compute-sizes)
      (let ([dc (get-dc)])
	(let-values ([(w h d a) (send dc get-text-extent lbl font)])
	  (set! lbl-w w)
	  (set! lbl-h h))))

    (define/override (on-char e) (void))
    (define/override (on-event e) (void))
    
    (define/override on-paint
      (entry-point
       (lambda ()
	 (let ([dc (get-dc)])
	   (send dc set-background bg-color)
	   (send dc set-font font)
	   (send dc clear)
	   (send dc draw-text lbl group-right-inset 0)
	   (send dc set-pen light-pen)
	   (let-values ([(w h) (my-get-client-size)]
			[(tw th ta td) (send dc get-text-extent lbl)])
	     (send dc draw-line 
		   1 (/ lbl-h 2)
		   (- group-right-inset 2) (/ lbl-h 2))
	     (send dc draw-line
		   1 (/ lbl-h 2)
		   1 (- h 2))
	     (send dc draw-line
		   1 (- h 2)
		   (- w 2) (- h 2))
	     (send dc draw-line
		   (- w 2) (- h 2)
		   (- w 2) (/ lbl-h 2))
	     (send dc draw-line
		   (- w 2) (/ lbl-h 2)
		   (min (- w 2)
			(+ group-right-inset 4 tw))
		   (/ lbl-h 2)))))))

    (define/private (my-get-client-size)
      (get-two-int-values (lambda (a b) (get-client-size a b))))

    (define/override (handles-key-code code alpha? meta?) 
      #f)

    (define/public (set-label l)
      (set! lbl l)
      (on-paint))

    (super-instantiate (mred proxy parent -1 -1 -1 -1 '(transparent)))

    (set-background-to-gray)

    (compute-sizes)
    (set-min-width (inexact->exact (ceiling (+ lbl-w group-right-inset 4))))
    (set-min-height (inexact->exact (ceiling (+ lbl-h 6))))
    (set-tab-focus #f)))

(define wx-group-box%
  (if (eq? 'unix (system-type))
      canvas-based-group-box%
      (class (make-window-glue%
	      (make-control% wx:group-box% 0 0 #t #t))
	     (define/override (gets-focus?) #f)
	     (super-instantiate ()))))

;--------------------- wx media Classes -------------------------

(define (make-editor-canvas% %)
  (class100 % (parent x y w h name style spp init-buffer)
    (inherit get-editor force-redraw
	     call-as-primary-owner min-height get-size
	     get-hard-minimum-size set-min-height)
    (rename [super-set-editor set-editor]
	    [super-on-set-focus on-set-focus])
    (private-field
      [fixed-height? #f]
      [fixed-height-lines 0]
      [orig-hard #f]
      [single-line-canvas? #f]
      [tabable? #f])
    (override
      [on-container-resize (lambda ()
			     (let ([edit (get-editor)])
			       (when edit
				 (as-exit (lambda () (send edit on-display-size-when-ready))))))]
      [on-set-focus
       (entry-point
	(lambda ()
	  (as-exit (lambda () (super-on-set-focus)))
	  (let ([m (get-editor)])
	    (when m 
	      (let ([mred (wx->mred this)])
		(when mred
		  (as-exit (lambda () (send m set-active-canvas mred)))))))))]
      [set-editor
       (letrec ([l (case-lambda
		    [(edit) (l edit #t)]
		    [(edit redraw?)
		     (let ([old-edit (get-editor)])
		       (super-set-editor edit redraw?)
		       
		       (let ([mred (wx->mred this)])
			 (when mred
			   (when old-edit
			     (as-exit
			      (lambda () (send old-edit remove-canvas mred))))
			   (when edit
			     (as-exit
			      (lambda () (send edit add-canvas mred))))))

		       (update-size)
		       
		       ;; force-redraw causes on-container-resize to be called,
		       ;;  but only when the size of the canvas really matters
		       ;;  (i.e., when it is shown)
		       (force-redraw))])])
	 l)]
      [handles-key-code 
       (lambda (x alpha? meta?)
	 (case x
	   [(#\tab #\return escape) (and (not tabable?)
					 (not single-line-canvas?))]
	   [else (not meta?)]))]


      [popup-for-editor (entry-point
			 (lambda (e m)
			   (let ([mwx (mred->wx m)])
			     (and (send mwx popup-grab e)
				  (as-exit (lambda () (send m on-demand) #t))
				  mwx))))])
    (public
      [set-tabable (lambda (on?) (set! tabable? on?))]
      [is-tabable? (lambda () tabable?)]
      [on-tab-in (lambda () 
		   (let ([mred (wx->mred this)])
		     (when mred
		       (send mred on-tab-in))))]
      [set-single-line (lambda () (set! single-line-canvas? #t))]
      [is-single-line? (lambda () single-line-canvas?)]
      [set-line-count (lambda (n)
			(if n
			    (begin
			      (unless orig-hard
				(let-values ([(hmw hmh) (get-hard-minimum-size)])
				  (set! orig-hard hmh)))
			      (set! fixed-height? #t)
			      (set! fixed-height-lines n))
			    (when orig-hard
			      (set! fixed-height? #f)
			      (set-min-height orig-hard)))
			(update-size))]
      [get-line-count (lambda () (and fixed-height? fixed-height-lines))]
      [update-size
       (lambda ()
	 (let ([edit (get-editor)])
	   (when (and edit fixed-height?)
	     (let* ([top (if (is-a? edit text%)
			     (send edit line-location 0 #t)
			     0)]
		    [bottom (if (is-a? edit text%)
				(send edit line-location 0 #f)
				14)]
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
		   (set-min-height (inexact->exact (round new-min-height)))
		   (force-redraw)))))))])
    (rename [super-set-y-margin set-y-margin])
    (override
      [set-y-margin (lambda (m)
		      (super-set-y-margin m)
		      (when fixed-height? (update-size)))])
    
    (sequence
      (super-init style parent x y w h (or name "") style spp init-buffer)
      (when init-buffer
	(let ([mred (wx->mred this)])
	  (when mred
	    (as-exit (lambda () (send init-buffer add-canvas mred)))))))))

(define wx-editor-canvas% (make-canvas-glue%
			   (make-editor-canvas% (make-control% wx:editor-canvas%
							       0 0 #t #t))))

(define internal-editor<%> (interface ()))
(define editor<%> (interface (wx:editor<%>)
		    get-canvases
		    get-active-canvas set-active-canvas
		    get-canvas
		    add-canvas remove-canvas
		    auto-wrap get-max-view-size))

(define-local-member-name 
  -format-filter
  -get-current-format
  -get-file-format
  -set-file-format
  -set-format)
		    
(define (make-editor-buffer% % can-wrap? get-editor%)
  ; >>> This class is instantiated directly by the end-user <<<
  (class100* % (editor<%> internal-editor<%>) args
    (inherit get-max-width set-max-width get-admin get-view-size
	     get-keymap get-style-list
	     can-load-file? on-load-file after-load-file
	     set-modified set-filename)
    (rename [super-on-display-size on-display-size]
	    [super-get-view-size get-view-size]
	    [super-copy-self-to copy-self-to]
	    [super-print print]
	    [super-get-filename get-filename]
	    [super-begin-edit-sequence begin-edit-sequence]
	    [super-end-edit-sequence end-edit-sequence]
	    [super-erase erase]
	    [super-insert-port insert-port]
	    [super-clear-undos clear-undos]
	    [super-get-load-overwrites-styles get-load-overwrites-styles])
    (private-field
      [canvases null]
      [active-canvas #f]
      [auto-set-wrap? #f])
    (private
      [max-view-size
       (lambda ()
	 (let ([wb (box 0)]
	       [hb (box 0)])
	   (super-get-view-size wb hb)
	   (unless (or (null? canvases) (null? (cdr canvases)))
	     (for-each
	      (lambda (canvas)
		(send canvas call-as-primary-owner
		      (lambda ()
			(let ([wb2 (box 0)]
			      [hb2 (box 0)])
			  (super-get-view-size wb2 hb2)
			  (set-box! wb (max (unbox wb) (unbox wb2)))
			  (set-box! hb (max (unbox hb) (unbox hb2)))))))
	      canvases))
	   (values (unbox wb) (unbox hb))))])
    (public
      [-format-filter (lambda (f) f)]
      [-set-file-format (lambda (f) (void))]
      [-get-file-format (lambda () 'standard)])

    (override
      [insert-file
       (opt-lambda ([file #f] [format 'guess] [show-errors? #t])
	 (dynamic-wind
	     (lambda () (super-begin-edit-sequence))
	     (lambda () (super-insert-port file format #f))
	     (lambda () (super-end-edit-sequence))))]

      [load-file
       (opt-lambda ([file #f] [format 'guess] [show-errors? #t])
	 (let* ([temp-filename?-box (box #f)]
		[old-filename (super-get-filename temp-filename?-box)])
	   (let* ([file (cond
			 [(or (not (path-string? file))
			      (equal? file ""))
			  (if (or (equal? file "") (not old-filename) (unbox temp-filename?-box))
			      (let ([path (if old-filename
					      (path-only old-filename)
					      #f)])
				(get-file path))
			      old-filename)]
			 [(path? file) file]
			 [else (string->path file)])])
	     (and 
	      file
	      (can-load-file? file (-format-filter format))
	      (begin
		(on-load-file file (-format-filter format))
		(let ([port (open-input-file file)]
		      [finished? #f])
		  (dynamic-wind
		      void
		      (lambda ()
			(wx:begin-busy-cursor)
			(super-begin-edit-sequence)
			(dynamic-wind
			    void
			    (lambda ()
			      (super-erase)
			      (unless (and (not (unbox temp-filename?-box))
					   (equal? file old-filename))
				(set-filename file #f))
			      (let ([format (if (eq? format 'same)
						(-get-file-format)
						format)])
				(let ([new-format (super-insert-port port 
								     (-format-filter format) 
								     (super-get-load-overwrites-styles))])
				  (close-input-port port) ; close as soon as possible
				  (-set-file-format new-format)))) ; text% only
			    (lambda ()
			      (super-end-edit-sequence)
			      (wx:end-busy-cursor)))
			(super-clear-undos)
			(set-modified #f)
			(set! finished? #t)
			#t)
		      (lambda ()
			(after-load-file finished?)
			;; In case it wasn't closed before:
			(close-input-port port)))))))))])

    (public
      [get-canvases (entry-point (lambda () (map wx->mred canvases)))]
      [get-active-canvas (entry-point (lambda () (and active-canvas (wx->mred active-canvas))))]
      [get-canvas
       (entry-point
	(lambda ()
	  (let ([c (or active-canvas
		       (and (not (null? canvases))
			    (car canvases)))])
	    (and c (wx->mred c)))))]
      [set-active-canvas
       (entry-point
	(lambda (new-canvas)
	  (check-instance '(method editor<%> set-active-canvas) editor-canvas% 'editor-canvas% #t new-canvas)
	  (set! active-canvas (mred->wx new-canvas))))]

      [add-canvas
       (entry-point
	(lambda (new-canvas)
	  (check-instance '(method editor<%> add-canvas) editor-canvas% 'editor-canvas% #f new-canvas)
	  (let ([new-canvas (mred->wx new-canvas)])
	    (unless (memq new-canvas canvases)
	      (set! canvases (cons new-canvas canvases))))))]

      [remove-canvas
       (entry-point
	(lambda (old-canvas)
	  (check-instance '(method editor<%> remove-canvas) editor-canvas% 'editor-canvas% #f old-canvas)
	  (let ([old-canvas (mred->wx old-canvas)])
	    (when (eq? old-canvas active-canvas)
	      (set! active-canvas #f))
	    (set! canvases (remq old-canvas canvases)))))]

      [auto-wrap (case-lambda
		  [() auto-set-wrap?]
		  [(on?) (as-entry
			  (lambda ()
			    (set! auto-set-wrap? (and on? #t))
			    (as-exit
			     (lambda ()
			       (if on?
				   (on-display-size)
				   (set-max-width 'none))))))])]
      [get-max-view-size (entry-point (lambda () (max-view-size)))])
    (override
      [copy-self
       (lambda () (let ([e (make-object (get-editor%))])
		    (copy-self-to e)
		    e))]
      [copy-self-to
       (lambda (e)
	 (super-copy-self-to e)
	 (send e auto-wrap auto-set-wrap?))]
      [on-display-size
       (entry-point
	(lambda ()
	  (as-exit (lambda () (super-on-display-size)))
	  (when (as-exit (lambda () (get-admin)))
	    (when (and can-wrap? auto-set-wrap?)
	      (let-values ([(current-width) (as-exit (lambda () (get-max-width)))]
			   [(new-width new-height) (max-view-size)])
		(when (and (not (= current-width new-width))
			   (< 0 new-width))
		  (as-exit (lambda () (set-max-width new-width)))))))))])
    
    (private
      [sp (lambda (x y z f b?)
	    ;; let super method report z errors:
	    (let ([zok? (memq z '(standard postscript))])
	      (when zok?
		(check-top-level-parent/false '(method editor<%> print) f))
	      (let ([p (and zok? f (mred->wx f))])
		(as-exit (lambda () (super-print x y z p b?))))))])

    (override
      [print
       (entry-point
	(case-lambda 
	 [() (sp #t #t 'standard #f #t)]
	 [(x) (sp x #t 'standard #f #t)]
	 [(x y) (sp x y 'standard #f #t)]
	 [(x y z) (sp x y z #f #t)]
	 [(x y z f) (sp x y z f #t)]
	 [(x y z f b?) (sp x y z f b?)]))]

      [on-new-box
       (entry-point
	(lambda (type)
	  (unless (memq type '(text pasteboard))
	    (raise-type-error (who->name '(method editor<%> on-new-box)) "symbol: text or pasteboard" type))
	  (make-object editor-snip%
		       (let ([e (make-object (cond
					      [(eq? type 'pasteboard) pasteboard%]
					      [else text%]))])
			 (send e set-keymap (get-keymap))
			 (send e set-style-list (get-style-list))
			 e))))])

    (sequence (apply super-init args))))

  (define text%
    (class100 (lock-contract-mixin
               (es-contract-mixin
                (make-editor-buffer% wx:text% #t  (lambda () text%))))
      ([line-spacing 1.0] 
       [tab-stops null]
       [auto-wrap #f])
      (rename (super-auto-wrap auto-wrap)
              (super-set-file-format set-file-format)
              (super-get-file-format get-file-format)
              (super-set-position set-position))
      (override
        [-get-file-format (lambda ()
                            (super-get-file-format))]
        [-set-file-format (lambda (format)
                            (super-set-file-format format)
                            (super-set-position 0 0))])
      
      (sequence (super-init line-spacing tab-stops)
                (when auto-wrap
                  (super-auto-wrap #t)))))
  
  (define pasteboard%
    (class100 (es-contract-mixin (make-editor-buffer% wx:pasteboard% #f (lambda () pasteboard%))) ()
      (override
        [-format-filter (lambda (f) 'standard)])
      (sequence (super-init))))

(define editor-snip% (class100 wx:editor-snip% ([editor #f]
						[with-border? #t]
						[left-margin 5]
						[top-margin 5]
						[right-margin 5]
						[bottom-margin 5]
						[left-inset 1]
						[top-inset 1]
						[right-inset 1]
						[bottom-inset 1]
						[min-width 'none]
						[max-width 'none]
						[min-height 'none]
						[max-height 'none])
		       (sequence
			 (super-init (or editor (make-object text%))
				     with-border?
				     left-margin
				     top-margin
				     right-margin
				     bottom-margin
				     left-inset
				     top-inset
				     right-inset
				     bottom-inset
				     min-width
				     max-width
				     min-height
				     max-height))))

(wx:set-editor-snip-maker (lambda args (apply make-object editor-snip% args)))
(wx:set-text-editor-maker (lambda () (make-object text%)))
(wx:set-pasteboard-editor-maker (lambda () (make-object pasteboard%)))

;--------------------- wx Panel Classes -------------------------

(define wx:windowless-panel%
  (class100 object% (prnt x y w h style)
    (private-field
      [pos-x 0] [pos-y 0] [width 1] [height 1]
      [parent prnt])
    (public
      [drag-accept-files (lambda () (void))]
      [on-drop-file (lambda () (void))]
      [on-set-focus (lambda () (void))]
      [on-kill-focus (lambda () (void))]
      [set-focus (lambda () (void))]
      [on-size (lambda () (void))]
      [enable (lambda () (void))]
      [show (lambda (on?) (void))]
      [is-shown-to-root? (lambda () (send parent is-shown-to-root?))]
      [is-enabled-to-root? (lambda () (send parent is-enabled-to-root?))]
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
      [get-height (lambda () height)])
    (sequence (super-init))))

(define wx-basic-panel<%> (interface ()))

(define tab-h-border (if (eq? (system-type) 'unix)
			 2
			 3))
(define tab-v-bottom-border (if (memq (system-type) '(macosx macos))
				2
				2))

(define (wx-make-basic-panel% wx:panel% stretch?)
  (class100* (wx-make-container% (make-item% wx:panel% 0 0 stretch? stretch?)) (wx-basic-panel<%>) (parent style)
    (inherit get-x get-y get-width get-height
	     min-width min-height set-min-width set-min-height
	     x-margin y-margin
	     get-client-size area-parent
	     get-hard-minimum-size)
    
    (rename [super-set-focus set-focus])
    
    (private-field
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
	     (send (car children) set-focus)))]

      [ext-dx (lambda () (if hidden-child
			     tab-h-border
			     0))]
      [ext-dy (lambda () (if hidden-child
			     (let-values ([(mw mh) (get-hard-minimum-size)])
			       (- mh tab-v-bottom-border 1))
			     0))])
    
    (private-field
      ;; list of panel's contents.
      [children null]
      [hidden-child #f]
      [curr-border const-default-border]
      [border? (memq 'border style)])
      
    (public
      [need-move-children (lambda () (set! move-children? #t))]

      [get-children (lambda () children)]
      [get-hidden-child (lambda () hidden-child)]
      [set-first-child-is-hidden (lambda ()
				   (set! hidden-child (car children))
				   (let ([i (send hidden-child get-info)])
				     (set-min-width (child-info-x-min i))
				     (set-min-height (child-info-y-min i))))]

      [border
       (case-lambda
	[() curr-border]
	[(new-val)
	 (check-margin-integer '(method area-container<%> border) new-val)
	 (set! curr-border new-val)
	 (force-redraw)])]

      ; add-child: adds an existing child to the panel.
      ; input: new-child: item% descendant to add
      ; returns: nothing
      ; effects: adds new-child to end of list of children.
      [add-child
       (lambda (new-child)
	 (unless (eq? this (send new-child area-parent))
	   (raise-mismatch-error 'add-child 
				 "not a child of this container: "
				 (wx->proxy new-child)))
	 (when (memq new-child children)
	   (raise-mismatch-error 'add-child "child already active: "
				 (wx->proxy new-child)))
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
	 (let ([new-children (f children)]) ;; hidden child, if any , must be first!
	   (unless (andmap (lambda (child)
			     (eq? this (send child area-parent)))
			   new-children)
	     (raise-mismatch-error 'change-children
				   (format
				    (string-append 
				     "not all members of the returned list are "
				     "children of the container ~e; list: ")
				    (wx->proxy this))
				   (map wx->proxy (remq hidden-child new-children))))
	   (let loop ([l new-children])
	     (unless (null? l)
	       (if (memq (car l) (cdr l))
		   (raise-mismatch-error 'change-children 
					 "child in the returned list twice: " 
					 (wx->proxy (car l)))
		   (loop (cdr l)))))
	   ; show all new children, hide all deleted children.
	   (let ([added-children (list-diff new-children children)]
		 [removed-children (list-diff children new-children)])
	     (let ([non-window (ormap (lambda (child)
					(and (not (is-a? child wx:window%))
					     child))
				      removed-children)])
	       (when non-window
		 (raise-mismatch-error 'change-children
				       (format "cannot delete non-window area in ~e: "
					       (wx->proxy this))
				       non-window)))

	     ;; Newly-added children may have been removed when
	     ;;  disabled, or now added into a disabled panel:
	     (for-each (lambda (child) (send child queue-active))
		       added-children)

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
	 (unless (memq child children)
	   (raise-mismatch-error 'delete-child 
				 "not a child of this container or child is not active: " 
				 (wx->proxy child)))
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
	   (let* ([childs children]
		  [info (map (lambda (child)
			       (send child get-info))
			     childs)])
	     (if (and (= (length childs) (length children))
		      (andmap eq? childs children))
		 ;; Got the info for the right set of children
		 (set! children-info info)
		 
		 ;; During the call to some get-info, the set of children changed;
		 ;; try again
		 (get-children-info))))
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
		    (lambda (kid-info x-accum y-accum first?)
		      (if (null? kid-info)
			  (list x-accum y-accum)
			  (gms-help
			   (cdr kid-info)
			   (compute-x x-accum kid-info (and hidden-child first?))
			   (compute-y y-accum kid-info (and hidden-child first?))
			   #f)))])
	    (let-values ([(client-w client-h)
			  (get-two-int-values (lambda (a b) (get-client-size a b)))])
	      (let* ([border (border)]
		     [min-client-size
		      (gms-help (get-children-info)
				(* 2 border) (* 2 border)
				#t)]
		     [delta-w (- (get-width) client-w)]
		     [delta-h (- (get-height) client-h)])
		(list (+ delta-w (car min-client-size) (if hidden-child (* 2 tab-h-border) 0))
		      (+ delta-h (cadr min-client-size)))))))]
      
      ; do-get-min-graphical-size: poll children and return minimum possible
      ;   size, as required by the graphical representation of the tree,
      ;   of the panel.
      ; input: none
      ; returns: minimum full size (as a list, width & height) of the
      ;   container.
      ; effects: none
      [get-graphical-min-size (lambda () (void))]
      [do-get-graphical-min-size
       (lambda ()
	 (do-graphical-size 
	  (lambda (x-accum kid-info first?)
	    (max x-accum (+ (* 2 (border))
			    (child-info-x-min (car kid-info)))))
	  (lambda (y-accum kid-info first?)
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
    
      [on-container-resize
       (lambda ()
	 (let-values ([(client-width client-height)
		       (get-two-int-values (lambda (a b) (get-client-size a b)))])
	   (unless (and (number? curr-width)
			(number? curr-height)
			(= curr-width client-width)
			(= curr-height client-height)
			(not move-children?))
	     (set! curr-width client-width)
	     (set! curr-height client-height)
	     (set! move-children? #f)
	     (redraw client-width client-height))))]

      [init-min (lambda (x) (if border? 8 0))])
    
    (public
      ; place-children: determines where each child of panel should be
      ; placed.
      ; input: children-info: list of (int int bool bool)
      ;        width/height: size of panel's client area.
      ; returns: list of placement info for children; each item in list
      ;   is a list of 4 elements, consisting of child's x-posn,
      ;   y-posn, x-size, y-size (including margins).  Items are in same 
      ;   order as children-info list.
      [place-children (lambda (l w h) (void))]
      [check-place-children
       (lambda (children-info width height)
	 (unless (and (list? children-info)
		      (andmap (lambda (x) (and (list? x)
					       (= 4 (length x))
					        (integer? (car x)) (not (negative? (car x))) (exact? (car x))
					        (integer? (cadr x)) (not (negative? (cadr x))) (exact? (cadr x))))
			      children-info))
	   (raise-type-error (who->name '(method area-container-window<%> place-children))
			     "list of (list of non-negative-integer non-negative-integer boolean boolean)"
			     children-info))
	 (check-non-negative-integer '(method area-container-window<%> place-children) width)
	 (check-non-negative-integer '(method area-container-window<%> place-children) height))]
      [do-place-children
       (lambda (children-info width height)
	 (check-place-children children-info width height)
	 (let loop ([children-info children-info])
	   (if (null? children-info)
	       null
	       (let ([curr-info (car children-info)])
		 (cons
		  (list
		   0 0
		   (car curr-info) ; child-info-x-min
		   (cadr curr-info)) ; child-info-y-min
		  (loop (cdr children-info)))))))])

    (private-field
      [curr-spacing const-default-spacing])
      
    (public
      [spacing ; does nothing!
       (case-lambda
	[() curr-spacing]
	[(new-val)
	 (check-margin-integer '(method area-container<%> spacing) new-val)
	 (set! curr-spacing new-val)])]

      [do-align (lambda (h v set-h set-v)
		  (unless (memq h '(left center right))
		    (raise-type-error 'set-alignment "horizontal alignment symbol: left, center, or right" h))
		  (unless (memq v '(top center bottom))
		    (raise-type-error 'set-alignment "vertical alignment symbol: top, center, or bottom" v))
		  (set-h h)
		  (set-v (case v [(top) 'left] [(center) 'center] [(bottom) 'right])))]
      [alignment (lambda (h v) 
		   (do-align h v (lambda (h) (set! h-align h)) (lambda (h) (set! v-align v)))
		   (force-redraw))]
      [get-alignment (lambda () (values h-align v-align))]

      ; redraw: redraws panel and all children
      ; input: width, height: size of area area in panel.
      ; returns: nothing
      ; effects: places children at default positions in panel.
      [redraw
       (lambda (width height)
	 (let ([children-info (get-children-info)]
	       [children children]) ; keep list of children matching children-info
	   (let ([l (place-children (map (lambda (i)
					   (list (child-info-x-min i) (child-info-y-min i)
						 (child-info-x-stretch i) (child-info-y-stretch i)))
					 (if hidden-child
					     (cdr children-info)
					     children-info))
				    (if hidden-child
					(- width (* 2 tab-h-border))
					width)
				    (if hidden-child
					(- height (child-info-y-min (car children-info))) ;; 2-pixel border here, too
					height))])
	     (unless (and (list? l)
			  (= (length l) (- (length children-info) (if hidden-child 1 0)))
			  (andmap (lambda (x) (and (list? x)
						   (= 4 (length x))
						   (andmap (lambda (x) (and (integer? x) (exact? x))) x)))
				  l))
	       (raise-mismatch-error 'container-redraw 
				     "result from place-children is not a list of 4-integer lists with the correct length: "
				     l))
	     (when hidden-child
	       ;; This goes with the hack for macos and macosx below
	       (send hidden-child set-phantom-size width height))
	     (panel-redraw children children-info (if hidden-child
						      (cons (list 0 0 width 
								  (if (memq (system-type) '(macos macosx)) ;; Yucky hack
								      (child-info-y-min (car children-info)) 
								      height))
							    (let ([dy (child-info-y-min (car children-info))])
							      (map (lambda (i)
								     (list (+ (car i) tab-h-border)
									   (+ dy (cadr i) (- tab-v-bottom-border) -1)
									   (caddr i)
									   (cadddr i)))
								   l)))
						      l)))))]
      [panel-redraw
       (lambda (childs child-infos placements)
	 (for-each
	  (lambda (child info placement)
	    (let-values ([(x y w h) (apply values placement)])
	      (let ([minw (child-info-x-min info)]
		    [minh (child-info-y-min info)]
		    [xm (child-info-x-margin info)]
		    [ym (child-info-y-margin info)])
		(dynamic-wind
		 (lambda () (set! ignore-redraw-request? #t))
		 (lambda ()
		   (send child set-size
			 (max 0 (+ x xm)) (max 0 (+ y ym))
			 (- (max minw w) (* 2 xm))
			 (- (max minh h) (* 2 ym))))
		 (lambda () (set! ignore-redraw-request? #f)))
		(send child on-container-resize))))
	  childs
	  child-infos
	  placements))])
    (sequence
      (super-init style parent -1 -1 0 0 style))))

(define (wx-make-pane% wx:panel% stretch?)
  (class100 (make-container-glue% (make-glue% (wx-make-basic-panel% wx:panel% stretch?))) args
    (inherit get-parent get-x get-y need-move-children get-children)
    (rename [super-set-size set-size])
    (override
      [on-visible
       (lambda ()
	 (for-each (lambda (c) (send c queue-visible)) (get-children)))]
      [on-active
       (lambda ()
	 (for-each (lambda (c) (send c queue-active)) (get-children)))]

      [get-window (lambda () (send (get-parent) get-window))]
      [set-size (lambda (x y w h) 
		  (super-set-size x y w h)
		  (need-move-children))]
      [dx (lambda () (get-x))]
      [dy (lambda () (get-y))])
    (sequence
      (apply super-init args))))

(define (wx-make-panel% wx:panel%)
  (class100 (make-container-glue% (make-window-glue% (wx-make-basic-panel% wx:panel% #t))) args
    (rename [super-on-visible on-visible]
	    [super-on-active on-active])
    (inherit get-children)
    (override 
      [on-visible
       (lambda ()
	 (for-each (lambda (c) (send c queue-visible)) (get-children))
	 (super-on-visible))]
      [on-active
       (lambda ()
	 (for-each (lambda (c) (send c queue-active)) (get-children))
	 (super-on-active))])
    (sequence (apply super-init args))))

(define (wx-make-linear-panel% wx-panel%)
  (class100 wx-panel% args
    (private-field
      [major-align-pos 'left]
      [minor-align-pos 'center])
    
    (inherit force-redraw border get-width get-height
	     get-graphical-min-size)
    (private-field [curr-spacing const-default-spacing])
    (override
      [spacing
       (case-lambda
	[() curr-spacing]
	[(new-val)
	 (check-margin-integer '(method area-container<%> spacing) new-val)
	 (set! curr-spacing new-val)
	 (force-redraw)])])
    (public
      [minor-align (lambda (a) (set! minor-align-pos a) (force-redraw))]
      [major-align (lambda (a) (set! major-align-pos a) (force-redraw))]
      [major-offset (lambda (space)
		      (case major-align-pos
			[(center) (quotient space 2)]
			[(left) 0]
			[(right) space]))]
      [minor-offset (lambda (width size)
		      (case minor-align-pos
			[(center) (quotient (- width size) 2)]
			[(left) 0]
			[(right) (- width size)]))]
      
      [do-get-alignment (lambda (pick) (values (pick major-align-pos minor-align-pos)
					       (case (pick minor-align-pos major-align-pos)
						 [(left) 'top] [(center) 'center] [(right) 'bottom])))]

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
  (class100 wx-linear-panel% args
    (inherit major-align minor-align do-align do-get-alignment major-offset minor-offset
	     spacing border do-graphical-size place-linear-children check-place-children)
    (override
      [alignment (lambda (h v) (do-align h v 
					 (lambda (x) (major-align x)) 
					 (lambda (x) (minor-align x))))]
      [get-alignment (lambda () (do-get-alignment (lambda (x y) x)))]
      
      [do-get-graphical-min-size
       (lambda ()
	 (do-graphical-size 
	  (lambda (x-accum kid-info hidden?)
	    (+ x-accum (child-info-x-min (car kid-info))
	       (if (or hidden? (null? (cdr kid-info)))
		   0
		   (spacing))))
	  (lambda (y-accum kid-info hidden?)
	    (max y-accum
		 (+ (child-info-y-min (car kid-info))
		    (* 2 (border)))))))]
      [do-place-children
       (lambda (l w h)
	 (check-place-children l w h)
	 (place-linear-children l w h
				car    ; child-info-x-min
				caddr  ; child-info-x-stretch
				(lambda (s) (major-offset s))
				cadr   ; child-info-y-min
				cadddr ; child-info-y-stretch
				(lambda (s t) (minor-offset s t))
				(lambda (width height) width)
				(lambda (width height) height)
				(lambda (major minor) major)
				(lambda (major minor) minor)))])
    (sequence (apply super-init args))))

; vertical-panel%.  See horizontal-panel%, but reverse
; "horizontal" and "vertical."
(define (wx-make-vertical-panel% wx-linear-panel%)
  (class100 wx-linear-panel% args
    (inherit major-align minor-align do-align do-get-alignment major-offset minor-offset
	     spacing border do-graphical-size place-linear-children check-place-children)
    (override
      [alignment (lambda (h v) (do-align h v 
					 (lambda (x) (minor-align x)) 
					 (lambda (x) (major-align x))))]
      [get-alignment (lambda () (do-get-alignment (lambda (x y) y)))]

      [do-get-graphical-min-size
       (lambda ()
	 (do-graphical-size
	  (lambda (x-accum kid-info hidden?)
	    (max x-accum
		 (+ (child-info-x-min (car kid-info))
		    (* 2 (border)))))
	  (lambda (y-accum kid-info hidden?)
	    (+ y-accum (child-info-y-min (car kid-info))
	       (if (or (null? (cdr kid-info)) hidden?)
		   0
		   (spacing))))))]
      
      [do-place-children
       (lambda (l w h)
	 (check-place-children l w h)
	 (place-linear-children l w h
				cadr   ; child-info-y-min
				cadddr ; child-info-y-stretch
				(lambda (s) (major-offset s))
				car    ; child-info-x-min
				caddr  ; child-info-x-stretch
				(lambda (s t) (minor-offset s t))
				(lambda (width height) height)
				(lambda (width height) width)
				(lambda (major minor) minor)
				(lambda (major minor) major)))])
    (sequence (apply super-init args))))

(define wx-panel% (wx-make-panel% wx:panel%))
(define wx-linear-panel% (wx-make-linear-panel% wx-panel%))
(define wx-horizontal-panel% (wx-make-horizontal-panel% wx-linear-panel%))
(define wx-vertical-panel% (wx-make-vertical-panel% wx-linear-panel%))

(define wx-pane% (wx-make-pane% wx:windowless-panel% #t))
(define wx-grow-box-pane%
  (class100 (wx-make-pane% wx:windowless-panel% #f) (mred proxy parent style)
	 (override
	   [init-min (lambda (x) (if (or (eq? (system-type) 'macos)
					 (eq? (system-type) 'macosx))
				     15
				     0))])
	 (sequence
	   (super-init mred proxy parent style))))
(define wx-linear-pane% (wx-make-linear-panel% wx-pane%))
(define wx-horizontal-pane% (wx-make-horizontal-panel% wx-linear-pane%))
(define wx-vertical-pane% (wx-make-vertical-panel% wx-linear-pane%))

;-------------------- Text control simulation -------------------------

(define text-field-text% 
  (class100 text% (cb ret-cb control set-cb-mgrs!)
      (rename [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-on-char on-char])
      (inherit get-text last-position)
      (private-field
	[return-cb ret-cb])
      (private-field
	[block-callback 1]
	[callback
	 (lambda (type)
	   (when (zero? block-callback)
	     (let ([e (make-object wx:control-event% type)])
	       (as-exit (lambda ()
			  (cb control e))))))])
      (override
	[on-char
	 (entry-point
	  (lambda (e)
	    (let ([c (send e get-key-code)])
	      (unless (and (or (eq? c #\return) (eq? c #\newline))
			   return-cb
			   (return-cb (lambda () (callback 'text-field-enter) #t)))
		(as-exit (lambda () (super-on-char e)))))))]
	[after-insert
	 (lambda args
	   (as-entry
	    (lambda ()
	      (as-exit (lambda () (super-after-insert . args)))
	      (callback 'text-field))))]
	[after-delete
	 (lambda args
	   (as-entry
	    (lambda ()
	      (as-exit (lambda () (super-after-delete . args)))
	      (callback 'text-field))))])
      (sequence
	(set-cb-mgrs!
	 (lambda (thunk)
	   (dynamic-wind
	    (lambda () (set! block-callback (add1 block-callback)))
	    thunk
	    (lambda () (set! block-callback (sub1 block-callback)))))
	 (lambda () 
	   (set! block-callback 0)))
	(super-init))))
  
(define wx-text-editor-canvas% 
  (class100 wx-editor-canvas% (mred proxy control parent style)
    (sequence
      (super-init mred proxy parent -1 -1 100 30 #f style 100 #f))))
  
(define (font->delta f)
  (define d (make-object wx:style-delta%))
  (let ([v (send f get-face)]
	[m (send f get-family)])
    (if v
	(send d set-delta-face v m)
	(send d set-delta 'change-family m)))
  (send d set-delta 'change-size (send f get-point-size))
  (send d set-delta 'change-style (send f get-style))
  (send d set-delta 'change-weight (send f get-weight))
  (send d set-delta 'change-underline (send f get-underlined))
  (send d set-delta 'change-smoothing (send f get-smoothing))
  (send d set-delta 'change-size-in-pixels (send f get-size-in-pixels))
  d)

(define wx-text-field%
  (class100 wx-horizontal-panel% (mred proxy parent fun label value style)
    ;; Make text field first because we'll have to exit
    ;;  for keymap initializer
    (private-field
      [func fun]
      [without-callback #f]
      [callback-ready #f]
      [e (make-object text-field-text%
		      func
		      (lambda (do-cb)
			(if multi?
			    #f
			    (do-cb)))
		      this
		      (lambda (wc cr)
			(set! without-callback wc)
			(set! callback-ready cr)))])
    (sequence
      (as-exit
       (lambda ()
	 ((current-text-keymap-initializer) (send e get-keymap)))))
    (inherit alignment stretchable-in-y get-control-font area-parent
	     get-min-size set-min-width set-min-height)
    (rename [super-place-children place-children])
    (public
      [command (lambda (e)  ; No entry/exit needed
		 (check-instance '(method text-field% command) wx:control-event% 'control-event% #f e)
		 (func this e)
		 (void))]

      [get-editor (lambda () e)]
      
      [get-value (lambda () (send e get-text))] ; note: not as-entry when called
      [set-value (lambda (v) (without-callback
			      (lambda () (send e insert v 0 (send e last-position)))))]

      [set-label (lambda (str) (when l (send l set-label str)))])
    (override
      ;; These might be called before we are fully initialized

      [set-cursor (lambda (c) (send e set-cursor c #t))]
      [set-focus (lambda () (when (object? c) (send c set-focus)))]
      
      [place-children
       (lambda (children-info width height)
	 (if (null? children-info)
	     null
	     (let ([r (super-place-children children-info width height)])
	       (if horiz?
		   ;; Line up label right with text:
		   (cons (list* (caar r) (+ (cadar r) dy) (cddar r))
			 (cdr r))
		   r))))])
    (sequence
      (super-init #f proxy parent (if (memq 'deleted style) '(deleted) null))
      (unless (memq 'deleted style)
	(send (area-parent) add-child this)))
    (private-field
      [multi? (memq 'multiple style)]
      [horiz? (cond
	       [(memq 'vertical-label style) #f]
	       [(memq 'horizontal-label style) #t]
	       [else (eq? (send (send parent get-window) get-label-position) 'horizontal)])]
      [dy 0]
      [p (if horiz?
	     this
	     (let ([p (make-object wx-vertical-pane% #f proxy this null)])
	       (send (send p area-parent) add-child p)
	       p))])
    (sequence
      (alignment 'left 'top)
      (unless horiz? (send p alignment 'left 'top))
      (unless multi? (stretchable-in-y #f)))
    (private-field
      [l (and label
	      (make-object wx-message% #f proxy p label -1 -1 null))]
      [c (make-object wx-text-editor-canvas% #f proxy this p
		      (append
		       '(control-border)
		       (if multi?
			   (if (memq 'hscroll style)
			       null
			       '(hide-hscroll))
			   '(hide-vscroll hide-hscroll))))])
    (sequence
      (send c set-x-margin 2)
      (send c set-y-margin 2)
      (send e set-line-spacing 0)
      (send e set-paste-text-only #t)
      (send e auto-wrap (and multi? (not (memq 'hscroll style))))
      (let ([f (get-control-font)]
	    [s (send (send e get-style-list) find-named-style "Standard")])
	(send s set-delta (let ([d (font->delta f)])
			    (if (memq 'password style)
				(begin
				  (send d set-face #f)
				  (send d set-family 'modern)
				  (send d set-delta-foreground "darkgray")
				  (send d set-delta-background "darkgray"))
				d))))
      (send c set-editor e)
      (send c set-line-count (if multi? 3 1))
      (unless multi? (send c set-single-line))

      (when (and l horiz?)
	;; Find amount to drop label down to line up the baselines:
	(let ([wbox (box 0)]
	      [hbox (box 0)]
	      [ybox (box 0)]
	      [abox (box 0)])
	  ; To bottom of first line
	  (send (send e get-admin) get-dc #f ybox)
	  (set! dy (+ -2 (abs (unbox ybox)) (send e line-location 0 #f))) ; -2 is fudge factor
	    
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
	  (set! dy (- dy (quotient (- (send l get-height) (unbox hbox)) 2)))

	  ; Exact
	  (set! dy (inexact->exact dy))))
      
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
	      (send c set-min-width (inexact->exact new-min-width))))))
      (let ([min-size (get-min-size)])
	(set-min-width (car min-size))
	(set-min-height (cadr min-size)))
      (callback-ready))))

;;;;;;;;;;;;;;;;;;;;;;;;; mred Class Construction ;;;;;;;;;;;;;;;;;;;;;;;;;

;------------ More helpers ---------------

(define wx-get-mred/gen (make-generic wx<%> 'get-mred))
(define wx-get-proxy/gen (make-generic wx/proxy<%> 'get-proxy))

(define (wx->mred w) (send-generic w wx-get-mred/gen))
(define (wx->proxy w) (send-generic w wx-get-proxy/gen))

(define-syntax (param stx)
  (syntax-case stx ()
    [(_ get-obj method)
     (syntax/loc stx
      (entry-point
       (case-lambda
	[() (send (get-obj) method)]
	[(v) (send (get-obj) method v)])))]))

(define (constructor-name who)
  (string->symbol (format "initialization for ~a%" who)))

(define (iconstructor-name who)
  (string->symbol (format "initialization for a class that implements ~a<%>" who)))

(define (check-container-parent who p)
  (unless (is-a? p internal-container<%>)
    (raise-type-error (who->name who) "built-in container<%> object" p)))

(define (check-top-level-parent/false who p)
  (unless (or (not p) (is-a? p frame%) (is-a? p dialog%))
    (raise-type-error (who->name who) "frame% or dialog% object or #f" p)))

(define (check-frame-parent/false who p)
  (unless (or (not p) (is-a? p frame%))
    (raise-type-error (who->name who) "frame% object or #f" p)))

(define (check-orientation cwho l)
  (check-style cwho '(vertical horizontal) '(vertical-label horizontal-label deleted) l))

(define (check-container-ready cwho p)
  (when p
    (let ([wx (mred->wx p)])
      (unless wx
	(raise-mismatch-error (who->name cwho)
			      "container is not yet fully initialized: " 
			      p)))))

(define double-boxed
  (lambda (x y f)
    (let ([x (box x)][y (box y)])
      (f x y)
      (values (unbox x) (unbox y)))))


(define-local-member-name private-wx)

(define mred%
  (class object%
    (init-field private-wx)
    (super-make-object)))

(define mred->wx (class-field-accessor mred% private-wx))

(define (mred->wx-container w) (send (mred->wx w) get-container))

(define (wrap-callback cb)
  (if (and (procedure? cb)
	     (procedure-arity-includes? cb 2))
      (lambda (w e) (if (or (eq? 'windows (system-type))
			    (and (memq (system-type) '(macos macosx))
				 (eq? (send e get-event-type) 'slider)))
			;; Mac OS slider and Windows (all): need trampoline
			(wx:queue-callback
			 (lambda ()
			   (cb (wx->proxy w) e))
			 wx:middle-queue-key)
			(cb (wx->proxy w) e)))
      cb))

;---------------- Keyword propagation macros -------------------

;; Since we use class100 to construct the classes that users see,
;; keywords are not propagated by position automatically. So we use
;; the class100*/kw macro for every class exported to the user; it
;; explicitly includes all keywords supported through superclasses.
;; To avoid writing the same keyword sets over and over, we have
;; a define-keywords form.

;; Arguably, this is making a problem (using `class100' instead of
;; `class') worse as much as it solves the problem. Or maybe the
;; problem is trying to hard to make by-position and by-name
;; initialization work.

(define-syntax (define-keywords stx)
  (syntax-case stx ()
    [(_ name kw ...)
     (with-syntax ([(kw2 ...)
		    (apply
		     append
		     (map (lambda (kw)
			    (if (identifier? kw)
				(syntax-local-value kw)
				(list kw)))
			  (syntax->list #'(kw ...))))])
       #'(define-syntax name '(kw2 ...)))]))

(define-syntax (class100*/kw stx)
  (syntax-case stx ()
    [(_ base (intf ...) ((base-init ...) keywords) . rest)
     (let ([kws (syntax-local-value #'keywords)])
       (with-syntax ([super-init (datum->syntax-object 
				  stx
				  'super-init
				  stx)]
		     [super-instantiate (datum->syntax-object 
					 stx
					 'super-instantiate
					 stx)]
		     [this (datum->syntax-object 
			    stx
			    'this)]
		     [(new-keyword ...) (map car kws)]
		     [(new-init ...) (datum->syntax-object 
				      stx
				      kws)])
	 #'(let-syntax ([super-init
			 (lambda (sstx)
			   (syntax-case sstx ()
			     [(_ arg (... ...))
			      (with-syntax ([super-instantiate
					     (datum->syntax-object 
					      sstx
					      'super-instantiate
					      sstx)]
					    [(new-kw (... ...))
					     (map (lambda (x)
						    (datum->syntax-object 
						     sstx
						     x))
						  '(new-keyword ...))])
				#'(super-instantiate (arg (... ...))
						     [new-kw new-kw] (... ...)))]))])
	     (class100*/names (this -hide-super-init super-instantiate)
			      base (intf ...) (base-init ... new-init ...)
			      . rest))))]))

;---------------- Window interfaces and base classes ------------

(define area<%>
  (interface ()
    get-parent get-top-level-window
    min-width min-height
    get-graphical-min-size
    stretchable-width stretchable-height))

(define-keywords area%-keywords 
  [min-width no-val]
  [min-height no-val]
  [stretchable-width no-val]
  [stretchable-height no-val])

(define area%
  (class100* mred% (area<%>) (mk-wx get-wx-pan mismatches prnt
				    ;; for keyword use:
				    [min-width no-val]
				    [min-height no-val]
				    [stretchable-width no-val]
				    [stretchable-height no-val])
    (sequence
      (let ([cwho '(iconstructor area)])
	(unless (eq? min-width no-val) (check-non#f-dimension cwho min-width))
	(unless (eq? min-height no-val) (check-non#f-dimension cwho min-height)))
      (mismatches))
    (private-field 
      [get-wx-panel get-wx-pan]
      [parent prnt])
    (public
      [get-parent (lambda () parent)]
      [get-top-level-window (entry-point (lambda () (wx->mred (send wx get-top-level))))]
      [(minw min-width) (param get-wx-panel min-width)]
      [(minh min-height) (param get-wx-panel min-height)]
      [(sw stretchable-width) (param get-wx-panel stretchable-in-x)]
      [(sh stretchable-height) (param get-wx-panel stretchable-in-y)]
      [get-graphical-min-size (entry-point (lambda () 
					     (if (wx . is-a? . wx-basic-panel<%>)
						 (apply values (send wx get-graphical-min-size))
						 (send wx get-hard-minimum-size))))])
    (private-field
      [wx (mk-wx)])
    (sequence
      (super-init wx)
      (unless (eq? min-width no-val) (minw min-width))
      (unless (eq? min-height no-val) (minh min-height))
      (unless (eq? stretchable-width no-val) (sw stretchable-width))
      (unless (eq? stretchable-height no-val) (sh stretchable-height)))))

(define internal-subarea<%> (interface ()))

(define subarea<%> 
  (interface (area<%> internal-subarea<%>)
    horiz-margin vert-margin))

(define-keywords subarea%-keywords
  [horiz-margin no-val]
  [vert-margin no-val])

(define (make-subarea% %) ; % implements area<%>
  (class100* % (subarea<%>) (mk-wx get-wx-pan mismatches parent
				   ;; for keyword use
				   [horiz-margin no-val]
				   [vert-margin no-val])
    (sequence
      (let ([cwho '(iconstructor subarea)])
	(unless (eq? horiz-margin no-val) (check-margin-integer cwho horiz-margin))
	(unless (eq? vert-margin no-val) (check-margin-integer cwho vert-margin))))
    (private-field [get-wx-panel get-wx-pan])
    (public
      [(hm horiz-margin) (param get-wx-panel x-margin)]
      [(vm vert-margin) (param get-wx-panel y-margin)])
    (sequence 
      (super-init mk-wx get-wx-panel mismatches parent)
      (unless (eq? horiz-margin no-val) (hm horiz-margin))
      (unless (eq? vert-margin no-val) (vm vert-margin)))))

(define area-container<%> 
  (interface (area<%>) 
    reflow-container container-flow-modified begin-container-sequence end-container-sequence
    container-size
    get-children change-children place-children
    after-new-child
    add-child delete-child
    border spacing 
    set-alignment get-alignment))

(define internal-container<%> (interface ()))

(define-keywords container%-keywords
  [border no-val]
  [spacing no-val]
  [alignment no-val])

(define (make-container% %) ; % implements area<%>
  (class100* % (area-container<%> internal-container<%>) (mk-wx get-wx-pan mismatches parent
								;; for keyword use
								[border no-val]
								[spacing no-val]
								[alignment no-val])
    (sequence
      (let ([cwho '(iconstructor area-container)])
	(unless (eq? border no-val) (check-margin-integer cwho border))
	(unless (eq? spacing no-val) (check-margin-integer cwho spacing))
	(unless (eq? alignment no-val)
	  (unless (and (list? alignment)
		       (= 2 (length alignment))
		       (memq (car alignment) '(left center right))
		       (memq (cadr alignment) '(top center bottom)))
	    (raise-type-error (who->name cwho) "alignment list" alignment)))))
    (private-field [get-wx-panel get-wx-pan])
    (public
      [after-new-child (lambda (c) 
			 (check-instance '(method area-container<%> after-new-child) subarea<%> 'subarea<%> #f c)
			 (void))]
      [reflow-container (entry-point (lambda () (send (send (get-wx-panel) get-top-level) force-redraw)))]
      [container-flow-modified (entry-point (lambda () 
					      (let ([p (get-wx-panel)])
						(send p need-move-children)
						(send p force-redraw))))]
      [begin-container-sequence (entry-point (lambda () (send (send (get-wx-panel) get-top-level) begin-container-sequence)))]
      [end-container-sequence (entry-point (lambda () (send (send (get-wx-panel) get-top-level) end-container-sequence)))]
      [get-children (entry-point (lambda () (map wx->proxy 
						 (let ([l (send (get-wx-panel) get-children)]
						       [h (send (get-wx-panel) get-hidden-child)])
						   (if h (remq h l) l)))))]
      [(bdr border) (param get-wx-panel border)]
      [(spc spacing) (param get-wx-panel spacing)]
      [set-alignment (entry-point (lambda (h v) (send (get-wx-panel) alignment h v)))]
      [get-alignment (entry-point (lambda () (send (get-wx-panel) get-alignment)))]
      [change-children (entry-point
			(lambda (f)
			  (unless (and (procedure? f)
				       (procedure-arity-includes? f 1))
			    (raise-type-error (who->name '(method container<%> change-children))
					      "procedure or arity 1"
					      f))
			  (send (get-wx-panel) change-children
				(lambda (kids)
				  (let* ([hidden (send (get-wx-panel) get-hidden-child)]
					 [mred-kids (map wx->proxy (remq hidden kids))]
					 [l (as-exit (lambda () (f mred-kids)))])
				    (unless (and (list? l)
						 (andmap (lambda (x) (is-a? x internal-subarea<%>)) l))
				      (raise-mismatch-error 'change-children
							    "result of given procedure was not a list of subareas: "
							    l))
				    (append
				     (if hidden (list hidden) null)
				     (map mred->wx l)))))))]
      [container-size (entry-point
		       (lambda (l) 
			 ; Check l, even though we don't use it
			 (unless (and (list? l) 
				      (andmap
				       (lambda (l)
					 (and (list? l) (= (length l) 4)
					      (integer? (car l)) (exact? (car l)) (<= 0 (car l) 10000)
					      (integer? (cadr l)) (exact? (cadr l)) (<= 0 (cadr l) 10000)))
				       l))
			   (raise-type-error (who->name '(method area-container<%> container-size))
					     "list of lists containing two exact integers in [0, 10000] and two booleans"
					     l))
			 (let ([l (send (get-wx-panel) do-get-graphical-min-size)])
			   (apply values l))))]
      [place-children (entry-point (lambda (l w h) (send (get-wx-panel) do-place-children l w h)))]
      [add-child (entry-point
		  (lambda (c) 
		    (check-instance '(method area-container<%> add-child) subwindow<%> 'subwindow<%> #f c)
		    (send (get-wx-panel) add-child (mred->wx c))))]
      [delete-child (entry-point
		     (lambda (c) 
		       (check-instance '(method area-container<%> delete-child) subwindow<%> 'subwindow<%> #f c)
		       (send (get-wx-panel) delete-child (mred->wx c))))])
    (sequence
      (super-init mk-wx get-wx-panel mismatches parent)
      (unless (eq? border no-val) (bdr border))
      (unless (eq? spacing no-val) (spc spacing))
      (unless (eq? alignment no-val) (set-alignment . alignment)))))

(define window<%>
  (interface (area<%>)
    on-focus focus has-focus?
    on-size on-move
    accept-drop-files on-drop-file
    on-subwindow-char on-subwindow-event
    client->screen screen->client
    enable is-enabled? on-superwindow-enable
    get-label set-label get-plain-label
    get-client-size get-size get-width get-height get-x get-y
    get-cursor set-cursor popup-menu
    show is-shown? on-superwindow-show refresh))

(define-keywords window%-keywords [enabled #t])

(define (make-window% top? %) ; % implements area<%>
  (class100* % (window<%>) (mk-wx get-wx-panel mismatches lbl parent crsr
				  ;; for keyword use
				  [enabled #t])
    (private-field [label lbl][cursor crsr])
    (public
      [popup-menu (entry-point 
		   (lambda (m x y)
		     (check-instance '(method window<%> popup-menu) popup-menu% 'popup-menu% #f m)
		     (check-range-integer '(method window<%> popup-menu) x)
		     (check-range-integer '(method window<%> popup-menu) y)
		     (let ([mwx (mred->wx m)])
		       (and (send mwx popup-grab this)
			    (as-exit
			     (lambda ()
			       (send m on-demand)
			       (send wx popup-menu mwx x y)))))))]
      [on-focus (lambda (x) (void))]
      [on-size (lambda (w h)
		 (check-range-integer '(method window<%> on-size) w)
		 (check-range-integer '(method window<%> on-size) h))]
      [on-move (lambda (x y)
		 (check-slider-integer '(method window<%> on-move) x)
		 (check-slider-integer '(method window<%> on-move) y))]
      [on-subwindow-char (lambda (w e)
			   (check-instance '(method window<%> on-subwindow-char) window<%> 'window<%> #f w)
			   (check-instance '(method window<%> on-subwindow-char) wx:key-event% 'key-event% #f e)
			   #f)]
      [on-subwindow-event (lambda (w e)
			   (check-instance '(method window<%> on-subwindow-event) window<%> 'window<%> #f w)
			   (check-instance '(method window<%> on-subwindow-event) wx:mouse-event% 'mouse-event% #f e)
			   #f)]
      [on-drop-file (lambda (s)
		      (unless (string? s)
			(raise-type-error (who->name '(method window<%> on-drop-file)) "pathname string" s)))]

      [focus (entry-point (lambda () (send wx set-focus)))]
      [has-focus? (entry-point (lambda () (send wx has-focus?)))]
      [enable (entry-point (lambda (on?) (send wx enable on?)))]
      [is-enabled? (entry-point (lambda () (send wx is-enabled?)))]
      
      [get-label (lambda () label)]
      [set-label (lambda (l)
		   (check-label-string/false '(method window<%> set-label) l)
		   (set! label (if (string? l)
				   (string->immutable-string l)
				   l)))]
      [get-plain-label (lambda () (and (string? label) (wx:label->plain-label label)))]

      [accept-drop-files
       (entry-point
	(case-lambda
	 [() (send wx accept-drag?)]
	 [(on?) (send wx drag-accept-files on?)]))]
      
      [client->screen (entry-point
		       (lambda (x y)
			 (check-slider-integer '(method window<%> client->screen) x)
			 (check-slider-integer '(method window<%> client->screen) y)
			 (double-boxed
			  x y
			  (lambda (x y) (send wx client-to-screen x y)))))]
      [screen->client (entry-point
		       (lambda (x y)
			 (check-slider-integer '(method window<%> screen->client) x)
			 (check-slider-integer '(method window<%> screen->client) y)
			 (double-boxed
			  x y
			  (lambda (x y) (send wx screen-to-client x y)))))]
      [get-client-size (entry-point
			(lambda ()
			  (double-boxed
			   0 0
			   (lambda (x y) (send wx get-client-size x y)))))]
      [get-size (entry-point
		 (lambda ()
		   (double-boxed
		    0 0
		    (lambda (x y) (send wx get-size x y)))))]
      
      [get-width (entry-point (lambda () (send wx get-width)))]
      [get-height (entry-point (lambda () (send wx get-height)))]
      [get-x (entry-point (lambda () (- (send wx get-x) (if top? 0 (send (send wx get-parent) ext-dx)))))]
      [get-y (entry-point (lambda () (- (send wx get-y) (if top? 0 (send (send wx get-parent) ext-dy)))))]
      
      [get-cursor (lambda () cursor)]
      [set-cursor (entry-point
		   (lambda (x)
		     (send wx set-cursor x)
		     (set! cursor x)))]

      [show (entry-point (lambda (on?) 
			     (when on?
			       (unless top?
				 (unless (memq wx (send (send wx area-parent) get-children))
				   (raise-mismatch-error 
				    (who->name '(method window<%> show))
				    "cannot show a subwindow that is not active in its parent: "
				    this))))
			     (send wx show on?)))]
      [is-shown? (entry-point (lambda () (send wx is-shown?)))]
      [on-superwindow-show (lambda (visible?) (void))]
      [on-superwindow-enable (lambda (active?) (void))]

      [refresh (entry-point (lambda () (send wx refresh)))])
    (private-field
      [wx #f])
    (sequence
      (super-init (lambda () (set! wx (mk-wx)) wx) get-wx-panel mismatches parent)
      (unless enabled (enable #f)))))

(define area-container-window<%>
  (interface (window<%> area-container<%>)
    set-control-font get-control-font
    set-label-font get-label-font
    set-label-position get-label-position))

(define (make-area-container-window% %) ; % implements window<%> (and area-container<%>)
  (class100* % (area-container-window<%>) (mk-wx get-wx-pan mismatches label parent cursor) 
    (private-field [get-wx-panel get-wx-pan])    
    (public
      [get-control-font (entry-point (lambda () (send (get-wx-panel) get-control-font)))]
      [set-control-font (entry-point (lambda (x) (send (get-wx-panel) set-control-font x)))]
      [get-label-font (entry-point (lambda () (send (get-wx-panel) get-label-font)))]
      [set-label-font (entry-point (lambda (x) (send (get-wx-panel) set-label-font x)))]
      [get-label-position (entry-point (lambda () (send (get-wx-panel) get-label-position)))]
      [set-label-position (entry-point (lambda (x) (send (get-wx-panel) set-label-position x)))])
    (sequence
      (super-init mk-wx get-wx-panel mismatches label parent cursor))))

(define top-level-window<%>
  (interface (area-container-window<%>)
    get-eventspace
    on-activate on-traverse-char on-system-menu-char
    can-close? on-close
    can-exit? on-exit
    get-focus-window get-edit-target-window
    get-focus-object get-edit-target-object
    center move resize
    on-message))

(define-keywords top-level-window%-keywords
  window%-keywords container%-keywords area%-keywords)

(define basic-top-level-window%
  (class100* (make-area-container-window% (make-window% #t (make-container% area%))) (top-level-window<%>) 
    (mk-wx mismatches label parent)
    (inherit show)
    (rename [super-set-label set-label])
    (private
      [wx-object->proxy
       (lambda (o)
	 (if (is-a? o wx:window%)
	     (wx->proxy o)
	     o))])
    (override
      [set-label (entry-point
		  (lambda (l)
		    (check-label-string/false '(method top-level-window<%> set-label) l)
		    (send wx set-title (or l ""))
		    (super-set-label l)))])
    (public
      [on-traverse-char (entry-point
			 (lambda (e)
			   (check-instance '(method top-level-window<%> on-traverse-char)
					   wx:key-event% 'key-event% #f e)
			   (send wx handle-traverse-key e)))]
      [on-system-menu-char (entry-point
			    (lambda (e)
			      (check-instance '(method top-level-window<%> on-system-menu-char) 
					      wx:key-event% 'key-event% #f e)
			      (and (eq? #\space (send e get-key-code))
				   (send e get-meta-down)
				   (eq? 'windows (system-type))
				   (send wx system-menu) #t)))]
      [get-eventspace (entry-point (lambda () (send wx get-eventspace)))]
      [can-close? (lambda () #t)]
      [can-exit? (lambda () (can-close?))]
      [on-close (lambda () (void))]
      [on-exit (lambda () (on-close) (show #f))]
      [on-activate (lambda (x) (void))]
      [center (entry-point
	       (case-lambda
		[() (send wx center 'both)]
		[(dir) (send wx center dir)]))]
      [move (entry-point
	     (lambda (x y)
	       (check-slider-integer '(method top-level-window<%> move) x)
	       (check-slider-integer '(method top-level-window<%> move) y)
	       (send wx move x y)))]
      [resize (entry-point
	       (lambda (w h)
		 (check-range-integer '(method top-level-window<%> resize) w)
		 (check-range-integer '(method top-level-window<%> resize) h)
		 (send wx set-size -1 -1 w h)))]

      [get-focus-window (entry-point
			 (lambda () (let ([w (send wx get-focus-window)])
				      (and w (wx->proxy w)))))]
      [get-edit-target-window (entry-point
			       (lambda () (let ([w (send wx get-edit-target-window)])
					    (and w (wx->proxy w)))))]
      [get-focus-object (entry-point
			 (lambda () (let ([o (send wx get-focus-object)])
				      (and o (wx-object->proxy o)))))]
      [get-edit-target-object (entry-point
			       (lambda () (let ([o (send wx get-edit-target-object)])
					    (and o (wx-object->proxy o)))))]

      [on-message (lambda (m) (void))])
    (private-field
      [wx #f]
      [wx-panel #f]
      [finish (entry-point
	       (lambda (top-level hide-panel?)
		 (set! wx-panel (make-object wx-vertical-panel% #f this top-level null))
		 (send (send wx-panel area-parent) add-child wx-panel)
		 (send top-level set-container wx-panel)
		 (when hide-panel?
		   (send wx-panel show #f))
		 top-level))])
    (sequence 
      (super-init (lambda () (set! wx (mk-wx finish)) wx) (lambda () wx-panel) mismatches label parent arrow-cursor))))

(define subwindow<%> 
  (interface (window<%> subarea<%>)))

(define control<%>
  (interface (subwindow<%>)
    command))

(define-local-member-name hidden-child? label-checker)

(define-keywords control%-keywords
  window%-keywords
  subarea%-keywords
  area%-keywords)

(define basic-control%
  (class100* (make-window% #f (make-subarea% area%)) (control<%>) (mk-wx mismatches lbl parent cb cursor)
    (rename [super-set-label set-label])
    (private-field [label lbl][callback cb])
    (override
      [get-label (lambda () label)]
      [get-plain-label (lambda () (and (string? label) (wx:label->plain-label label)))]
      [set-label (entry-point
		  (lambda (l)
		    ((label-checker)
		     '(method control<%> set-label) l)
		    (let ([l (if (string? l) 
				 (string->immutable-string l)
				 l)])
		      (send wx set-label l)
		      (set! label l))))])
    (public
      [hidden-child? (lambda () #f)] ; module-local method
      [label-checker  (lambda () check-label-string/false)] ; module-local method
      [command (lambda (e) (void (callback this e)))]) ; no entry/exit needed
    (private-field
      [wx #f])
    (sequence
      (when (string? label)
	(set! label (string->immutable-string label)))
      (super-init (lambda () (set! wx (mk-wx)) wx) (lambda () wx) mismatches label parent cursor)
      (unless (hidden-child?)
	(as-exit (lambda () (send parent after-new-child this)))))))

;--------------------- Final mred class construction --------------------
    
(define frame%
  (class100*/kw basic-top-level-window% ()
		[(label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null])
		 top-level-window%-keywords]
    (inherit on-traverse-char on-system-menu-char)
    (sequence
      (let ([cwho '(constructor frame)])
	(check-label-string cwho label)
	(check-frame-parent/false cwho parent)
	(check-dimension cwho width)
	(check-dimension cwho height)
	(check-init-pos-integer cwho x)
	(check-init-pos-integer cwho y)
	(check-style cwho #f '(no-resize-border no-caption no-system-menu mdi-parent mdi-child toolbar-button hide-menu-bar metal) 
		     style)
	(when (memq 'mdi-child style)
	  (when (memq 'mdi-parent style)
	    (raise-type-error (who->name cwho) 
			      "style list, 'mdi-child and 'mdi-parent are mutually exclusive" 
			      style)))))
    (rename [super-on-subwindow-char on-subwindow-char])
    (private-field
      [wx #f]
      [status-line? #f]
      [modified? #f])
    (override
      [on-subwindow-char (lambda (w event)
			   (super-on-subwindow-char w event)
			   (or (on-menu-char event)
			       (on-system-menu-char event)
			       (on-traverse-char event)))])
    (public
      [on-menu-char (entry-point
		     (lambda (e)
		       (check-instance '(method frame% on-menu-char) wx:key-event% 'key-event% #f e)
		       (send wx handle-menu-key e)))]
      [on-toolbar-button-click (lambda () (void))]
      [create-status-line (entry-point (lambda () (unless status-line? (send wx create-status-line) (set! status-line? #t))))]
      [set-status-text (lambda (s) (send wx set-status-text s))]
      [has-status-line? (lambda () status-line?)]
      [iconize (entry-point (lambda (on?) (send wx iconize on?)))]
      [is-iconized? (entry-point (lambda () (send wx iconized?)))]
      [set-icon (case-lambda 
		 [(i) (send wx set-icon i)]
		 [(i b) (send wx set-icon i b)]
		 [(i b l?) (send wx set-icon i b l?)])]
      [maximize (entry-point (lambda (on?) (send wx maximize on?)))]
      [get-menu-bar (entry-point (lambda () (let ([mb (send wx get-the-menu-bar)])
					      (and mb (wx->mred mb)))))]
      [modified (entry-point
		 (case-lambda
		  [() modified?]
		  [(m) 
		   (set! modified? m)
		   (send wx set-modified m)]))])
    (sequence
      (as-entry
       (lambda ()
	 (super-init 
	  (lambda (finish) 
	    (set! wx (finish (make-object wx-frame% this this
					  (and parent (mred->wx parent)) label
					  (or x -11111) (or y -11111)
					  (or width -1) (or height -1)
					  style)
			     (memq 'mdi-parent style)))
	    (send wx set-mdi-parent (memq 'mdi-parent style))
	    wx)
	  (lambda ()
	    (let ([cwho '(constructor frame)])
	      (check-container-ready cwho parent)
	      (when (memq 'mdi-child style)
		(let ([pwx (and parent (mred->wx parent))])
		  (unless (and pwx (send pwx get-mdi-parent))
		    (raise-mismatch-error (who->name cwho) "parent for 'mdi-child frame is not an 'mdi-parent frame: " parent))))))
	  label parent))))))

(define dialog%
  (class100*/kw basic-top-level-window% ()
		[(label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null])
		 top-level-window%-keywords]
    (inherit on-traverse-char on-system-menu-char)
    (sequence
      (let ([cwho '(constructor dialog)])
	(check-label-string cwho label)
	(check-top-level-parent/false cwho parent)
	(for-each (lambda (x) (check-dimension cwho x)) (list width height x y))
	(check-style cwho #f '(no-caption resize-border) style)))
    (rename [super-on-subwindow-char on-subwindow-char])
    (private-field [wx #f])
    (override
      [on-subwindow-char (lambda (w event)
			   (super-on-subwindow-char w event)
			   (or (on-system-menu-char event)
			       (on-traverse-char event)))])
    (sequence
      (as-entry
       (lambda ()
	 (super-init (lambda (finish) 
		       (set! wx (finish (make-object wx-dialog% this this
						     (and parent (mred->wx parent)) label #t
						     (or x -1) (or y -1) (or width 0) (or height 0)
						     style)
					#f))
		       wx)
		     (lambda () 
		       (let ([cwho '(constructor dialog)])
			 (check-container-ready cwho parent)))
		     label parent))))))

(define (get-top-level-windows)
  (remq root-menu-frame (map wx->mred (wx:get-top-level-windows))))

(define (get-top-level-focus-window)
  (ormap (lambda (f) (and (send f is-act-on?) (wx->mred f))) (wx:get-top-level-windows)))

(define (get-top-level-edit-target-window)
  (let loop ([l (wx:get-top-level-windows)][f #f][s 0][ms 0])
    (if (null? l)
	(and f (wx->mred f))
	(let* ([f2 (car l)]
	       [s2 (send f2 get-act-date/seconds)]
	       [ms2 (send f2 get-act-date/milliseconds)])
	  (if (or (not f)
		  (> s2 s)
		  (and (= s2 s) (> ms2 ms)))
	      (loop (cdr l) f2 s2 ms2)
	      (loop (cdr l) f s ms))))))

(define message%
  (class100*/kw basic-control% () [(label parent [style null]) control%-keywords]
    (override
      [label-checker  (lambda () check-label-string-or-bitmap)]) ; module-local method
    (sequence
      (let ([cwho '(constructor message)])
	(check-label-string/bitmap/iconsym cwho label)
	(check-container-parent cwho parent)
	(check-style cwho #f '(deleted) style))
      (as-entry
       (lambda ()
	 (super-init (lambda () (make-object wx-message% this this
					     (mred->wx-container parent)
					     label -1 -1 style))
		     (lambda () 
		       (let ([cwho '(constructor message)])
			 (check-container-ready cwho parent)))
		     label parent void #f))))))

(define button%
  (class100*/kw basic-control% () [(label parent callback [style null]) control%-keywords]
    (override
      [label-checker  (lambda () check-label-string-or-bitmap)]) ; module-local method
    (sequence
      (let ([cwho '(constructor button)])
	(check-label-string-or-bitmap cwho label)
	(check-container-parent cwho parent)
	(check-callback cwho callback)
	(check-style cwho #f '(border deleted) style))
      (as-entry
       (lambda ()
	 (super-init (lambda () (make-object wx-button% this this
					     (mred->wx-container parent) (wrap-callback callback)
					     label -1 -1 -1 -1 style))
		     (lambda () 
		       (let ([cwho '(constructor button)])
			 (check-container-ready cwho parent)))
		     label parent callback #f))))))

(define check-box%
  (class100*/kw basic-control% () [(label parent callback [style null] [value #f]) control%-keywords]
    (sequence
      (let ([cwho '(constructor check-box)])
	(check-label-string-or-bitmap cwho label)
	(check-container-parent cwho parent)
	(check-callback cwho callback)
	(check-style cwho #f '(deleted) style)))
    (override
      [label-checker  (lambda () check-label-string-or-bitmap)]) ; module-local method
    (private-field
      [wx #f])
    (public
      [get-value (entry-point (lambda () (send wx get-value)))]
      [set-value (entry-point (lambda (v) (send wx set-value v)))])
    (sequence
      (as-entry
       (lambda ()
	 (super-init (lambda () 
		       (set! wx (make-object wx-check-box% this this
					     (mred->wx-container parent) (wrap-callback callback)
					     label -1 -1 -1 -1 style))
		       wx)
		     (lambda ()
		       (let ([cwho '(constructor check-box)])
			 (check-container-ready cwho parent)))
		     label parent callback #f)))
      (when value (set-value #t)))))

(define radio-box%
  (class100*/kw basic-control% () 
		[(label choices parent callback [style '(vertical)] [selection 0]) control%-keywords]
    (private-field [chcs choices])
    (sequence 
      (let ([cwho '(constructor radio-box)])
	(check-label-string/false cwho label)
	(unless (and (list? chcs) (pair? chcs)
		     (or (andmap label-string? chcs)
			 (andmap (lambda (x) (is-a? x wx:bitmap%)) chcs)))
	  (raise-type-error (who->name cwho) "non-empty list of strings (up to 200 characters) or bitmap% objects" chcs))
	(check-container-parent cwho parent)
	(check-callback cwho callback)
	(check-orientation cwho style)
	(check-non-negative-integer cwho selection)))
    (private-field
      [wx #f])
    (private
      [check-button
       (lambda (method n)
	 (check-non-negative-integer `(method radio-box% ,method) n)
	 (unless (< n (length chcs))
	   (raise-mismatch-error (who->name `(method radio-box% ,method)) "no such button: " n)))])
    (override
      [enable (entry-point
	       (case-lambda
		[(on?) (send wx enable on?)]
		[(which on?) (check-button 'enable which)
			     (send wx enable which on?)]))]
      [is-enabled? (entry-point
		    (case-lambda
		     [() (send wx is-enabled?)]
		     [(which) (check-button 'is-enabled? which)
			      (send wx is-enabled? which)]))])
    (public
      [get-number (lambda () (length chcs))]
      [get-item-label (lambda (n) 
			(check-button 'get-item-label n)
			(list-ref chcs n))]
      [get-item-plain-label (lambda (n) 
			      (check-button 'get-item-plain-label n)
			      (wx:label->plain-label (list-ref chcs n)))]
       
      [get-selection (entry-point (lambda () (send wx get-selection)))]
      [set-selection (entry-point
		      (lambda (v) 
			(check-button 'set-selection v)
			(send wx set-selection v)))])
    (sequence
      (as-entry
       (lambda ()
	 (when (andmap string? chcs)
	   (set! chcs (map string->immutable-string chcs)))
	 (super-init (lambda () 
		       (set! wx (make-object wx-radio-box% this this
					     (mred->wx-container parent) (wrap-callback callback)
					     label -1 -1 -1 -1 chcs 0 style))
		       wx)
		     (lambda ()
		       (let ([cwho '(constructor radio-box)])
			 (check-container-ready cwho parent)
			 (unless (< selection (length choices))
			   (raise-mismatch-error (who->name cwho)
						 (format "initial selection is too large, given only ~a choices: "
							 (length choices))
						 selection))))
		     label parent callback #f)))
      (when (positive? selection)
	(set-selection selection)))))

(define slider%
  (class100*/kw basic-control% ()
		[(label min-value max-value parent callback [init-value min-value] [style '(horizontal)])
		 control%-keywords]
    (private-field [minv min-value][maxv max-value])
    (sequence 
      (let ([cwho '(constructor slider)])
	(check-label-string/false cwho label)
	(check-slider-integer cwho minv)
	(check-slider-integer cwho maxv)
	(check-container-parent cwho parent) 
	(check-callback cwho callback)
	(check-slider-integer cwho init-value)
	(check-style cwho '(vertical horizontal) '(plain vertical-label horizontal-label deleted) style)))
    (private-field
      [wx #f])
    (public
      [get-value (entry-point (lambda () (send wx get-value)))]
      [set-value (entry-point
		  (lambda (v)
		    (check-slider-integer '(method slider% set-value) v)
		    (unless (<= minv v maxv)
		      (raise-mismatch-error (who->name '(method slider% set-value))
					    (format "slider's range is ~a to ~a; cannot set the value to: "
						    minv maxv)
					    v))
		    (send wx set-value v)))])
    (sequence
      (as-entry
       (lambda ()
	 (super-init (lambda () 
		       (set! wx (make-object wx-slider% this this
					     (mred->wx-container parent) (wrap-callback callback)
					     label init-value minv maxv style))
		       wx)
		     (lambda () 
		       (let ([cwho '(constructor slider)])
			 (check-container-ready cwho parent)))
		     label parent callback #f))))))

(define gauge%
  (class100*/kw basic-control% ()
		[(label range parent [style '(horizontal)]) control%-keywords]
    (sequence 
      (let ([cwho '(constructor gauge)])
	(check-label-string/false cwho label)
	(check-container-parent cwho parent) 
	(check-gauge-integer cwho range)
	(check-orientation cwho style)))
    (private-field
      [wx #f])
    (public
      [get-value (entry-point (lambda () (send wx get-value)))]
      [set-value (entry-point
		  (lambda (v)
		    (check-range-integer '(method gauge% set-value) v)
		    (when (> v (send wx get-range))
		      (raise-mismatch-error (who->name '(method gauge% set-value))
					    (format "gauge's range is 0 to ~a; cannot set the value to: "
						    (send wx get-range))
					    v))
		    (send wx set-value v)))]
      [get-range (entry-point (lambda () (send wx get-range)))]
      [set-range (entry-point
		  (lambda (v)
		    (check-gauge-integer '(method gauge% set-range) v)
		    (send wx set-range v)))])
    (sequence
      (as-entry
       (lambda ()
	 (super-init (lambda () 
		       (set! wx (make-object wx-gauge% this this
					     (mred->wx-container parent)
					     label range style))
		       wx)
		     (lambda ()
		       (let ([cwho '(constructor gauge)])
			 (check-container-ready cwho parent)))
		     label parent void #f))))))

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

(define-local-member-name 
  -append-list-string
  -set-list-strings
  -set-list-string
  -delete-list-item)

(define basic-list-control%
  (class100* basic-control% (list-control<%>) (mk-wx mismatches label parent selection callback init-choices)
    (private-field
      [content (map string->immutable-string init-choices)])
    (public
      [(-append append) (entry-point (lambda (i) 
				       (check-label-string '(method list-control<%> append) i)
				       (-append-list-string i)
				       (send wx append i)))]
      [clear (entry-point (lambda () (send wx clear) (set! content null)))]
      [get-number (entry-point (lambda () (send wx number)))]
      [get-string (entry-point (lambda (n) (check-item 'get-string n) (list-ref content n)))]
      [get-selection (entry-point (lambda () (and (positive? (send wx number)) (-1=>false (send wx get-selection)))))]
      [get-string-selection (entry-point (lambda () (and (positive? (send wx number)) 
							 (let ([v (send wx get-selection)])
							   (if (= v -1)
							       #f
							       (list-ref content v))))))]
      [set-selection (entry-point (lambda (s) (check-item 'set-selection s) (send wx set-selection s)))]
      [set-string-selection (entry-point
			     (lambda (s)
			       (check-label-string '(method list-control<%> set-string-selection) s)
			       (let ([pos (do-find-string s)])
				 (if pos
				     (send wx set-selection pos)
				     (raise-mismatch-error (who->name '(method list-control<%> set-string-selection))
							   "no item matching the given string: " s)))))]
      [find-string (entry-point (lambda (x)
				  (check-label-string '(method list-control<%> find-string) x)
				  (do-find-string x)))]
      
      [-append-list-string (lambda (i)
			     (set! content (append content (list i))))]
      [-set-list-string (lambda (i s)
			  (set-car! (list-tail content i) (string->immutable-string s)))]
      [-delete-list-item (lambda (pos)
			   (if (zero? pos)
			       (set! content (cdr content))
			       (set-cdr! (list-tail content (sub1 pos)) 
					 (list-tail content (add1 pos)))))]
      [-set-list-strings (lambda (l)
			   (set! content (map string->immutable-string l)))])
    (private-field
      [wx #f])
    (private
      [do-find-string
       (lambda (s)
	 (let loop ([l content][pos 0])
	   (cond
	    [(null? l) #f]
	    [(string=? s (car l)) pos]
	    [else (loop (cdr l) (add1 pos))])))]
      [check-item
       (lambda (method n)
	 (check-non-negative-integer `(method list-control<%> ,method) n)
	 (let ([m (send wx number)])
	   (unless (< n m)
	     (raise-mismatch-error (who->name `(method list-control<%> ,method)) 
				   (if (zero? m)
				       "control has no items; given index: " 
				       (format "control has only ~a items, indexed 0 to ~a; given out-of-range index: " 
					       m (sub1 m)))
				   n))))])
    (sequence
      (as-entry
       (lambda ()
	 (super-init (lambda () (set! wx (mk-wx)) wx) mismatches label parent callback #f)))
      (when selection
	(set-selection selection)))))

(define (check-list-control-args cwho label choices parent callback)
  (check-label-string/false cwho label)
  (unless (and (list? choices) (andmap label-string? choices))
    (raise-type-error (who->name cwho) "list of strings (up to 200 characters)" choices))
  (check-container-parent cwho parent)
  (check-callback cwho callback))

(define (check-list-control-selection cwho choices selection)
  (unless (< selection (length choices))
    (raise-mismatch-error (who->name cwho)
			  (format "initial selection is too large, given only ~a choices: "
				  (length choices))
			  selection)))

(define choice%
  (class100*/kw basic-list-control% ()
		[(label choices parent callback [style null] [selection 0])
		 control%-keywords]
    (sequence
      (let ([cwho '(constructor choice)])
	(check-list-control-args cwho label choices parent callback)
	(check-style cwho #f '(vertical-label horizontal-label deleted) style)
	(check-non-negative-integer cwho selection))
      (super-init (lambda () (make-object wx-choice% this this
					  (mred->wx-container parent) (wrap-callback callback)
					  label -1 -1 -1 -1 choices style))
		  (lambda ()
		    (let ([cwho '(constructor choice)])
		      (check-container-ready cwho parent)
		      (unless (= 0 selection)
			(check-list-control-selection cwho choices selection))))
		  label parent 
		  (and (positive? selection) selection)
		  callback
		  choices))))

(define list-box%
  (class100*/kw basic-list-control%  ()
		[(label choices parent callback [style '(single)] [selection #f])
		 control%-keywords]
    (sequence 
      (let ([cwho '(constructor list-box)])
	(check-list-control-args cwho label choices parent callback)
	(check-style cwho '(single multiple extended) '(vertical-label horizontal-label deleted) style)
	(check-non-negative-integer/false cwho selection)))
    (rename [super-append append])
    (override
      [append (entry-point
	       (case-lambda
		[(i) 
		 (super-append i)]
		[(i d) 
		 (check-label-string '(method list-control<%> append) i)
		 (send this -append-list-string i)
		 (send wx append i d)]))])
    (public
      [delete (entry-point (lambda (n) 
			     (check-item 'delete n) 
			     (send this -delete-list-item n)
			     (send wx delete n)))]
      [get-data (entry-point (lambda (n) (check-item 'get-data n) (send wx get-data n)))]
      [get-selections (entry-point (lambda () (send wx get-selections)))]
      [number-of-visible-items (entry-point (lambda () (send wx number-of-visible-items)))]
      [is-selected? (entry-point (lambda (n) (check-item 'is-selected? n) (send wx selected? n)))]
      [set (entry-point (lambda (l) 
			  (unless (and (list? l) (andmap label-string? l))
			    (raise-type-error (who->name '(method list-box% set)) 
					      "list of strings (up to 200 characters)" l))
			  (send this -set-list-strings l)
			  (send wx set l)))]
      [set-string (entry-point
		   (lambda (n d)
		     (check-non-negative-integer '(method list-box% set-string) n) ; int error before string
		     (check-label-string '(method list-box% set-string) d) ; string error before range mismatch
		     (check-item 'set-string n)
		     (send this -set-list-string n d)
		     (send wx set-string n d)))]
      [set-data (entry-point (lambda (n d) (check-item 'set-data n) (send wx set-data n d)))]
      [get-first-visible-item (entry-point (lambda () (send wx get-first-item)))]
      [set-first-visible-item (entry-point (lambda (n) 
					       (check-item 'set-first-visible-item n) 
					       (send wx set-first-visible-item n)))]
      [select (entry-point 
	       (case-lambda 
		[(n) (check-item 'select n) (send wx select n #t)]
		[(n on?) (check-item 'select n) (send wx select n on?)]))])
    (private-field
      [wx #f])
    (private
      [check-item
       (entry-point
	(lambda (method n)
	  (check-non-negative-integer `(method list-box% ,method) n)
	  (let ([m (send wx number)])
	    (unless (< n m)
	      (raise-mismatch-error (who->name `(method list-box% ,method))
				    (if (zero? m)
					"list has no items; given index: " 
					(format "list has only ~a items, indexed 0 to ~a; given out-of-range index: "
						m (sub1 m)))
				    n)))))])
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
		  (lambda ()
		    (let ([cwho '(constructor list-box)])
		      (check-container-ready cwho parent)
		      (when selection
			(check-list-control-selection cwho choices selection))))
		  label parent (and (pair? choices) selection) callback
		  choices))))

(define text-field%
  (class100*/kw basic-control% () 
		[(label parent callback [init-value ""] [style '(single)])
		 control%-keywords]
    (sequence 
      (let ([cwho '(constructor text-field)])
	(check-label-string/false cwho label)
	(check-container-parent cwho parent)
	(check-callback cwho callback)
	(check-string cwho init-value)
	(check-style cwho '(single multiple) '(hscroll password vertical-label horizontal-label deleted) style)))
    (private-field
      [wx #f])
    (public
      [get-editor (entry-point (lambda () (send wx get-editor)))]
      [get-value (lambda () (send wx get-value))] ; note: wx method doesn't expect as-entry
      [set-value (entry-point 
		  (lambda (v) 
		    (check-string '(method text-control<%> set-value) v)
		    (send wx set-value v)))])
    (sequence
      ;; Technically a bad way to change margin defaults, since it's
      ;;  implemented with an update after creation:
      (when (eq? horiz-margin no-val) (set! horiz-margin 2))
      (when (eq? vert-margin no-val) (set! vert-margin 2))
      (as-entry
       (lambda ()
	 (super-init (lambda () 
		       (set! wx (make-object wx-text-field% this this
					     (mred->wx-container parent) (wrap-callback callback)
					     label init-value style))
		       wx)
		     (lambda ()
		       (let ([cwho '(constructor text-field)])
			 (check-container-ready cwho parent)))
		     label parent callback ibeam))))))

;; Not exported:
(define tab-group%
  (class100 basic-control% (label choices parent callback [style null])
    (override
     [hidden-child? (lambda () #t)])
    (sequence
      (let ([cwho '(constructor tab-group)])
	(check-list-control-args cwho label choices parent callback)
	(check-style cwho #f '(deleted border) style))
      (super-init (lambda () (make-object wx-tab-group% this this
					  style
					  (mred->wx-container parent)
					  (wrap-callback callback)
					  label
					  choices
					  style))
		  (lambda ()
		    (let ([cwho '(constructor tab-group)])
		      (check-container-ready cwho parent)))
		  label parent callback #f))))

;; Not exported:
(define group-box%
  (class100 basic-control% (label parent [style null])
    (override
     [hidden-child? (lambda () #t)])
    (sequence
      (let ([cwho '(constructor group-box)])
	(check-label-string cwho label)
	(check-container-parent cwho parent)
	(check-style cwho #f '(deleted) style))
      (super-init (lambda () (make-object wx-group-box% this this
					  style
					  (mred->wx-container parent)
					  label
					  style))
		  (lambda ()
		    (let ([cwho '(constructor group-box)])
		      (check-container-ready cwho parent)))
		  label parent void #f))))

;-------------------- Canvas class constructions --------------------

(define canvas-default-size 20) ; a default size for canvases tht fits borders without losing client sizes
(define canvas-scroll-size 10)
(define canvas-control-border-extra (case (system-type)
				      [(windows) 2]
				      [else 0]))

(define canvas<%>
  (interface (subwindow<%>)
    min-client-width min-client-height
    on-char on-event on-paint on-scroll on-tab-in
    warp-pointer get-dc))

(define-keywords canvas%-keywords
  window%-keywords
  subarea%-keywords
  area%-keywords)

(define basic-canvas%
  (class100* (make-window% #f (make-subarea% area%)) (canvas<%>) (mk-wx mismatches parent)
    (public
      [on-char (lambda (e) (send wx do-on-char e))]
      [on-event (lambda (e) (send wx do-on-event e))]
      [on-paint (lambda () (when wx (send wx do-on-paint)))]
      [on-scroll (lambda (e) (send wx do-on-scroll e))]
      [on-tab-in (lambda () (void))]
      
      [min-client-width (param (lambda () wx) min-client-width)]
      [min-client-height (param (lambda () wx) min-client-height)]

      [warp-pointer (entry-point (lambda (x y) (send wx warp-pointer x y)))]

      [get-dc (entry-point (lambda () (send wx get-dc)))])
    (private-field
      [wx #f])
    (sequence
      (as-entry
       (lambda ()
	 (super-init (lambda () (set! wx (mk-wx)) wx) (lambda () wx) mismatches #f parent #f))))))

(define default-paint-cb (lambda (canvas dc) (void)))

(define canvas%
  (class100*/kw basic-canvas% ()
		[(parent [style null] [paint-callback default-paint-cb] [label #f])
		 canvas%-keywords]
    (private-field [paint-cb paint-callback])
    (inherit get-client-size get-dc set-label)
    (rename [super-on-paint on-paint])
    (sequence 
      (let ([cwho '(constructor canvas)])
	(check-container-parent cwho parent)
	(check-style cwho #f '(border hscroll vscroll gl deleted control-border no-autoclear transparent) style)
	(check-callback cwho paint-callback)
	(check-label-string/false cwho label)))
    (public
      [swap-gl-buffers (lambda () (send (send (send wx get-dc) get-gl-context) swap-buffers))]
      [with-gl-context (lambda (thunk) 
			 (send (send (send wx get-dc) get-gl-context) call-as-current thunk))]
      [accept-tab-focus (entry-point
			 (case-lambda
			  [() (send wx get-tab-focus)]
			  [(on?) (send wx set-tab-focus (and on? #t))]))]
      [get-virtual-size (entry-point
			 (lambda () (double-boxed
				     0 0
				     (lambda (x y) (send wx get-virtual-size x y)))))]
      [get-view-start (entry-point
		       (lambda () (double-boxed
				   0 0
				   (lambda (x y) (send wx view-start x y)))))]

      [scroll (entry-point (lambda (x y) 
			       (when x (check-fraction '(method canvas% scroll) x))
			       (when y (check-fraction '(method canvas% scroll) y))
			       (send wx scroll (or x -1) (or y -1))))]

      [init-auto-scrollbars
       (lambda (w h x y)
	 (when w (check-gauge-integer '(method canvas% init-auto-scrollbars) w))
	 (when h (check-gauge-integer '(method canvas% init-auto-scrollbars) h))
	 (check-fraction '(method canvas% init-auto-scrollbars) x)
	 (check-fraction '(method canvas% init-auto-scrollbars) y)
	 (let-values ([(cw ch) (get-client-size)])
	   (send wx set-scrollbars (if w 1 0) (if h 1 0)
		 (or w 0) (or h 0) 1 1
		 (if w (inexact->exact (floor (* x (max 0 (- w cw))))) 0)
		 (if h (inexact->exact (floor (* y (max 0 (- h ch))))) 0)
		 #t)))]
	 
      [init-manual-scrollbars 
       (lambda (x-len y-len x-page y-page x-val y-val)
	 (let ([who '(method canvas% init-auto-scrollbars)])
	   (when x-len (check-range-integer who x-len))
	   (when y-len (check-range-integer who y-len))
	   (check-gauge-integer who x-page)
	   (check-gauge-integer who y-page)
	   (check-range-integer who x-val)
	   (check-range-integer who y-val)
	   (when (and x-len (< x-len x-val))
	     (raise-mismatch-error (who->name who)
				   (format "horizontal value: ~e larger than the horizontal range: "
					   x-val)
				   x-len))
	   (when (and y-len (< y-len y-val))
	     (raise-mismatch-error (who->name who)
				   (format "vertical value: ~e larger than the vertical range: "
					   y-val)
				   y-len)))
	 (send wx set-scrollbars (if x-len 1 0) (if y-len 1 0)
	       (or x-len 0) (or y-len 0) x-page y-page x-val y-val #f))]

      [get-scroll-pos (entry-point (lambda (d) (send wx get-scroll-pos d)))]
      [set-scroll-pos (entry-point (lambda (d v) (send wx set-scroll-pos d v)))]
      [get-scroll-range (entry-point (lambda (d) (send wx get-scroll-range d)))]
      [set-scroll-range (entry-point (lambda (d v) (send wx set-scroll-range d v)))]
      [get-scroll-page (entry-point (lambda (d) (send wx get-scroll-page d)))]
      [set-scroll-page (entry-point (lambda (d v) (send wx set-scroll-page d v)))])
    (override
      [on-paint (lambda () 
		  (if (eq? paint-cb default-paint-cb)
		      (super-on-paint)
		      (paint-cb this (get-dc))))])
    (private-field
      [wx #f])
    (sequence
      (super-init (lambda () 
		    (let ([ds (+ (cond
				  [(memq 'control-border style) (+ 4 canvas-control-border-extra)]
				  [(memq 'border style) 4]
				  [else 0])
				 (if (or (memq 'vscroll style) (memq 'hscroll style))
				     canvas-default-size
				     1))])
		      (set! wx (make-object wx-canvas% this this
					    (mred->wx-container parent)
					    -1 -1 ds ds
					    style)))
		    wx)
		  (lambda ()
		    (let ([cwho '(constructor canvas)])
		      (check-container-ready cwho parent)))
		  parent)
      (when label
	(set-label label))
      (send parent after-new-child this))))
    
(define editor-canvas%
  (class100*/kw basic-canvas% ()
		[(parent [editor #f] [style null] [scrolls-per-page 100] [label #f]
			 [wheel-step no-val] [line-count no-val]
			 [horizontal-inset 5] [vertical-inset 5])
		 canvas%-keywords]
    (sequence 
      (let ([cwho '(constructor editor-canvas)])
	(check-container-parent cwho parent)
	(check-instance cwho internal-editor<%> "text% or pasteboard%" #t editor)
	(check-style cwho #f '(hide-vscroll hide-hscroll no-vscroll no-hscroll deleted control-border) style)
	(check-gauge-integer cwho scrolls-per-page)
	(check-label-string/false cwho label)
	(unless (eq? wheel-step no-val)
	  (check-wheel-step cwho wheel-step))
	(unless (or (not line-count) (eq? line-count no-val))
	  ((check-bounded-integer 1 1000 #t) cwho line-count))
	(unless (eq? horizontal-inset 5)
	  (check-margin-integer cwho horizontal-inset))
	(unless (eq? vertical-inset 5)
	  (check-margin-integer cwho vertical-inset))))
    (inherit set-label)
    (private-field
      [force-focus? #f]
      [scroll-to-last? #f]
      [scroll-bottom? #f])
    (public
      [call-as-primary-owner (lambda (f) (send wx call-as-primary-owner f))]
      [allow-scroll-to-last
       (entry-point 
	(case-lambda
	 [() scroll-to-last?]
	 [(on?) (set! scroll-to-last? (and on? #t))
		(send wx allow-scroll-to-last on?)]))]
      [scroll-with-bottom-base
       (entry-point
	(case-lambda
	 [() scroll-bottom?]
	 [(on?) (set! scroll-bottom? (and on? #t))
		(send wx scroll-with-bottom-base on?)]))]
      [lazy-refresh
       (entry-point
	(case-lambda
	 [() (send wx get-lazy-refresh)]
	 [(on?) (send wx set-lazy-refresh on?)]))]
      [force-display-focus
       (entry-point
	(case-lambda
	 [() force-focus?]
	 [(on?) (set! force-focus? (and on? #t))
		(send wx force-display-focus on?)]))]

      [allow-tab-exit (entry-point
		       (case-lambda
			[() (send wx is-tabable?)]
			[(on?) (send wx set-tabable (and on? #t))]))]

      [set-line-count
       (entry-point
	(lambda (n)
	  ((check-bounded-integer 1 1000 #t) '(method editor-canvas% set-line-count) n)
	  (send wx set-line-count n)))]
      [get-line-count
       (entry-point
	(lambda ()
	  (send wx get-line-count)))]

      [scroll-to (case-lambda 
		  [(x y w h refresh?) (send wx scroll-to x y w h refresh?)]
		  [(x y w h refresh? bias) (send wx scroll-to x y w h refresh? bias)])]

      [get-editor (entry-point (lambda () (send wx get-editor)))]
      [set-editor (entry-point 
		   (case-lambda 
		    [(m) (send wx set-editor m)]
		    [(m upd?) (send wx set-editor m upd?)]))]
      [(ws wheel-step)
       (case-lambda 
	[() (let ([v (send wx get-wheel-step)])
	      (if (zero? v) #f v))]
	[(wheel-step)
	 (check-wheel-step '(method editor-canvas% wheel-step) wheel-step)
	 (send wx set-wheel-step (or wheel-step 0))])]
      [(vi vertical-inset)
       (entry-point
	(case-lambda
	 [() (send wx get-y-margin)]
	 [(m) 
	  (check-margin-integer '(method editor-canvas% vertical-inset) m)
	  (as-exit (lambda () (send wx set-y-margin m)))]))]
      [(hi horizontal-inset)
       (entry-point
	(case-lambda
	 [() (send wx get-x-margin)]
	 [(m) 
	  (check-margin-integer '(method editor-canvas% horizontal-inset) m)
	  (as-exit (lambda () (send wx set-x-margin m)))]))])
    (private-field
      [wx #f])
    (sequence
      (super-init (lambda () 
		    (let* ([no-h? (or (memq 'no-vscroll style)
				      (memq 'hide-vscroll style))]
			   [no-v? (or (memq 'no-hscroll style)
				      (memq 'hide-hscroll style))]
			   [get-ds (lambda (no-this? no-other?)
				     (+ (if (memq 'control-border style)
					    canvas-control-border-extra
					    0)
					(cond
					 [(and no-this? no-other?) 14]
					 [no-this? canvas-default-size]
					 [else (+ canvas-scroll-size canvas-default-size)])))])
		      (set! wx (make-object wx-editor-canvas% this this
					    (mred->wx-container parent) -1 -1
					    (get-ds no-h? no-v?)
					    (get-ds no-v? no-h?)
					    #f style scrolls-per-page #f))
		      wx))
		  (lambda () 
		    (let ([cwho '(constructor editor-canvas)])
		      (check-container-ready cwho parent)))
		  parent)
      (unless (eq? wheel-step no-val)
	(ws wheel-step))
      (when label
	(set-label label))
      (when editor
	(set-editor editor))
      (send parent after-new-child this)
      (unless (or (not line-count) (eq? line-count no-val))
	(set-line-count line-count))
      (unless (or (eq? vertical-inset 5))
	(vi vertical-inset))
      (unless (or (eq? horizontal-inset 5))
	(hi horizontal-inset)))))

;-------------------- Final panel interfaces and class constructions --------------------

(define-keywords pane%-keywords
  subarea%-keywords
  container%-keywords
  area%-keywords)

(define pane%
  (class100*/kw (make-subarea% (make-container% area%)) ()
		[(parent) pane%-keywords]
    (private-field [wx #f])
    (sequence 
      (let* ([who (cond ; yuck! - we do this to make h-p and v-p subclasses of p
		   [(is-a? this vertical-pane%) 'vertical-pane]
		   [(is-a? this horizontal-pane%) 'horizontal-pane]
		   [(is-a? this grow-box-spacer-pane%) 'grow-box-spacer-pane]
		   [else 'pane])]
	     [cwho `(constructor ,who)])
	(check-container-parent cwho parent)
	(as-entry
	 (lambda ()
	   (super-init (lambda () (set! wx (make-object (case who
							  [(vertical-pane) wx-vertical-pane%]
							  [(horizontal-pane) wx-horizontal-pane%]
							  [(grow-box-spacer-pane) wx-grow-box-pane%]
							  [else wx-pane%])
							this this (mred->wx-container parent) null)) wx)
		       (lambda () wx) 
		       (lambda ()
			 (check-container-ready cwho parent))
		       parent)
	   (send (send wx area-parent) add-child wx)))
	(send parent after-new-child this)))))

(define vertical-pane% (class100*/kw pane% () [(parent) pane%-keywords] (sequence (super-init parent))))
(define horizontal-pane% (class100*/kw pane% () [(parent) pane%-keywords] (sequence (super-init parent))))
(define grow-box-spacer-pane% (class100*/kw pane% () [(parent) pane%-keywords] (sequence (super-init parent))))

(define-keywords panel%-keywords
  window%-keywords
  subarea%-keywords
  container%-keywords
  area%-keywords)

(define panel%
  (class100*/kw (make-area-container-window% (make-window% #f (make-subarea% (make-container% area%)))) (subwindow<%>) 
		[(parent [style null]) panel%-keywords]
    (private-field [wx #f])
    (sequence 
      (let* ([who (cond ; yuck! - we do this to make h-p and v-p subclasses of p
		   [(is-a? this tab-panel%) 'tab-panel]
		   [(is-a? this group-box-panel%) 'group-box-panel]
		   [(is-a? this vertical-panel%) 'vertical-panel]
		   [(is-a? this horizontal-panel%) 'horizontal-panel]
		   [else 'panel])]
	     [cwho `(constructor ,who)])
	(check-container-parent cwho parent)
	(check-style cwho #f '(border deleted) style)
	(as-entry
	 (lambda ()
	   (super-init (lambda () (set! wx (make-object (case who
							  [(vertical-panel tab-panel group-box-panel) wx-vertical-panel%]
							  [(horizontal-panel) wx-horizontal-panel%]
							  [else wx-panel%])
							this this (mred->wx-container parent) style)) wx)
		       (lambda () wx) 
		       (lambda () (check-container-ready cwho parent))
		       #f parent #f)
	   (unless (memq 'deleted style)
	     (send (send wx area-parent) add-child wx))))
	(send parent after-new-child this)))))

(define vertical-panel% (class100*/kw panel% () [(parent [style null]) panel%-keywords] (sequence (super-init parent style))))
(define horizontal-panel% (class100*/kw panel% () [(parent [style null]) panel%-keywords] (sequence (super-init parent style))))

(define list-append append)

(define tab-panel%
  (class100*/kw vertical-panel% ()
		[(choices parent callback [style null]) panel%-keywords]
    (sequence
      (let ([cwho '(constructor tab-panel)])
	(unless (and (list? choices) (andmap label-string? choices))
	  (raise-type-error (who->name cwho) "list of strings (up to 200 characters)" choices))
	(check-callback cwho callback)
	(check-container-parent cwho parent)
	(check-style cwho #f '(deleted no-border) style))
      (super-init parent (if (memq 'deleted style)
			     '(deleted)
			     null)))

    (private-field
     [tabs (make-object tab-group% #f choices this (lambda (c e) (callback this e)) 
			(if (memq 'no-border style)
			     null
			     '(border)))])
    (sequence
      (send (mred->wx this) set-first-child-is-hidden))

    (private-field
     [save-choices (map string->immutable-string choices)]
     [hidden-tabs? #f])

    (public
     [get-number (lambda () (length save-choices))]
     [append (entry-point
	      (lambda (n) 
		(check-label-string '(method tab-panel% append) n)
		(let ([n (string->immutable-string n)])
		  (set! save-choices (list-append save-choices (list n)))
		  (as-exit (lambda () (send (mred->wx tabs) append n))))))]
     [get-selection (lambda () (and (pair? save-choices)
				    (send (mred->wx tabs) get-selection)))]
     [set-selection (entry-point
		     (lambda (i)
		       (check-item 'set-selection i)
		       (as-exit (lambda () (send (mred->wx tabs) set-selection i)))))]
     [delete (entry-point
	      (lambda (i)
		(check-item 'delete i)
		(set! save-choices (let loop ([p 0][l save-choices])
				     (if (= p i)
					 (cdr l)
					 (cons (car l) (loop (add1 p) (cdr l))))))
		(as-exit (lambda () (send (mred->wx tabs) delete i)))))])
    
    (private
      [check-item
       (lambda (method n)
	 (check-non-negative-integer `(method tab-panel% ,method) n)
	 (let ([m (length save-choices)])
	   (unless (< n m)
	     (raise-mismatch-error (who->name `(method tab-panel% ,method)) 
				   (if (zero? m)
				       "panel has no tabs; given index: " 
				       (format "panel has only ~a tabls, indexed 0 to ~a; given out-of-range index: " 
					       m (sub1 m)))
				   n))))])))


(define group-box-panel%
  (class100*/kw vertical-panel% ()
		[(label parent [style null]) panel%-keywords]
    (sequence
      (let ([cwho '(constructor group-box-panel)])
	(check-label-string cwho label)
	(check-container-parent cwho parent)
	(check-style cwho #f '(deleted) style))

      ;; Technically a bad way to change margin defaults, since it's
      ;;  implemented with an update after creation:
      (when (eq? horiz-margin no-val) (set! horiz-margin 2))
      (when (eq? vert-margin no-val) (set! vert-margin 2))

      (super-init parent (if (memq 'deleted style)
			     '(deleted)
			     null)))

    (private-field
     [gbox (make-object group-box% label this null)]
     [lbl label])
    (sequence
      (send (mred->wx this) set-first-child-is-hidden))

    (override
     [set-label (entry-point
		 (lambda (s)
		   (check-label-string '(method group-box-panel% set-label) s)
		   (set! lbl (if (immutable? s)
				 s
				 (string->immutable-string s)))
		   (send gbox set-label s)))]
     [get-label (lambda () lbl)])))

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

(define wx-menu-item%
  (class100* wx:menu-item% (wx<%>) (mr mn-dat)
    (private-field 
      [menu-data mn-dat]
      [mred mr]
      [keymap #f]
      [wx-menu #f]
      [enabled? #t])
    (public
      [get-keymap (lambda () keymap)]
      [set-keymap (lambda (k) (set! keymap k))]
      [swap-keymap (lambda (parent k) 
		     (send (send (mred->wx parent) get-container) swap-item-keymap keymap k) 
		     (set-keymap k))]
      [get-mred (lambda () mred)]
      [get-menu-data (lambda () menu-data)]  ; for meta-shortcuts
      [get-container (lambda () wx-menu)]
      [set-wx-menu (lambda (wx) (set! wx-menu wx))]
      [is-enabled? (lambda () enabled?)]
      [set-enabled (lambda (on?) (set! enabled? on?))])
    (sequence
      (super-init))))

(define wx-menu-bar%
  (class100* wx:menu-bar% (wx<%>) (mr)
    (inherit delete)
    (rename [super-append append]
	    [super-enable-top enable-top])
    (private-field
      [mred mr]
      [items null]
      [disabled null]
      [disabled? #f]
      [keymap (make-object wx:keymap%)])
    (public
      [get-container (lambda () this)]
      [handle-key (lambda (event) 
		    (as-exit 
		     (lambda () 
		       (or (send keymap handle-key-event this event)
			   (and (wx:shortcut-visible-in-label? #t)
				(send event get-meta-down)
				(char? (send event get-key-code))
				(let ([c (send event get-key-code)])
				  (and (or (char-alphabetic? c)
					   (char-numeric? c))
				       (let ([re (key-regexp c)])
					 (ormap
					  (lambda (i)
					    (let* ([data (send (mred->wx i) get-menu-data)]
						   [label (car data)]
						   [menu (cdr data)])
					      (if (regexp-match re label)
						  (begin
						    (send menu select)
						    #t)
						  #f)))
					  items)))))))))]
      [on-demand (lambda () (as-exit (lambda () (send mred on-demand))))]
      [get-mred (lambda () mred)]
      [get-items (lambda () items)]
      [append-item (lambda (item menu title)
		     (super-append menu title)
		     (when disabled?
		       (super-enable-top (length items) #f))
		     (set! items (append items (list item)))
		     (send keymap chain-to-keymap (send (mred->wx item) get-keymap) #f))]
      [all-enabled? (lambda () (not disabled?))]
      [enable-all (lambda (on?)
		    (set! disabled? (not on?))
		    (let loop ([n (sub1 (length items))])
		      (unless (negative? n)
			(if on?
			    (unless (memq (list-ref items n) disabled)
			      (super-enable-top n #t))
			    (super-enable-top n #f))
			(loop (sub1 n)))))]
      [delete-item (lambda (i)
		     (let ([p (position-of i)])
		       (set! items (remq i items))
		       (set! disabled (remq i disabled))
		       (delete #f p)
		       (send keymap remove-chained-keymap (send (mred->wx i) get-keymap))))]
      [position-of (lambda (i) (find-pos items i eq?))])
    (override
      [enable-top (lambda (p on?)
		    (let ([i (list-ref items p)])
		      (if on?
			  (when (memq i disabled)
			    (set! disabled (remq i disabled))
			    (unless disabled?
			      (super-enable-top p #t)))
			  (unless (memq i disabled)
			    (set! disabled (cons i disabled))
			    (super-enable-top p #f)))))])
    (sequence
      (super-init))))

(define wx-menu%
  (class100* wx:menu% (wx<%>) (mr popup-label popup-callback)
    (private-field
      [mred mr]
      [items null]
      [keymap (make-object wx:keymap%)]
      [popup-grabber #f])
    (inherit delete-by-position)
    (rename [super-delete delete]
	    [super-enable enable])
    (public
      [get-container (lambda () this)]
      [get-keymap (lambda () keymap)]
      [get-mred (lambda () mred)]
      [get-items (lambda () items)]
      [append-item (lambda (i iwx) 
		     (set! items (append items (list i)))
		     (unless (or (is-a? i separator-menu-item%)
				 (not (send iwx is-enabled?)))
		       (let ([k (send iwx get-keymap)])
			 (when k
			   (send keymap chain-to-keymap k #f)))))]
      [delete-sep (lambda (i iwx)
		    (delete-by-position (find-pos items i eq?))
		    (set! items (remq i items)))]
      [swap-item-keymap (lambda (old-k new-k)
			  (when old-k (send keymap remove-chained-keymap old-k))
			  (when new-k (send keymap chain-to-keymap new-k #f)))]

      [popup-grab (lambda (c)
		    (if popup-grabber
			#f
			(begin
			  (set! popup-grabber c)
			  #t)))]
      [popup-release (lambda () (set! popup-grabber #f))]
      [get-popup-grabber (lambda () popup-grabber)])
    (override
      [delete (lambda (id i) 
		(super-delete id) 
		(set! items (remq i items))
		(let ([k (send (mred->wx i) get-keymap)])
		  (when k
		    (send keymap remove-chained-keymap k))))]
      [enable (lambda (iwx id on?)
		; Only called if the item is not deleted
		(unless (eq? (send iwx is-enabled?) (and on? #t))
		  (send iwx set-enabled (and on? #t))
		  (super-enable id on?)))])
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
    enable is-enabled?
    on-demand))

(define submenu-item<%>
  (interface (labelled-menu-item<%>) get-menu))

(define separator-menu-item%
  (class100* mred% (menu-item<%>) (parent)
    (sequence (menu-parent-only 'separator-menu-item parent))
    (private-field
      [prnt parent]
      [wx #f]
      [shown? #f]
      [wx-parent #f])
    (public
      [get-parent (lambda () prnt)]
      [restore (entry-point
		(lambda ()
		  (unless shown?
		    (send wx-parent append-separator)
		    (send wx-parent append-item this wx)
		    (set! shown? #t))))]
      [delete (entry-point
	       (lambda ()
		 (when shown?
		   (send wx-parent delete-sep this wx)
		   (set! shown? #f))))]
      [is-deleted? (lambda () (not shown?))])
    (sequence
      (as-entry
       (lambda ()
	 (set! wx (make-object wx-menu-item% this #f))
	 (set! wx-parent (send (mred->wx prnt) get-container))
	 (super-init wx)))
      (restore))))

(define (strip-tab s) (car (regexp-match #rx"^[^\t]*" s)))

(define basic-labelled-menu-item%
  (class100* mred% (labelled-menu-item<%>) (prnt lbl help-str wx-sub chkble? keymap set-wx demand-callback)
    (private-field
      [parent prnt]
      [label lbl]
      [help-string help-str]
      [wx-submenu wx-sub]
      [checkable? chkble?]
      [callback demand-callback]
      [wx #f]
      [wx-parent #f]
      [plain-label (string->immutable-string (wx:label->plain-label label))]
      [in-menu? (is-a? parent internal-menu<%>)]
      [shown? #f]
      [enabled? #t])
    (private
      [do-enable (lambda (on?)
		   (when shown?
		     (if in-menu?
			 (send wx-parent enable wx (send wx id) on?)
			 (send wx-parent enable-top (send wx-parent position-of this) on?)))
		   (set! enabled? (and on? #t)))])
    (public
      [on-demand (lambda () (callback this))]
      [get-parent (lambda () parent)]
      [get-label (lambda () label)]
      [set-label (entry-point
		  (lambda (l)
		    (check-label-string '(method labelled-menu-item<%> set-label) l)
		    (set! label (string->immutable-string l))
		    (set-car! (send wx get-menu-data) l)  ; for meta-shortcuts
		    (set! plain-label (string->immutable-string (wx:label->plain-label l)))
		    (when shown?
		      (if in-menu?
			  (send wx-parent set-label (send wx id) l)
			  (send wx-parent set-label-top (send wx-parent position-of this) label)))))]
      [get-plain-label (lambda () plain-label)]
      [get-help-string (lambda () help-string)]
      [set-help-string (entry-point
			(lambda (s) 
			  (check-label-string/false '(method labelled-menu-item<%> set-help-string) s)
			  (set! help-string (and s (string->immutable-string s)))
			  (when in-menu?
			    (send wx-parent set-help-string (send wx id) help-string))))]
      [enable (lambda (on?) (do-enable on?))]
      [is-enabled? (lambda () enabled?)]
      [restore (entry-point
		(lambda ()
		  (unless shown?
		    (if in-menu?
			(begin
			  (if wx-submenu
			      (send wx-parent append (send wx id) label wx-submenu help-string)
			      (send wx-parent append (send wx id) label help-string checkable?))
			  (send wx-parent append-item this wx))
			(send wx-parent append-item this wx-submenu label))
		    (set! shown? #t)
		    (do-enable enabled?))))]
      [delete (entry-point
	       (lambda ()
		 (when shown?
		   (if in-menu?
		       (send wx-parent delete (send wx id) this)
		       (send wx-parent delete-item this))
		   (set! shown? #f))))]
      [is-deleted? (lambda () (not shown?))])
    (sequence
      (as-entry
       (lambda ()
	 (when help-string
	   (set! help-string (string->immutable-string help-string)))
	 (set! wx (set-wx (make-object wx-menu-item% this (cons label #f))))
	 (set! wx-parent (send (mred->wx parent) get-container))
	 (super-init wx)
	 (when keymap (send wx set-keymap keymap))))
      (restore))))

(define selectable-menu-item<%>
  (interface (labelled-menu-item<%>)
    command
    get-shortcut set-shortcut
    get-x-shortcut-prefix set-x-shortcut-prefix))

(define (char-name c print?)
  (case c
    [(#\return) (if (eq? (system-type) 'macos) "Return" "Enter")]
    [(#\tab) "Tab"]
    [(#\space) "Space"]
    [(#\backspace) "Backspace"]
    [(#\rubout) "Delete"]
    [(#\:) (if print? ":" "Colon")]
    [(#\;) (if print? ";" "Semicolon")]
    [else c]))

(define basic-selectable-menu-item%
  (class100* basic-labelled-menu-item% (selectable-menu-item<%>) (lbl checkable? mnu cb shrtcut help-string set-wx demand-callback)
    (inherit is-enabled?)
    (rename [super-restore restore] [super-set-label set-label]
	    [super-is-deleted? is-deleted?]
	    [super-is-enabled? is-enabled?]
	    [super-get-label get-label])
    (private-field
      [menu mnu]
      [callback cb]
      [label lbl]
      [shortcut shrtcut]
      [wx #f])
    (public
      [command (lambda (e)
		 (check-instance '(method selectable-menu-item<%> command) wx:control-event% 'control-event% #f e)
		 (void (callback this e)))])
    (private-field
      [x-prefix 'meta])
    (private
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
							      (char-name
							       (char-upcase shortcut)
							       #t))]
					      [(windows) (format "~aCtl+~a" #\tab 
								 (char-name (char-upcase shortcut) #t))]
					      [(macos macosx) (format "~aCmd+~a" #\tab 
								      (char-name (char-upcase shortcut) #t))]))
					   (strip-tab label))]
			    [key-binding (and shortcut
					      (case (system-type)
						[(unix) (format "~a~a" 
								(case x-prefix
								  [(meta) ":m:"]
								  [(alt) ":a:"]
								  [(ctl-m) ":c:m;:"]
								  [(ctl) ":c:"])
								(char-name (char-downcase shortcut) #f))]
						[(windows) (format ":c:~a" (char-name (char-downcase shortcut) #f))]
						[(macos macosx) (format ":d:~a" (char-name (char-downcase shortcut) #f))]))]
			    [keymap (and key-binding
					 (let ([keymap (make-object wx:keymap%)])
					   (send keymap add-function "menu-item" 
						 ;; keymap function callback already in exit mode:
						 (lambda (edit event)
						   (when (is-enabled?)
						     (callback this (make-object wx:control-event% 'menu)))))
					   (send keymap map-function key-binding "menu-item")
					   keymap))])
		       (values new-label keymap)))])
    (private
      [do-set-label (entry-point
		     (lambda (l) 
		       (check-label-string '(method labelled-menu-item<%> set-label) l)
		       (let-values ([(new-label keymap) (calc-labels l)])
			 (set! label (string->immutable-string l))
			 (super-set-label new-label)
			 (if (or (super-is-deleted?)
				 (not (super-is-enabled?)))
			     (send wx set-keymap keymap)
			     (send wx swap-keymap menu keymap)))))])
    (override
      [get-label (lambda () label)]
      [set-label (lambda (s) (do-set-label s))])
    (public
      [set-shortcut (lambda (c) 
		      (check-char/false '(method selectable-menu-item<%> set-shortcut) c)
		      (set! shortcut c) (do-set-label (super-get-label)))]
      [get-shortcut (lambda () shortcut)]
      [get-x-shortcut-prefix (lambda () x-prefix)]
      [set-x-shortcut-prefix (lambda (p) 
			       (unless (memq p '(meta alt ctl-m ctl))
				 (raise-type-error (who->name '(method selectable-menu-item<%> set-x-shortcut-prefix))
						   "symbol: meta, alt, ctl-m, or ctl" p))
			       (set! x-prefix p) (do-set-label (super-get-label)))])
    (sequence
      (set! label (string->immutable-string label))
      (let-values ([(new-label keymap) (calc-labels label)])
	(super-init menu new-label help-string #f checkable? keymap (lambda (x) (set! wx x) (set-wx x)) demand-callback)))))

(define (check-shortcut-args who label menu callback shortcut help-string demand-callback)
  (let ([cwho `(constructor ,who)])
    (check-label-string cwho label)
    (menu-parent-only who menu)
    (check-callback cwho callback)
    (check-char/false cwho shortcut)
    (check-label-string/false cwho help-string)
    (check-callback1 cwho demand-callback)))

(define menu-item%
  (class100 basic-selectable-menu-item% (label parent callback [shortcut #f] [help-string #f] [demand-callback void])
    (sequence 
      (check-shortcut-args 'menu-item label parent callback shortcut help-string demand-callback)
      (super-init label #f parent callback shortcut help-string (lambda (x) x) demand-callback))))

(define checkable-menu-item%
  (class100 basic-selectable-menu-item% (label parent callback [shortcut #f] [help-string #f] [demand-callback void] [checked #f])
    (sequence
      (check-shortcut-args 'checkable-menu-item label parent callback shortcut help-string demand-callback))
    (private-field
      [mnu parent]
      [wx #f])
    (public
      [check (entry-point (lambda (on?) (send (send (mred->wx mnu) get-container) check (send wx id) on?)))]
      [is-checked? (entry-point (lambda () (send (send (mred->wx mnu) get-container) checked? (send wx id))))])
    (sequence
      (super-init label #t mnu callback shortcut help-string (lambda (x) (set! wx x) x) demand-callback)
      (when checked (check #t)))))

(define menu-item-container<%> (interface () get-items on-demand))
(define internal-menu<%> (interface ()))

(define menu%
  (class100* basic-labelled-menu-item% (menu-item-container<%> internal-menu<%>) (label parent [help-string #f] [demand-callback void])
    (private-field 
      [callback demand-callback])
    (sequence 
      (check-label-string '(constructor menu) label)
      (menu-or-bar-parent 'menu parent)
      (check-label-string/false '(constructor menu) help-string)
      (check-callback1 '(constructor menu) demand-callback))
    (public
      [get-items (entry-point (lambda () (send wx-menu get-items)))])
    (override
      [on-demand (lambda ()
		   (callback this)
		   (for-each
		    (lambda (i) 
		      (when (is-a? i labelled-menu-item<%>)
			(send i on-demand)))
		    (send wx-menu get-items)))])
    (private-field
      [wx-menu #f])
    (sequence
      (as-entry
       (lambda () 
	 (set! wx-menu (make-object wx-menu% this #f void))
	 (super-init parent label help-string wx-menu #f (send wx-menu get-keymap) (lambda (x) x) void)
	 (let ([wx-item (mred->wx this)])
	   (set-cdr! (send wx-item get-menu-data) wx-menu) ; for meta-shortcuts
	   (send wx-item set-wx-menu wx-menu)))))))

(define popup-menu%
  (class100* mred% (menu-item-container<%> internal-menu<%>) ([title #f][popdown-callback void][demand-callback void])
    (private-field 
      [callback demand-callback])
    (public
      [get-popup-target
       (lambda ()
	 (send wx get-popup-grabber))]
      [get-items (entry-point (lambda () (send wx get-items)))]
      [on-demand (lambda ()
		   (callback this)
		   (for-each
		    (lambda (i) 
		      (when (is-a? i labelled-menu-item<%>)
			(send i on-demand)))
		    (send wx get-items)))])
    (private-field
      [wx #f])
    (sequence
      (check-label-string/false '(constructor popup-menu) title)
      (check-callback '(constructor popup-menu) popdown-callback)
      (check-callback1 '(constructor popup-menu) demand-callback)
      (as-entry 
       (lambda ()
	 (set! wx (make-object wx-menu% this title
			       (lambda (mwx e)
				 (let ([go
					(lambda ()
					  (let ([wx (wx:id-to-menu-item (send e get-menu-id))])
					    (when wx
						  (send (wx->mred wx) command (make-object wx:control-event% 'menu)))
					    (dynamic-wind
					     void
					     (lambda ()
					       (popdown-callback this (make-object wx:control-event% 
										   (if wx 
										       'menu-popdown
										 'menu-popdown-none))))
					     (lambda () (send mwx popup-release)))))])
				   (if (eq? 'windows (system-type))
				       (wx:queue-callback go wx:middle-queue-key)
				       (go))))))
	 (super-init wx))))))

(define menu-bar%
  (class100* mred% (menu-item-container<%>) (parent [demand-callback void])
    (sequence 
      (unless (or (is-a? parent frame%) (eq? parent 'root))
	(raise-type-error (constructor-name 'menu-bar) "frame% object or 'root" parent))
      (check-callback1 '(constructor menu-bar) demand-callback)
      (if (eq? parent 'root)
	  (unless (current-eventspace-has-menu-root?)
	    (raise-mismatch-error (constructor-name 'menu-bar) "no menu bar allowed in the current eventspace for: " parent))
	  (when (as-entry (lambda () (send (mred->wx parent) get-the-menu-bar)))
	    (raise-mismatch-error (constructor-name 'menu-bar) "the specified frame already has a menu bar: " parent))))
    (private-field 
      [callback demand-callback]
      [prnt (if (eq? parent 'root)
		(let ([f (make-object (class frame%
					(define/override (on-exit)
					  (exit))
					(super-make-object "Root")))])
		  (as-entry
		   (lambda ()
		     (when root-menu-frame
		       (raise-mismatch-error (constructor-name 'menu-bar) "already has a menu bar: " parent))
		     (send (mred->wx f) designate-root-frame)
		     (set! root-menu-frame f)))
		  f)
		parent)]
      [wx #f]
      [wx-parent #f]
      [shown? #f])
    (public
      [get-frame (lambda () prnt)]
      [get-items (entry-point (lambda () (send wx get-items)))]
      [enable (entry-point (lambda (on?) (send wx enable-all on?)))]
      [is-enabled? (entry-point (lambda () (send wx all-enabled?)))]
      [on-demand (lambda ()
                   (callback this)
		   (for-each
		    (lambda (i) (send i on-demand))
		    (send wx get-items)))])
    (sequence
      (as-entry
       (lambda ()
	 (set! wx (make-object wx-menu-bar% this))
	 (set! wx-parent (mred->wx prnt))
	 (super-init wx)
	 (send wx-parent set-menu-bar wx)
	 (send wx-parent self-redraw-request))))))

(wx:set-menu-tester (lambda (m) (is-a? m popup-menu%)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; END SECURE LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Everything past this point is written at the user's level, so there
;; are no entry/edit operations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Standard Key Bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define std-keymap (make-object wx:keymap%))

(let* ([k std-keymap]
       [mouse-paste (lambda (edit event) 
		      (when (send event button-down?)
			(cond
			 [(is-a? edit wx:text%)
			  (let ([x-box (box (send event get-x))]
				[y-box (box (send event get-y))]
				[eol-box (box #f)])
			    (send edit global-to-local x-box y-box)
			    (let ([click-pos (send edit find-position 
						   (unbox x-box)
						   (unbox y-box)
						   eol-box)])
			      (send edit set-position click-pos)))]
			 [else (void)])
			(send edit paste)))]
       [mouse-popup-menu (lambda (edit event)
			   (when (send event button-up?)
			     (let ([a (send edit get-admin)])
			       (when a
				 (let ([m (make-object popup-menu%)])
				   (append-editor-operation-menu-items m)
				   ;; Remove shortcut indicators (because they might not be correct)
				   (for-each
				    (lambda (i)
				      (when (is-a? i selectable-menu-item<%>)
					(send i set-shortcut #f)))
				    (send m get-items))
				   (let-values ([(x y) (send edit
							     dc-location-to-editor-location
							     (send event get-x)
							     (send event get-y))])
				     (send a popup-menu m (+ x 5) (+ y 5))))))))])
  (wx:add-text-keymap-functions k)
  (send k add-function "mouse-paste" mouse-paste)
  (send k add-function "mouse-popup-menu" mouse-popup-menu)
  (map
   (lambda (key func) (send k map-function key func))
   (append
    (case (system-type)
      [(windows) '(":c:c" ":c:x" ":c:v" ":c:k" ":c:z" ":c:a")]
      [(macos macosx) '(":d:c" ":d:x" ":d:v" ":d:k" ":d:z" ":d:a")]
      [(unix) '(":m:w" ":c:w" ":c:y" ":c:k" ":c:s:_" ":m:a")])
    '(":middlebutton"))
   '("copy-clipboard" "cut-clipboard" "paste-clipboard" "delete-to-end-of-line" 
		      "undo" "select-all" "mouse-paste"))
  (send k map-function ":rightbuttonseq" "mouse-popup-menu")
  (when (eq? (system-type) 'unix)
    (send k map-function ":c:a" "beginning-of-line")
    (send k map-function ":c:e" "end-of-line")))

(define (check-installer who)
  (lambda (p)
    (unless (and (procedure? p)
		 (procedure-arity-includes? p 1))
      (raise-type-error who
			"procedure of arity 1"
			p))
    p))

(define current-text-keymap-initializer
  (make-parameter (let ([default-text-keymap-initializer
			  (lambda (k)
			    (check-instance 'default-text-keymap-initializer wx:keymap% 'keymap% #f k)
			    ;; Level of indirection to protect std-keymap:
			    (let ([naya (make-object wx:keymap%)])
			      (send naya chain-to-keymap std-keymap #f)
			      (send k chain-to-keymap naya #f)))])
		    default-text-keymap-initializer)
		  (check-installer 'default-text-keymap-initializer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (-graphical-read-eval-print-loop user-esp override-ports?)
  ;; The REPL buffer class
  (define esq:text%
    (class100 text% ()
      (inherit insert last-position get-text erase change-style clear-undos)
      (rename [super-on-char on-char])
      (private-field [prompt-pos 0] [locked? #f])
      (override
	[can-insert? (lambda (start end) (and (>= start prompt-pos) (not locked?)))]
	[can-delete? (lambda (start end) (and (>= start prompt-pos) (not locked?)))]
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
	(let ([s (last-position)]
	      [m (regexp-match #rx"^(.*), (Copyright.*)$" (banner))])
	  (insert (format "Welcome to ~a." (cadr m)))
	  (let ([e (last-position)])
	    (insert #\newline)
	    (change-style (send (make-object wx:style-delta% 'change-bold) set-delta-foreground "BLUE") s e))
	  (output (caddr m)))
	(insert "This is a simple window for evaluating MrEd Scheme expressions.") (insert #\newline)
	(let ([s (last-position)])
	  (insert "Quit now and run DrScheme to get a better window.")
	  (let ([e (last-position)])
	    (insert #\newline)
	    (change-style
	     (send (make-object wx:style-delta% 'change-italic) set-delta-foreground "RED")
	     s e)))
	(insert "The current input port always returns eof.") (insert #\newline)
	(new-prompt))))

  ;; GUI creation
  (define frame (make-object (class100 frame% args
			       (inherit accept-drop-files)
			       (override
				 [on-close (lambda () 
					     (custodian-shutdown-all user-custodian)
					     (semaphore-post waiting))]
				 [on-drop-file (lambda (f) (evaluate (format "(load ~s)" f)))])
			       (sequence 
				 (apply super-init args) (accept-drop-files #t)))
			     "MrEd REPL" #f 500 400))
  (define repl-buffer (make-object esq:text%))
  (define repl-display-canvas (make-object editor-canvas% frame))

  (define esq-eventspace (wx:current-eventspace))
  (define (queue-output proc)
    (parameterize ((wx:current-eventspace esq-eventspace))
      (wx:queue-callback proc #f)))
  
  ;; User space initialization
  (define user-custodian (make-custodian))
  
  (define user-output-port
    (let ([leftover #""]
	  [cvt (bytes-open-converter "UTF-8-permissive" "UTF-8")])
      (make-output-port
       'console
       always-evt
       (lambda (s start end flush? breakable?) 
	 (queue-output (lambda () 
			 ;; s might end in the middle of a UTF-8 encoding.
			 ;;  Get a complete prefix, and save the rest.
			 (let ([s (bytes-append leftover (subbytes s start end))])
			   (let-values ([(res used status) (bytes-convert cvt s)])
			     (send repl-buffer output (bytes->string/utf-8 res))
			     (set! leftover (subbytes s used))))))
	 (- end start))
       void))) ; no close action
   
  (define user-eventspace
    (or user-esp
	(parameterize ((current-custodian user-custodian))
	  (wx:make-eventspace))))

  ;; Evaluation
  
  (define (evaluate expr-str)
    (parameterize ((wx:current-eventspace user-eventspace))
      (wx:queue-callback
       (lambda ()
	 (dynamic-wind
	  void
	  (lambda () 
	    (call-with-values
	     (lambda () (eval (read (open-input-string expr-str))))
	     (lambda results
	       (for-each 
		(lambda (v) 
		  (parameterize ([current-output-port user-output-port])
		    (print v) 
		    (newline)))
		results))))
	  (lambda ()
	    (queue-output (lambda () (send repl-buffer new-prompt)))))))))

  (define waiting (make-semaphore 0))

  (let ([mb (make-object menu-bar% frame)])
    (let ([m (make-object menu% "&File" mb)])
      (make-object menu-item% "Load File..." m (lambda (i e) (let ([f (get-file #f frame)]) (and f (evaluate (format "(load ~s)" f))))))
      (unless (current-eventspace-has-standard-menus?)
	(make-object menu-item% 
		     (if (eq? (system-type) 'windows)
			 "E&xit"
			 "&Quit")
		     m (lambda (i e) (send frame on-close) (send frame show #f)) #\q)))
    (let ([m (make-object menu% "&Edit" mb)])
      (append-editor-operation-menu-items m #f)))

  ;; Just a few extra key bindings:
  ((current-text-keymap-initializer) (send repl-buffer get-keymap))
  (send repl-buffer auto-wrap #t)

  ;; Go
  (when override-ports?
    (parameterize ((wx:current-eventspace user-eventspace))
      (wx:queue-callback
       (lambda ()
	 (current-output-port user-output-port)
	 (current-error-port user-output-port)
	 (current-input-port (open-input-bytes #"")))
       #t)))

  (send repl-display-canvas set-editor repl-buffer)

  (send frame show #t)
  
  (send repl-display-canvas focus)

  (wx:yield waiting))

(define graphical-read-eval-print-loop
  (case-lambda
   [() (-graphical-read-eval-print-loop #f #t)]
   [(esp)
    (graphical-read-eval-print-loop esp (not esp))]
   [(esp override-ports?)
    (unless (or (not esp) (wx:eventspace? esp))
      (raise-type-error 'graphical-read-eval-print-loop "eventspace or #f" esp))
    (-graphical-read-eval-print-loop esp override-ports?)]))

(define box-width 300)
(define (no-stretch a) (send a stretchable-width #f) (send a stretchable-height #f))

(define protect&
  (lambda (s)
    (regexp-replace* #rx"&" s "\\&\\&")))


(define message-box/custom
  (opt-lambda (title message 
		     button1
		     button2
		     button3
		     [parent #f]
		     [style '(no-default)]
		     [close-result #f])
    (check-label-string 'message-box/custom title)
    (check-string/false 'message-box/custom message)
    (check-label-string-or-bitmap/false 'message-box/custom button1)
    (check-label-string-or-bitmap/false 'message-box/custom button2)
    (check-label-string-or-bitmap/false 'message-box/custom button3)
    (check-top-level-parent/false 'message-box/custom parent)
    (check-style 'message-box/custom 
		 '(default=1 default=2 default=3 no-default) 
		 '(disallow-close number-order caution stop)
		 style)

    (let* ([strings (let loop ([s message])
		      (let ([m (regexp-match #rx"([^\n]*)[\n](.*)" s)])
			(if m
			    (cons (cadr m) (loop (caddr m)))
			    (list s))))]
	   [single? (and (< (length strings) 10) 
			 (andmap (lambda (s) (< (string-length s) 60)) strings))]
	   [f (make-object (class100 dialog% ()
			     (public
			       [get-message
				(lambda () message)])
			     (override
			       [can-close? (lambda ()
					     (if (memq 'disallow-close style)
						 (begin
						   (wx:bell)
						   #f)
						 #t))]
			       [on-subwindow-event 
				(lambda (w e)
				  (if (send e button-down?)
				      (if (is-a? w button%)
					  #f
					  (if (or (is-a? w message%)
						  (and
						   (is-a? w editor-canvas%)
						   (let-values ([(w h) (send w get-client-size)])
						     (< (send e get-x) w))))
					      (begin
						(send w popup-menu
						      (let ([m (make-object popup-menu%)])
							(make-object menu-item%
								     "Copy Message"
								     m
								     (lambda (i e)
								       (send (wx:get-the-clipboard)
									     set-clipboard-string
									     message
									     (send e get-time-stamp))))
							m)
						      (send e get-x)
						      (send e get-y))
						#t)
					      #f))
				      #f))])
			     (sequence
			       (super-init title parent box-width))))]
	   [result close-result]
	   [icon-id (cond
		     [(memq 'stop style) 'stop]
		     [(memq 'caution style) 'caution]
		     [else 'app])])
      (let-values ([(msg-pnl btn-pnl extra-width btn-h-align msg-h-align msg-v-align)
		    (case (system-type)
		      [(macosx) (let ([p (make-object horizontal-pane% f)])
				  (send f min-width 300)
				  (send p set-alignment 'center 'top)
				  (let ([m (make-object message% icon-id p)])
				    (send m horiz-margin 16)
				    (send m vert-margin 16))
				  (let* ([rhs-pnl (make-object vertical-pane% p)]
					 [msg-pnl (make-object vertical-pane% rhs-pnl)]
					 [btn-pnl (make-object vertical-pane% rhs-pnl)])
				    (send msg-pnl vert-margin 16)
				    (when single?
				      (send msg-pnl horiz-margin 8))
				    (send btn-pnl vert-margin 8)
				    (send msg-pnl min-height 64)
				    (send btn-pnl stretchable-height #f)
				    (values msg-pnl btn-pnl 96 'right 'left 'top)))]
		      [else (let ([p (instantiate horizontal-pane% (f) [alignment '(center top)])])
			      (make-object message% icon-id p)
			      (values (make-object vertical-pane% p) f 0 'center 'center 'center))])])
	(if single?
	    (begin
	      (send msg-pnl set-alignment (if (= (length strings) 1) msg-h-align 'left) msg-v-align)
	      (for-each (lambda (s) (make-object message% (protect& s) msg-pnl)) strings)
	      (send f stretchable-width #f)
	      (send f stretchable-height #f))
	    (let* ([e (make-object text%)]
		   [c (make-object editor-canvas% msg-pnl e '(no-hscroll))])
	      (send f resize (+ 400 extra-width) 200)
	      (send c set-line-count (min 5 (length strings)))
	      (send c allow-tab-exit #t)
	      (send f reflow-container)
	      (send e auto-wrap #t)
	      (send e insert message)
	      (send e set-position 0)
	      (send e hide-caret #t)
	      (send e set-cursor (make-object wx:cursor% 'arrow) #t)
	      (send e lock #t)))
	(let* ([p (make-object horizontal-pane% btn-pnl)]
	       [mk-button (lambda (title v default?) 
			    (let ([b (make-object button% title p (lambda (b e) (set! result v) (send f show #f))
						  (if default? '(border) null))])
			      (when default? (send b focus))))])
	  (send p set-alignment btn-h-align 'center)
	  (send p stretchable-height #f)
	  (send p stretchable-width #t) ; to get panel's centering
	  (let ([mk-1 (lambda () 
			(when button1
			  (mk-button button1 1 (memq 'default=1 style))))]
		[mk-2 (lambda ()
			(when button2
			  (mk-button button2 2 (memq 'default=2 style))))]
		[mk-3 (lambda ()
			(when button3
			  (mk-button button3 3 (memq 'default=3 style))))])
	    (cond 
	     [(or (memq 'number-order style)
		  (not (memq (system-type) '(macos macosx)))) 
	      (mk-1)
	      (mk-2)
	      (mk-3)]
	     [else
	      (mk-3)
	      (make-object horizontal-pane% p)
	      (mk-2)
	      (mk-1)])))
	(send f center)
	(send f show #t)
	result))))

(define message-box
  (case-lambda
   [(title message) (message-box title message #f '(ok))]
   [(title message parent) (message-box title message parent '(ok))]
   [(title message parent style)
    (check-label-string 'message-box title)
    (check-string/false 'message-box message)
    (check-top-level-parent/false 'message-box parent)
    (check-style 'message-box '(ok ok-cancel yes-no) '(caution stop) style)

    (let-values ([(one two one-v two-v close-val default)
		  (cond
		    [(memq 'ok style) 
		     (values "OK" #f 'ok #f 1 'default=1)]
		    [(memq 'ok-cancel style)
		     (values "OK" "Cancel" 'ok 'cancel 2 'default=1)]
		    [(memq 'yes-no style)
		     (values "&Yes" "&No" 'yes 'no #f 'no-default)])])
      (case (message-box/custom title message
				 one two #f
				 parent
				 (append
				  (cond
				   [(memq 'stop style) '(stop)]
				   [(memq 'caution style) '(caution)]
				   [else null])
				  (if close-val
				      (list default)
				      (list default 'disallow-close)))
				 close-val)
	[(1) one-v]
	[(2) two-v]))]))

(define (number->string* n)
  (let ([s (number->string n)])
    (regexp-replace #rx"[.]([0-9][0-9][0-9])[0-9]*$"
		    s
		    ".\\1")))

(define get-ps-setup-from-user
  (case-lambda
   [() (get-ps-setup-from-user #f #f #f null)]
   [(message) (get-ps-setup-from-user message #f #f null)]
   [(message parent) (get-ps-setup-from-user message parent #f null)]
   [(message parent pss) (get-ps-setup-from-user message parent pss null)]
   [(message parent pss-in style)
    (define _
      (begin
	;; Calls from C++ have wrong kind of window:
	(when (is-a? parent wx:window%)
	  (set! parent (as-entry (lambda () (wx->mred parent)))))

	(check-label-string/false 'get-ps-setup-from-user message)
	(check-top-level-parent/false 'get-ps-setup-from-user parent)
	(check-instance 'get-ps-setup-from-user wx:ps-setup% 'ps-setup% #t pss-in)
	(check-style 'get-ps-setup-from-user #f null style)))
    
    (define pss (or pss-in (wx:current-ps-setup)))
    (define f (make-object dialog% "PostScript Setup" parent))
    (define papers 
      '("A4 210 x 297 mm" "A3 297 x 420 mm" "Letter 8 1/2 x 11 in" "Legal 8 1/2 x 14 in"))
    (define p (make-object horizontal-pane% f))
    (define paper (make-object choice% #f papers p void))
    (define _0 (make-object vertical-pane% p))
    (define cancel (make-object button% "Cancel" p (lambda (b e) (done #f))))
    (define ok (make-object button% "OK" p (lambda (b e) (done #t)) '(border)))
    (define unix? (eq? (system-type) 'unix))
    (define dp (make-object horizontal-pane% f))
    (define orientation (make-object radio-box% "Orientation:" '("Portrait" "Landscape") dp void))
    (define destination (and unix? (make-object radio-box% "Destination:" 
						'("Printer" "Preview" "File") dp void)))
    (define ssp (make-object horizontal-pane% f))
    (define sp (make-object vertical-pane% ssp))
    (define def-scale "0100.000")
    (define def-offset "0000.000")
    (define xscale (make-object text-field% "Horizontal Scale:" sp void def-scale))
    (define xoffset (make-object text-field% "Horizontal Translation:" sp void def-offset))
    (define sp2 (make-object vertical-pane% ssp))
    (define yscale (make-object text-field% "Vertical Scale:" sp2 void def-scale))
    (define yoffset (make-object text-field% "Vertical Translation:" sp2 void def-offset))

    (define l2 (make-object check-box% "PostScript Level 2" f void))

    (define cp (and unix? (make-object horizontal-pane% f)))
    (define command (and unix? (make-object text-field% "Print Command:" cp void)))
    (define vcommand (and unix? (make-object text-field% "Preview Command:" f void)))

    (define ok? #f)
    (define (done ?)
      (send f show #f)
      (set! ok? ?))

    (define-values (xsb ysb xtb ytb) (values (box 0) (box 0) (box 0) (box 0)))

    (send paper set-selection (or (find-pos papers (send pss get-paper-name) equal?) 0))
    (send orientation set-selection (if (eq? (send pss get-orientation) 'landscape) 1 0))
    (when unix?
      (send destination set-selection (case (send pss get-mode)
					[(printer) 0] [(preview) 1] [(file) 2]))
      (send command set-value (send pss get-command))
      (send vcommand set-value (send pss get-preview-command)))

    (send sp set-alignment 'right 'top)
    (send sp2 set-alignment 'right 'top)
    (send pss get-scaling xsb ysb)
    (send xscale set-value (number->string* (unbox xsb)))
    (send yscale set-value (number->string* (unbox ysb)))
    (send pss get-translation xtb ytb)
    (send xoffset set-value (number->string* (unbox xtb)))
    (send yoffset set-value (number->string* (unbox ytb)))
    (send xscale stretchable-width #f)
    (send yscale stretchable-width #f)
    (send xoffset stretchable-width #f)
    (send yoffset stretchable-width #f)

    (send l2 set-value (send pss get-level-2))

    (send f set-alignment 'center 'top)

    (map no-stretch (list f xscale yscale xoffset yoffset dp))

    (send f center)

    (send f show #t)

    (if ok?
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
	    (send s set-preview-command (send vcommand get-value)))

	  s)
	#f)]))

(define get-text-from-user
  (case-lambda
   [(title message) (get-text-from-user title message #f "" null)]
   [(title message parent) (get-text-from-user title message parent "" null)]
   [(title message parent init-val) (get-text-from-user title message parent init-val null)]
   [(title message parent init-val style)
    (check-label-string 'get-text-from-user title)
    (check-label-string/false 'get-text-from-user message)
    (check-top-level-parent/false 'get-text-from-user parent)
    (check-string 'get-text-from-user init-val)
    (check-style 'get-text-from-user #f '(password) style)
    (let* ([f (make-object dialog% title parent box-width)]
	   [ok? #f]
	   [done (lambda (?) (lambda (b e) (set! ok? ?) (send f show #f)))])
      (send f set-label-position 'vertical)
      (let ([t (make-object text-field% message f (lambda (t e) (when (eq? (send e get-event-type) 'text-field-enter)
								  ((done #t) #f #f)))
			    init-val (cons 'single style))]
	    [p (make-object horizontal-pane% f)])
	(send p set-alignment 'right 'center)
	(send f stretchable-height #f)
	(make-object button% "Cancel" p (done #f))
	(make-object button% "OK" p (done #t) '(border))
	(send (send t get-editor) select-all)
	(send t focus)
	(send f center)
	(send f show #t)
	(and ok? (send t get-value))))]))

(define get-choices-from-user
  (case-lambda
   [(title message choices) (get-choices-from-user title message choices #f null '(single))]
   [(title message choices parent) (get-choices-from-user title message choices parent null '(single))]
   [(title message choices parent init-vals) (get-choices-from-user title message choices parent init-vals '(single))]
   [(title message choices parent init-vals style)
    (check-label-string 'get-choices-from-user title)
    (check-label-string/false 'get-choices-from-user message)
    (unless (and (list? choices) (andmap label-string? choices))
      (raise-type-error 'get-choices-from-user "list of strings (up to 200 characters)" choices))
    (check-top-level-parent/false 'get-choices-from-user parent)
    (unless (and (list? init-vals) (andmap (lambda (x) (and (integer? x) (exact? x) (not (negative? x)))) init-vals))
      (raise-type-error 'get-choices-from-user "list of exact non-negative integers" init-vals))
    (check-style 'get-choices-from-user '(single multiple extended) null style)
    (when (and (memq 'single style) (> (length init-vals) 1))
      (raise-mismatch-error 'get-choices-from-user 
			    (format "multiple initial-selection indices provided with ~e style: " 'single)
			    init-vals))
    (let* ([f (make-object dialog% title parent box-width (min 300 (max 150 (* 14 (length choices)))))]
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
	(for-each (lambda (i) 
		    (when (>= i (send l get-number))
		      (raise-mismatch-error 
		       'get-choices-from-user 
		       (format "inital-selection list specifies an out-of-range index (~e choices provided): "
			       (send l get-number))
		       i))
		    (send l select i #t)) init-vals)
	(send p set-alignment 'right 'center)
	(send p stretchable-height #f)
	(make-object button% "Cancel" p (done #f))
	(set! ok-button (make-object button% "OK" p (done #t) '(border)))
	(update-ok l)
	(send f center)
	(when (and (pair? init-vals)
		   ((car init-vals) . > . 1))
	  ;; Make sure initial selection is visible:
	  (send f reflow-container)
	  (send l set-first-visible-item (sub1 (car init-vals))))
	(send f show #t)
	(and ok? (send l get-selections))))]))

(define last-visted-directory #f)

(define (files->list s)
  (let ([s (open-input-bytes s)])
    (let loop ()
      (let ([n (read s)])
	(if (eof-object? n)
	    null
	    (begin
	      (read-byte s) ; drop space
	      (cons (read-bytes n s)
		    (loop))))))))

(define (mk-file-selector who put? multi? dir? force-unix?)
  (lambda (message parent directory filename extension style filters)
    ;; Calls from C++ have wrong kind of window:
    (when (is-a? parent wx:window%)
      (set! parent (as-entry (lambda () (wx->mred parent)))))

    (check-label-string/false who message)
    (check-top-level-parent/false who parent)
    (check-path/false who directory) (check-path/false who filename) (check-string/false who extension)
    (check-style who #f (cond
			 [put? null]
			 [dir? '(enter-packages)]
			 [else '(packages enter-packages)]) style)
    (unless (and (list? filters)
		 (andmap (lambda (p)
			   (and (list? p)
				(= (length p) 2)
				(string? (car p))
				(string? (cadr p))))
			 filters))
      (raise-type-error who "list of 2-string lists" filters))
    (if (not (or (eq? (system-type) 'unix)
		 force-unix?))
	(let ([s (wx:file-selector message directory filename extension 
				   ;; file types:
				   (apply string-append
					  (map (lambda (s) (format "~a|~a|" (car s) (cadr s)))
					       filters))
				   ;; style:
				   (cons
				    (cond
				     [dir? 'dir]
				     [put? 'put]
				     [multi? 'multi]
				     [else 'get])
				    style)
				   ;; parent:
				   (and parent (mred->wx parent)))])
	  (if (and multi? s)
	      (map bytes->path (files->list (path->bytes s)))
	      s))
	(letrec ([ok? #f]
		 [typed-name #f]
		 [dir (or (and directory (if (string? directory)
					     (string->path directory)
					     directory))
			  last-visted-directory 
			  (current-directory))]
		 [f (make-object dialog% (if dir? "Select Directory" (if put? "Save" "Open")) parent 500 300)]
		 [__ (when message
		       (let ([p (make-object vertical-pane% f)])
			 (send p stretchable-height #f)
			 (make-object message% (protect& message) p)))]
                 [dir-pane (instantiate horizontal-pane% (f) (stretchable-height #f))]
		 [m (make-object message% (protect& (path->string dir)) dir-pane)]
                 [lp (make-object horizontal-pane% f)]
                 [change-dir (lambda (d) (let ([sd (send d get-string-selection)])
                                           (set! dir (simplify-path (build-path dir sd)))
                                           (reset-directory)))]
		 [dirs (make-object (class list-box%
                                      (rename [super-on-subwindow-char on-subwindow-char])
                                      (define/override (on-subwindow-char w e)
                                        (cond
                                          [(and (send e get-meta-down)
                                                (eq? (send e get-key-code) 'down))
                                           (change-dir w)]
                                          [(and (send e get-meta-down)
                                                (eq? (send e get-key-code) 'up))
                                           (send dirs set-selection 0)
                                           (change-dir dirs)]
                                          [else
                                           (super-on-subwindow-char w e)]))
                                      (super-instantiate ()))
                         #f null lp (lambda (d e)
                                      (update-ok)
                                      (when (eq? (send e get-event-type) 'list-box-dclick)
                                        (change-dir d))))]
		 [dir-paths null]
		 [files (make-object list-box% #f null lp (lambda (d e)
							    (update-ok)
							    (when (eq? (send e get-event-type) 'list-box-dclick)
							      (done)))
				     (if multi? '(multiple) '(single)))]
		 [file-paths null]
		 [do-text-name (lambda ()
				 (let ([v (send dir-text get-value)])
				   (if (or dir? (directory-exists? v))
				       (begin
					 (set! dir (string->path v))
					 (reset-directory))
				       ;; Maybe specifies a file:
				       (let-values ([(super file) 
						     (with-handlers ([void #f])
						       (let-values ([(base name dir?) (split-path v)])
							 (let ([super (and (not dir?) 
									   (or (and (path? base) 
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
					       (set! dir (string->path v))
					       (reset-directory)))))))]
		 [dir-text (make-object text-field% #f f (lambda (t e)
							   (if (eq? (send e get-event-type) 'text-field-enter)
							       (do-text-name)
							       (begin
								 ; typing in the box; disable the lists and enable ok
								 (send dirs enable #f)
                                                                 (send files enable #f)
								 (when create-button
								   (send create-button enable #t))
								 (send ok-button enable #t)))))]
		 [bp (make-object horizontal-pane% f)]
		 [dot-check (make-object check-box% "Show files/directories that start with \".\"" bp (lambda (b e) (reset-directory)))]
		 [spacer (make-object vertical-pane% bp)]
		 [create-button (and dir? (make-object button% "Create" bp
						       (lambda (b e)
							 (with-handlers ([void
									  (lambda (exn)
									    (message-box "Error"
											 (exn-message exn)
											 f
											 '(ok stop)))])
							   (make-directory (send dir-text get-value))
							   (do-text-name)))))]
		 [cancel-button (make-object button% "Cancel" bp (lambda (b e) (set! ok? #f) (send f show #f)))]
		 [ok-button (make-object button% 
                              (if dir? "Goto" "OK")
                              bp (lambda (b e) 
                                   (if (send (if dir? dirs files) is-enabled?)
                                       ;; normal mode
                                       (if dir?
                                           (change-dir dirs)
                                           (done))
                                       ;; handle typed text
                                       (do-text-name))) 
                              '(border))]
		 [update-ok (lambda () (send ok-button enable (not (null? (send (if dir? dirs files) get-selections)))))]
		 [select-this-dir (and dir?
				       (make-object button% "<- &Select" dir-pane
						    (lambda (b e)
						      (send f show #f)
						      (done))))]
		 [path-string-locale<? (lambda (p1 p2)
					 (string-locale<? (path->string p1) (path->string p2)))]
		 [reset-directory (lambda ()
				    (wx:begin-busy-cursor)
                                    (let ([dir-exists? (directory-exists? dir)])
                                      (send m set-label (protect&
							 (if dir-exists?
							     (begin
							       (unless directory
								 (set! last-visted-directory dir))
                                                               (path->string dir))
							     (string-append "BAD DIRECTORY: " (path->string dir)))))
                                      (when select-this-dir
                                        (send select-this-dir enable dir-exists?))
				      (when create-button
					(send create-button enable (not dir-exists?))))
				    (send dir-text set-value (path->string dir))
				    (let ([l (with-handlers ([void (lambda (x) null)])
					       (directory-list dir))]
					  [dot? (send dot-check get-value)])
				      (let-values ([(ds fs)
						    (let loop ([l l][ds null][fs null])
						      (cond
						       [(null? l) (values (cons (string->path "..")
										(quicksort ds path-string-locale<?)) 
									  (quicksort fs path-string-locale<?))]
						       [(and (not dot?) 
							     (char=? (string-ref (path->string (car l)) 0) #\.)) 
							(loop (cdr l) ds fs)]
						       [(file-exists? (build-path dir (car l))) (loop (cdr l) ds (cons (car l) fs))]
						       [else (loop (cdr l) (cons (car l) ds) fs)]))])
					(set! dir-paths ds)
					(send dirs set (map path->string ds))
					(set! file-paths fs)
					(send files set (map path->string fs))
					(send dirs enable #t)
					(unless dir?
					  (send files enable #t))
					(update-ok)
					(wx:end-busy-cursor))))]
		 [get-filename (lambda () 
                                 (if dir?
                                     dir
                                     (let ([mk (lambda (f) (simplify-path (build-path dir f)))])
                                       (let ([l (map mk (if typed-name
                                                            (list typed-name)
                                                            (map (lambda (p) (list-ref (if dir? dir-paths file-paths) p))
                                                                 (send (if dir? dirs files) get-selections))))])
                                         (if multi? l (car l))))))]
		 [done (lambda ()
			 (let ([name (get-filename)])
                           (unless (and put? (file-exists? name)
                                        (eq? (message-box "Warning" 
							  (format "Replace ~s?" (path->string name) )
							  f '(yes-no)) 
					     'no)
                                        (set! typed-name #f))
			     (set! ok? #t)
			     (send f show #f))))])
	  (send bp stretchable-height #f)
	  (send m stretchable-width #t)
	  (reset-directory)
	  (when filename
	    (when (string? filename)
	      (set! filename (string->path filename)))
	    (let ([d (send dir-text get-value)])
	      (send dir-text set-value (path->string (build-path d filename)))
	      (set! typed-name filename)
	      (send ok-button enable #t)))
	  (when put?
	    (send dir-text focus))
          (when dir?
            (send files enable #f))
	  (send f center)
	  (send f show #t)
	  (and ok? (get-filename))))))

; We duplicate the case-lambda for `get-file', `get-file-list', and `put-file' so that they have the
;   right arities and names

(define default-filters '(("Any" "*.*")))

(define get-file
  (case-lambda
   [() (get-file #f #f #f #f #f null)]
   [(message) (get-file message #f #f #f #f null)]
   [(message parent) (get-file message parent #f #f #f null)]
   [(message parent directory) (get-file message parent directory #f #f null)]
   [(message parent directory filename) (get-file message parent directory filename #f null)]
   [(message parent directory filename extension) (get-file message parent directory filename extension null)]
   [(message parent directory filename extension style)
    (get-file message parent directory filename extension style default-filters)]
   [(message parent directory filename extension style filters)
    ((mk-file-selector 'get-file #f #f #f #f) message parent directory filename extension style filters)]))

(define get-file-list
  (case-lambda
   [() (get-file-list #f #f #f #f #f null)]
   [(message) (get-file-list message #f #f #f #f null)]
   [(message parent) (get-file-list message parent #f #f #f null)]
   [(message parent directory) (get-file-list message parent directory #f #f null)]
   [(message parent directory filename) (get-file-list message parent directory filename #f null)]
   [(message parent directory filename extension) (get-file-list message parent directory filename extension null)]
   [(message parent directory filename extension style)
    (get-file-list message parent directory filename extension style default-filters)]
   [(message parent directory filename extension style filters)
    ((mk-file-selector 'get-file-list #f #t #f #f) message parent directory filename extension style filters)]))

(define put-file
  (case-lambda
   [() (put-file #f #f #f #f #f null)]
   [(message) (put-file message #f #f #f #f null)]
   [(message parent) (put-file message parent #f #f #f null)]
   [(message parent directory) (put-file message parent directory #f #f null)]
   [(message parent directory filename) (put-file message parent directory filename #f null)]
   [(message parent directory filename extension) (put-file message parent directory filename extension null)]
   [(message parent directory filename extension style)
    (put-file message parent directory filename extension style default-filters)]
   [(message parent directory filename extension style filters)
    ((mk-file-selector 'put-file #t #f #f #f) message parent directory filename extension style filters)]))

(define get-directory
  (case-lambda
   [() (get-directory #f #f #f null)]
   [(message) (get-directory message #f #f null)]
   [(message parent) (get-directory message parent #f null)]
   [(message parent directory) (get-directory message parent directory null)]
   [(message parent directory style)
    ((mk-file-selector 'get-directory #f #f #t #f) message parent directory #f #f style null)]))

(define get-color-from-user 
  (case-lambda
   [() (get-color-from-user #f #f #f null)]
   [(message) (get-color-from-user message #f #f null)]
   [(message parent) (get-color-from-user message parent #f null)]
   [(message parent color) (get-color-from-user message parent color null)]
   [(message parent color style)
    (check-label-string/false 'get-color-from-user message)
    (check-top-level-parent/false 'get-color-from-user parent)
    (check-instance 'get-color-from-user wx:color% 'color% #t color)
    (check-style 'get-color-from-user #f null style)
    (if (not (eq? (system-type) 'unix))
	(wx:get-color-from-user message (and parent (mred->wx parent)) color)
	(letrec ([ok? #f]
		 [f (make-object dialog% "Choose Color" parent)]
		 [done (lambda (ok) (lambda (b e) (set! ok? ok) (send f show #f)))]
		 [canvas (make-object (class100 canvas% ()
					     (override
					       [on-paint (lambda () (repaint #f #f))])
					     (sequence (super-init f))))]
		 [p (make-object vertical-pane% f)]
		 [repaint (lambda (s e)
			    (let ([c (make-object wx:color% 
						  (send red get-value)
						  (send green get-value)
						  (send blue get-value))])
			      (wx:fill-private-color (send canvas get-dc) c)))]
		 [make-color-slider (lambda (l) (make-object slider% l 0 255 p repaint))]
		 [red (make-color-slider "Red:")]
		 [green (make-color-slider "Green:")]
		 [blue (make-color-slider "Blue:")]
		 [bp (make-object horizontal-pane% f)])
	  (when color
	    (send red set-value (send color red))
	    (send green set-value (send color green))
	    (send blue set-value (send color blue)))
	  (make-object button% "Cancel" bp (done #f))
	  (send (make-object button% "OK" bp (done #t) '(border)) focus)
	  (send bp set-alignment 'right 'center)
	  (send p set-alignment 'right 'center)
	  (send p stretchable-height #f)
	  (send canvas min-height 50)
	  (send f center)
	  (send f show #t)
	  (and ok?
	       (make-object wx:color% 
                            (send red get-value)
                            (send green get-value)
			    (send blue get-value)))))]))

(define get-font-from-user 
  (case-lambda
   [() (get-font-from-user #f #f #f null)]
   [(message) (get-font-from-user message #f #f null)]
   [(message parent) (get-font-from-user message parent #f null)]
   [(message parent font) (get-font-from-user message parent font null)]
   [(message parent font style)
    (check-label-string/false 'get-font-from-user message)
    (check-top-level-parent/false 'get-font-from-user parent)
    (check-instance 'get-font-from-user wx:font% 'font% #t font)
    (check-style 'get-font-from-user #f null style)
    (letrec ([ok? #f]
	     [f (make-object dialog% "Choose Font" parent 500 300)]
	     [refresh-sample (lambda (b e) (let ([f (get-font)])
					     (send ok-button enable f)
					     (when f
					       (let ([s (send (send edit get-style-list) find-named-style "Standard")])
						 (send s set-delta (font->delta f))))))]
	     [p (make-object horizontal-pane% f)]
	     [face (make-object list-box% #f
				(let ([l (wx:get-face-list)]
				      [ugly? (lambda (a)
					       (and (positive? (string-length a))
						    (not (or (char-alphabetic? (string-ref a 0))
							     (char-numeric? (string-ref a 0))
							     (char=? #\- (string-ref a 0))))))])
				  ;; Sort space-starting first (for Xft), and
				  ;;  otherwise push names that start with an
				  ;;  ASCII non-letter/digit/hyphen to the end
				  (quicksort l (lambda (a b)
						 (let ([a-sp? (char=? #\space (string-ref a 0))]
						       [b-sp? (char=? #\space (string-ref b 0))]
						       [a-ugly? (ugly? a)]
						       [b-ugly? (ugly? b)])
						   (cond
						    [(eq? a-sp? b-sp?)
						     (cond
						      [(eq? a-ugly? b-ugly?)
						       (string-locale-ci<? a b)]
						      [else
						       b-ugly?])]
						    [else a-sp?])))))
				p refresh-sample)]
	     [p2 (make-object vertical-pane% p)]
	     [p3 (instantiate horizontal-pane% (p2) [stretchable-width #f])]
	     [style (let ([pnl (instantiate group-box-panel% ("Style" p3) [stretchable-height #f] [stretchable-width #f])])
		      (make-object radio-box% #f '("Normal" "Italic" "Slant") pnl refresh-sample))]
	     [weight (let ([pnl (instantiate group-box-panel% ("Weight" p3) [stretchable-height #f] [stretchable-width #f])])
		       (make-object radio-box% #f '("Normal" "Bold" "Light") pnl refresh-sample))]
	     [p4 (instantiate vertical-pane% (p3) [alignment '(left center)])]
	     [underlined (make-object check-box% "Underlined" p4 refresh-sample)]
	     [smoothing (make-object choice% "Smoothing:" '("Default" "Some" "Full" "None") p4 refresh-sample)]
	     [sip (make-object check-box% "Size in Pixels" p4 refresh-sample)]
	     [sym (make-object check-box% "Map as Symbol" p4 refresh-sample)]
	     [size (make-object slider% "Size:" 4 127 p2 refresh-sample 12)]
	     [sample (make-object text-field% "Sample" f void "The quick brown fox jumped over the lazy dog" '(multiple))]
	     [edit (send sample get-editor)]
	     [done (lambda (ok) (lambda (b e) (set! ok? ok) (send f show #f)))]
	     [get-font (lambda () (let ([face (send face get-string-selection)])
				    (and face
					 (make-object wx:font% (send size get-value) face 
						      (if (send sym get-value)
							  'symbol
							  'default)
						      (case (send style get-selection) [(0) 'normal] [(1) 'italic] [(2) 'slant])
						      (case (send weight get-selection) [(0) 'normal] [(1) 'bold] [(2) 'light])
						      (send underlined get-value)
						      (case (send smoothing get-selection) 
							[(0) 'default] 
							[(1) 'partly-smoothed]
							[(2) 'smoothed]
							[(3) 'unsmoothed])
						      (send sip get-value)))))]
	     [bp (instantiate horizontal-pane% (f) [stretchable-height #f])]
	     [ms-button (if (eq? (system-type) 'windows)
			    (begin0
			     (make-object button% "Use System Dialog..." bp
					  (lambda (b e)
					    (let ([new-font (wx:get-font-from-user 
							     message 
							     (mred->wx f)
							     (get-font))])
					      (when new-font
						(reset-font new-font)))))
			     ;; Spacer:
			     (make-object pane% bp))
			    (void))]
	     [cancel-button (make-object button% "Cancel" bp (done #f))]
	     [ok-button (make-object button% "OK" bp (done #t) '(border))]
	     [reset-font 
	      (lambda (font)
		(let* ([facen (if font
				  (send font get-face)
				  (get-family-builtin-face 'default))]
		       [f (and facen (send face find-string facen))])
		  (and f (>= f 0) (send face set-selection f)))
		(when font
		  (send style set-selection (case (send font get-style) [(normal) 0] [(italic) 1] [(slant) 2]))
		  (send weight set-selection (case (send font get-weight) [(normal) 0] [(bold) 1] [(light) 2]))
		  (send underlined set-value (send font get-underlined))
		  (send size set-value (send font get-point-size))
		  (send sip set-value (send font get-size-in-pixels)))
		(refresh-sample (void) (void)))])
      (send bp set-alignment 'right 'center)
      (send face min-width (max 200 (let-values ([(w h) (send face get-graphical-min-size)]) w)))
      (reset-font font)
      (send f center)
      (send f show #t)
      (and ok? (get-font)))]))

(define (play-sound f async?)
  (if (not (eq? (system-type) 'unix))
      (wx:play-sound f async?)
      (begin
	(unless (string? f)
	  (raise-type-error 'play-sound "string" f))
	(let* ([subpath (system-library-subpath)]
	       [make-pattern (lambda (s) (string-append ".*" s ".*"))]
	       [b (box 
		   (cond 
		    [(regexp-match (make-pattern "linux") subpath)
		     ;; use play interface to sox
		     "play ~s"]
		    [(regexp-match (make-pattern "solaris") subpath)
		     "audioplay ~s"]
		    [else
		     (raise-mismatch-error
		      'play-sound
		      "not supported by default on this platform"
		      subpath)]))])
	  ; see if user has overridden defaults 		  
	  (let ([r (get-preference '|MrEd:playcmd| (lambda () #f))])
	    (when (and r (string? r))
	      (set-box! b r)))
	  ((if async? (lambda (x) (process x) #t) system)
	   (format (unbox b) (expand-path f)))))))

(define get-display-size
  (opt-lambda ([full-screen? #f])
    (let ([xb (box 0)]
	  [yb (box 0)])
      (wx:display-size xb yb (if full-screen? 1 0))
      (values (unbox xb) (unbox yb)))))

(define (get-display-left-top-inset)
  (let ([xb (box 0)]
	[yb (box 0)])
    (wx:display-origin xb yb)
    (values (unbox xb) (unbox yb))))

;; Currently only used for PS print and preview
(wx:set-executer
 (let ([orig-err (current-error-port)])
   (lambda (prog . args)
     (let ([cmd (string-append
		 prog 
		 (let loop ([args args])
		   (if (null? args)
		       ""
		       (format " ~s~a" (car args) (loop (cdr args))))))])
       (let-values ([(in out pid err x) (apply values (process cmd))])
	 (close-output-port out)
	 (let ([echo (lambda (p)
		       (thread (lambda ()
				 (dynamic-wind
				     void
				     (lambda ()
				       (let loop ()
					 (let ([l (read-line p)])
					   (unless (eof-object? l)
					     (fprintf orig-err "~a~n" l)
					     (loop)))))
				     (lambda () (close-input-port p))))))])
	   (echo in)
	   (echo err)
	   (void)))))))

(define register-collecting-blit
  (case-lambda
   [(canvas x y w h on off) (register-collecting-blit canvas x y w h on off 0 0 0 0)]
   [(canvas x y w h on off on-x) (register-collecting-blit canvas x y w h on off on-x 0 0 0)]
   [(canvas x y w h on off on-x on-y) (register-collecting-blit canvas x y w h on off on-x on-y 0 0)]
   [(canvas x y w h on off on-x on-y off-x) (register-collecting-blit canvas x y w h on off on-x on-y off-x 0)]
   [(canvas x y w h on off on-x on-y off-x off-y)
    (check-instance 'register-collecting-blit canvas% 'canvas% #f canvas)
    (wx:register-collecting-blit (mred->wx canvas) x y w h on off on-x on-y off-x off-y)]))

(define unregister-collecting-blit
  (lambda (canvas)
    (check-instance 'unregister-collecting-blit canvas% 'canvas% #f canvas)
    (wx:unregister-collecting-blit (mred->wx canvas))))

(define bitmap-dc%
  (class100 wx:bitmap-dc% ([bitmap #f])
    (inherit set-bitmap)
    (sequence
      (super-init)
      (when bitmap
	(set-bitmap bitmap)))))

(define post-script-dc%
  (class100 wx:post-script-dc% ([interactive #t][parent #f][use-paper-bbox #f][as-eps #t])
    (sequence
      (check-top-level-parent/false '(constructor post-script-dc) parent)
      (as-entry
       (lambda ()
	 (let ([p (and parent (mred->wx parent))])
	   (as-exit (lambda () (super-init interactive p use-paper-bbox as-eps)))))))))

(define printer-dc%
  (class100 wx:printer-dc% ([parent #f])
    (sequence
      (check-top-level-parent/false '(constructor printer-dc) parent)
      (as-entry
       (lambda ()
	 (let ([p (and parent (mred->wx parent))])
	   (as-exit (lambda () (super-init p)))))))))

(define (find-item-editor item)
  (let ([o (let loop ([i item])
	     (let ([p (send i get-parent)])
	       (cond
		[(not p) #f]
		[(is-a? p popup-menu%)
		 (let ([p (send p get-popup-target)])
		   (if (is-a? p window<%>)
		       (let ([f (send p get-top-level-window)])
			 (and f (send f get-edit-target-object)))
		       p))]
		[(is-a? p menu%) (loop p)]
		[else (let ([f (send p get-frame)])
			(and f (send f get-edit-target-object)))])))])
    (and (is-a? o wx:editor<%>)
	 o)))

(define append-editor-operation-menu-items
  (case-lambda
   [(m) (append-editor-operation-menu-items m #t)]
   [(m text-only?)
    (menu-parent-only 'append-editor-operation-menu-items m)
    (let* ([mk (lambda (name key op)
		 (make-object (class100 menu-item% ()
				(inherit enable)
				(override
				  [on-demand
				   (lambda ()
				     (let ([o (find-item-editor this)])
				       (enable (and o
						    (send o can-do-edit-operation? op)))))])
				(sequence
				  (super-init
				   name m
				   (lambda (i e)
				     (let* ([o (find-item-editor i)])
				       (and o
					    (send o do-edit-operation op))))
				   key)))))]
	   [mk-sep (lambda () (make-object separator-menu-item% m))])
      (mk "&Undo" #\z 'undo)
      (mk "Redo" #f 'redo)
      (mk-sep)
      (mk "&Copy" #\c 'copy)
      (mk "Cu&t" #\x 'cut)
      (mk "&Paste" #\v 'paste)
      (if (eq? (system-type) 'windows)
	  (mk "Delete" #f 'clear)
	  (mk "Clear" #f 'clear))
      (mk "Select &All" #\a 'select-all)
      (unless text-only?
	(mk-sep)
	(mk "Insert Text Box" #f 'insert-text-box)
	(mk "Insert Pasteboard Box" #f 'insert-pasteboard-box)
	(mk "Insert Image..." #f 'insert-image))
      (void))]))

(define (append-editor-font-menu-items m)
  (menu-parent-only 'append-editor-font-menu-items m)
  (let ([mk (lambda (name m cb)
	      (make-object menu-item% name m
			   (lambda (i e)
			     (let* ([o (find-item-editor i)])
			       (and o (cb o))))))]
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
		'("Standard" "Decorative" "Roman" "Script" "Swiss" "Fixed" "Symbol")
		'(default decorative roman script swiss modern symbol))
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
		  (mk name alignment (mk-cg 'change-alignment s)))
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
   [(eq? (car who) 'iconstructor) (iconstructor-name (cadr who))]
   [else (constructor-name (cadr who))]))

(define (label-string? s)
  (and (string? s) 
       (let ([l (string-length s)])
	 (and l
	      (<= 0 l 200)))))

(define (check-instance who class class-name false-ok? v)
  (unless (or (and false-ok? (not v)) (is-a? v class))
    (raise-type-error (who->name who) (format "~a object~a" class-name (if false-ok? " or #f" "")) v)))

(define (check-string/false who str)
  (unless (or (not str) (string? str))
    (raise-type-error (who->name who) "string or #f" str)))

(define (check-path/false who str)
  (unless (or (not str) (path-string? str))
    (raise-type-error (who->name who) "path, string, or #f" str)))

(define (check-string who str)
  (unless (string? str)
    (raise-type-error (who->name who) "string" str)))

(define (check-label-string who str)
  (unless (label-string? str)
    (raise-type-error (who->name who) "string (up to 200 characters)" str)))

(define (check-label-string/false who str)
  (unless (or (not str) (label-string? str))
    (raise-type-error (who->name who) "string  (up to 200 characters) or #f" str)))

(define (check-char/false who c)
  (unless (or (not c) (char? c))
    (raise-type-error (who->name who) "character or #f" c)))

(define (check-callback who callback)
  (unless (and (procedure? callback)
	       (procedure-arity-includes? callback 2))
    (raise-type-error (who->name who) "procedure of arity 2" callback)))

(define (check-callback1 who callback)
  (unless (and (procedure? callback)
	       (procedure-arity-includes? callback 1))
    (raise-type-error (who->name who) "procedure of arity 1" callback)))

(define (check-bounded-integer min max false-ok?)
  (lambda (who range)
    (unless (or (and false-ok? (not range))
		(and (integer? range) (exact? range) (<= min range max)))
      (raise-type-error (who->name who) 
			(format "exact integer in [~a, ~a]~a"
				min max
				(if false-ok? " or #f" ""))
			range))))

(define check-range-integer (check-bounded-integer 0 10000 #f))

(define check-slider-integer (check-bounded-integer -10000 10000 #f))

(define check-init-pos-integer (check-bounded-integer -10000 10000 #t))

(define check-margin-integer (check-bounded-integer 0 1000 #f))

(define check-gauge-integer (check-bounded-integer 1 10000 #f))

(define (check-wheel-step cwho wheel-step)
  (when (and wheel-step
	     (not (and (integer? wheel-step)
		       (exact? wheel-step)
		       (<= 1 wheel-step 10000))))
    (raise-type-error (who->name cwho)
		      "#f or exact integer in [1,10000]"
		      wheel-step)))

(define (check-fraction who x)
  (unless (and (real? x) (<= 0.0 x 1.0))
    (raise-type-error (who->name who) 
		      "real number in [0.0, 1.0]"
		      x)))

(define (-check-non-negative-integer who i false-ok?)
  (when (or i (not false-ok?))
    (unless (and (integer? i) (exact? i) (not (negative? i)))
      (raise-type-error (who->name who) 
			(if false-ok?
			    "non-negative exact integeror #f" 
			    "non-negative exact integer" )
			i))))

(define (check-non-negative-integer who i)
  (-check-non-negative-integer who i #f))

(define (check-non-negative-integer/false who i)
  (-check-non-negative-integer who i #t))

(define check-dimension (check-bounded-integer 0 10000 #t))
(define check-non#f-dimension (check-bounded-integer 0 10000 #f))

(define (check-label-string-or-bitmap who label)
  (unless (or (label-string? label) (is-a? label wx:bitmap%))
    (raise-type-error (who->name who) "string (up to 200 characters) or bitmap% object" label)))

(define (check-label-string-or-bitmap/false who label)
  (unless (or (not label) (label-string? label) (is-a? label wx:bitmap%))
    (raise-type-error (who->name who) "string (up to 200 characters), bitmap% object, or #f" label)))

(define (check-label-string/bitmap/iconsym who label)
  (unless (or (label-string? label) (is-a? label wx:bitmap%)
	      (memq label '(app caution stop)))
    (raise-type-error (who->name who) "string (up to 200 characters), bitmap% object, or icon symbol" label)))

(define (check-style who reqd other-allowed style)
  (unless (and (list? style) (andmap symbol? style))
    (raise-type-error (who->name who) "list of style symbols" style))
  (when reqd
    (letrec ([or-together (lambda (l)
			    (if (= (length l) 2)
				(format "~a or ~a" (car l) (cadr l))
				(let loop ([l l])
				  (if (null? (cdr l))
				      (format "or ~a" (car l))
				      (format "~a, ~a" (car l) (loop (cdr l)))))))])
      (unless (ormap (lambda (i) (memq i reqd)) style)
	(raise-type-error (who->name who)
			  (format "style list, missing ~a"
				  (if (= (length reqd) 1)
				      (car reqd)
				      (string-append
				       "one of "
				       (or-together reqd))))
			  style))))
  (if (and (not reqd) (null? other-allowed))
      (unless (null? style)
	(raise-type-error (who->name who) "empty style list" style))
      (let* ([l (append (or reqd null) other-allowed)]
	     [bad (ormap (lambda (x) (if (memq x l) #f x)) style)])
	(when bad
	  (raise-type-error (who->name who) (format "style list, ~e not allowed" bad) style))
	(let loop ([l style])
	  (unless (null? l)
	    (when (memq (car l) (cdr l))
	      (raise-type-error (who->name who) (format "style list, ~e allowed only once" (car l)) style))
	    (loop (cdr l)))))))

(define (sleep/yield secs)
  (unless (and (real? secs) (not (negative? secs)))
    (raise-type-error 'sleep/yield "non-negative real number" secs))
  (let ([s (make-semaphore)])
    (thread (lambda () (sleep secs) (semaphore-post s)))
    (wx:yield s)))

(define get-window-text-extent
  (let ([bm #f][dc #f])
    (case-lambda
     [(string font)
      (check-string 'get-window-text-extent string)
      (check-instance 'get-window-text-extent wx:font% 'font% #f font)
      (unless bm
	(set! bm (make-object wx:bitmap% 2 2))
	(set! dc (make-object wx:bitmap-dc%))
	(send dc set-bitmap bm))
      (unless (send bm ok?)
	(error 'get-window-text-extent "couldn't allocate sizing bitmap"))
      (let-values ([(w h d a) (send dc get-text-extent string font)])
	(values (inexact->exact w) (inexact->exact h)))])))

(define x-has-xft? 'unknown)
(define mswin-system #f)
(define mswin-default #f)
(define (look-for-font name)
  (if (ormap (lambda (n) (string-ci=? name n)) (wx:get-face-list))
      name
      "MS San Serif"))

(define (get-family-builtin-face family)
  (unless (memq family '(default decorative roman script swiss modern system symbol))
    (raise-type-error 'get-family-builtin-face "family symbol" family))
  (case (system-type)
    [(unix)
     ;; Detect Xft by looking for a font with a space in front of its name:
     (when (eq? x-has-xft? 'unknown)
       (set! x-has-xft? (ormap (lambda (s) (regexp-match #rx"^ " s)) (wx:get-face-list))))
     (if x-has-xft?
	 (case family
	   [(system) " Sans"]
	   [(default) " Sans"]
	   [(roman) " Serif"]
	   [(decorative) " Nimbus Sans L"]
	   [(modern) " Monospace"]
	   [(swiss) " Nimbus Sans L"]
	   [(script) " URW Chancery L"]
	   [(symbol) " Standard Symbols L,Nimbus Sans L"])
	 (case family
	   [(system) "-b&h-lucida"]
	   [(default) "-b&h-lucida"]
	   [(roman) "-adobe-times"]
	   [(decorative) "-adobe-helvetica"]
	   [(modern) "-adobe-courier"]
	   [(swiss) "-b&h-lucida"]
	   [(script) "-itc-zapfchancery"]
	   [(symbol) "-adobe-symbol"]))]
    [(windows)
     (case family
       [(system) 
	(unless mswin-system 
	  (set! mswin-system (look-for-font "Tahoma")))
	mswin-system]
       [(default) 
	(unless mswin-default 
	  (set! mswin-default (look-for-font "Microsoft Sans Serif")))
	mswin-default]
       [(default) "MS Sans Serif"]
       [(roman) "Times New Roman"]
       [(decorative) "Arial"]
       [(modern) "Courier New"]
       [(swiss) "Arial"]
       [(script) "Arial"]
       [(symbol) "Symbol"])]
    [(macos)
     (case family
       [(system) "systemfont"]
       [(default) "applicationfont"]
       [(roman) "Times"]
       [(decorative) "Geneva"]
       [(modern) "Monaco"]
       [(swiss) "Helvetica"]
       [(script) "Zaph Chancery"]
       [(symbol) "Symbol"])]
    [(macosx)
     (case family
       [(system) "systemfont"]
       [(default) "applicationfont"]
       [(roman) "Times"]
       [(decorative) "Arial"]
       [(modern) "Courier New"]
       [(swiss) "Helvetica"]
       [(script) "Apple Chancery"]
       [(symbol) "Symbol"])]))

(define (send-message-to-window x y m)
  (check-slider-integer 'send-message-to-window x)
  (check-slider-integer 'send-message-to-window y)
  (let ([w (wx:location->window x y)])
    (and w (let ([f (wx->proxy w)])
	     (and f 
		  (not (eq? f root-menu-frame))
		  (send f on-message m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Timers:

(define timer%
  (class wx:timer%
    (init [notify-callback void]
	  [interval #f]
	  [just-once? #f])
    
    (inherit start)
    
    (define -notify-callback notify-callback)
    
    (define/override (notify) (-notify-callback))
    
    (super-make-object)

    (when interval
      (start interval just-once?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize AFM/PS:
(wx:set-ps-procs
 afm-draw-text
 afm-get-text-extent
 afm-expand-name
 afm-glyph-exists?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(wx:set-dialogs get-file put-file get-ps-setup-from-user message-box)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; snip-class% and editor-data-class% loaders

(let ([load-one
       (lambda (str id %)
	 (let ([m (with-handlers ([void (lambda (x) #f)])
		    (and (regexp-match #rx"^[(].*[)]$" str)
			 (read (open-input-string str))))])
	   (if (and (list? m)
		    (eq? 'lib (car m))
		    (andmap string? (cdr m)))
	       (let ([result (dynamic-require m id)])
		 (if (is-a? result %)
		     result
		     (error 'load-class "not a ~a% instance" id)))
	       #f)))])
  ;; install the getters:
  (wx:set-snip-class-getter 
   (lambda (name)
     (load-one name 'snip-class wx:snip-class%)))
  (wx:set-editor-data-class-getter 
   (lambda (name)
     (load-one name 'editor-data-class wx:editor-data-class%))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define readable-snip<%>
  (interface ()
    read-one-special))

(define empty-string (make-bytes 0))
      
;; open-input-text-editor : (instanceof text%) num num -> input-port
;; creates a user port whose input is taken from the text%,
;; starting at position `start-in'
;; and ending at position `end'.
(define open-input-text-editor 
  (case-lambda
   [(text start end snip-filter port-name)
    ;; Check arguments:
    (unless (text . is-a? . text%)
      (raise-type-error 'open-input-text-editor "text% object" text))
    (check-non-negative-integer 'open-input-text-editor start)
    (unless (or (eq? end 'end)
		(and (integer? end) (exact? end) (not (negative? end))))
      (raise-type-error 'open-input-text-editor "non-negative exact integer or 'end" end))
    (let ([last (send text last-position)])
      (when (start . > . last)
	(raise-mismatch-error 'open-input-text-editor
			      (format "start index outside the range [0,~a]: " last)
			      start))
      (unless (eq? end 'end)
	(unless (<= start end last)
	  (raise-mismatch-error 'open-input-text-editor
				(format "end index outside the range [~a,~a]: " start last)
				end))))
    (let ([end (if (eq? end 'end) (send text last-position) end)]
	  [snip (send text find-snip start 'after-or-none)])
      (if (and (is-a? snip wx:string-snip%)
	       (let ([s (send text find-next-non-string-snip snip)])
		 (or (not s)
		     ((send text get-snip-position s) . >= . end))))
	  ;; It's all text --- just read it into a string
	  (let ([port (open-input-string (send text get-text start end))])
	    (port-count-lines! port)
	    port)
	  ;; Create the port:
	  (with-method ([gsp (text get-snip-position)])
	    (let-values ([(pipe-r pipe-w) (make-pipe)])
	      (let* ([get-text-generic (generic wx:snip% get-text)]
		     [get-count-generic (generic wx:snip% get-count)]
		     [next-generic (generic wx:snip% next)]
		     [next? #f]
		     [pos 0]
		     [lock-semaphore (make-semaphore 1)]
		     [update-str-to-snip
		      (lambda (to-str)
			(if snip
			    (let ([snip-start (gsp snip)])
			      (cond
			       [(snip-start . >= . end)
				(set! snip #f)
				(set! next? #f)
				0]
			       [(is-a? snip wx:string-snip%)
				(set! next? #t)
				(let ([c (min (send-generic snip get-count-generic) (- end snip-start))])
				  (write-string (send-generic snip get-text-generic 0 c) pipe-w)
				  (read-bytes-avail!* to-str pipe-r))]
			       [else
				(set! next? #f)
				0]))
			    (begin
			      (set! next? #f)
			      0)))]
		     [next-snip
		      (lambda (to-str)
			(set! snip (send-generic snip next-generic))
			(set! pos 0)
			(update-str-to-snip to-str))]
		     [read-chars (lambda (to-str)
				   (cond
				    [next?
				     (next-snip to-str)]
				    [snip
				     (let-values ([(the-snip alt-size) (snip-filter snip)])
				       (cons
					alt-size
					(lambda (file line col ppos)
					  (if (is-a? the-snip wx:snip%)
					      (if (is-a? the-snip readable-snip<%>)
						  (with-handlers ([special-comment?
								   (lambda (exn)
								     ;; implies "done"
								     (next-snip empty-string)
								     (raise exn))]
								  [void
								   (lambda (exn)
								     ;; Give up after an exception
								     (next-snip empty-string)
								     (raise exn))])
						    (let-values ([(val size done?)
								  (send the-snip read-one-special pos file line col ppos)])
						      (if done?
							  (next-snip empty-string)
							  (set! pos (add1 pos)))
						      val))
						  (begin
						    (next-snip empty-string)
						    (send the-snip copy)))
					      (begin
						(next-snip empty-string)
						the-snip)))))]
				    [else eof]))]
		     [close (lambda () (void))]
		     [port (make-input-port/read-to-peek
			    port-name
			    (lambda (s)
			      (if (char-ready? pipe-r)
				  (read-bytes-avail!* s pipe-r)
				  (if (semaphore-try-wait? lock-semaphore)
				      ;; If there's an error here, the
				      ;; port will remain locked.
				      (let ([v (read-chars s)])
					(semaphore-post lock-semaphore)
					v)
				      (convert-evt
				       (semaphore-peek-evt lock-semaphore)
				       (lambda (x) 0)))))
			    (lambda (s skip general-peek)
			      (let ([v (peek-bytes-avail!* s skip pipe-r)])
				(if (zero? v)
				    (general-peek s skip)
				    v)))
			    close)])
		(if (is-a? snip wx:string-snip%)
		    ;; Special handling for initial snip string in
		    ;; case it starts too early:
		    (let* ([snip-start (gsp snip)]
			   [skip (- start snip-start)]
			   [c (min (- (send-generic snip get-count-generic) skip)
				   (- end snip-start))])
		      (set! next? #t)
		      (display (send-generic snip get-text-generic skip c) pipe-w))
		    (update-str-to-snip empty-string))
		(port-count-lines! port)
		port)))))]
   [(text start end filter) (open-input-text-editor text start end filter text)]
   [(text start end) (open-input-text-editor text start end (lambda (x) (values x 1)))]
   [(text start) (open-input-text-editor text start 'end)]
   [(text) (open-input-text-editor text 0 'end)]))

(define (text-editor-load-handler filename expected-module)
  (unless (path? filename)
    (raise-type-error 'text-editor-load-handler "path" filename))
  (let-values ([(in-port src) (build-input-port filename)])
    (dynamic-wind
	(lambda () (void))
	(lambda ()
	  (parameterize ([read-accept-compiled #t])
	    (if expected-module
		(with-module-reading-parameterization 
		 (lambda ()
		   (let* ([first (read-syntax src in-port)]
			  [module-ized-exp (check-module-form first expected-module filename)]
			  [second (read in-port)])
		     (unless (eof-object? second)
		       (raise-syntax-error
			'text-editor-load-handler
			(format "expected only a `module' declaration for `~s', but found an extra expression"
				expected-module)
			second))
		     (eval module-ized-exp))))
		(let loop ([last-time-values (list (void))])
		  (let ([exp (read-syntax src in-port)])
		    (if (eof-object? exp)
			(apply values last-time-values)
			(call-with-values (lambda () (eval exp))
			  (lambda x (loop x)))))))))
	(lambda ()
	  (close-input-port in-port)))))


;; build-input-port : string -> (values input any)
;; constructs an input port for the load handler. Also
;; returns a value representing the source of code read from the file.
;; if the file's first lines begins with #!, skips the first chars of the file.
(define (build-input-port filename)
  (let ([p (open-input-file filename)])
    (port-count-lines! p)
    (let ([p (cond
	      [(regexp-match-peek #rx#"^WXME01[0-9][0-9] ## " p)
	       (let ([t (make-object text%)])
		 (send t insert-file p 'standard)
		 (close-input-port p)
		 (open-input-text-editor t))]
	      [else p])])
      (port-count-lines! p) ; in case it's new
      (let loop ()
	;; Wrap regexp check with `with-handlers' in case the file
	;;  starts with non-text input
	(when (with-handlers ([exn:fail? (lambda (x) #f)])
		(regexp-match-peek #rx"^#!" p))
	  ;; Throw away chars/specials up to eol,
	  ;;  and continue if line ends in backslash
	  (let lloop ([prev #f])
	    (let ([c (read-char-or-special p)])
	      (if (or (eof-object? c)
		      (eq? c #\return)
		      (eq? c #\newline))
		  (when (eq? prev #\\)
		    (loop))
		  (lloop c))))))
      (values p filename))))

(define (open-input-graphical-file filename)
  (let-values ([(p name) (build-input-port filename)])
    p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-namespace-with-mred
  (opt-lambda ([flag 'mred])
    (unless (memq flag '(initial mred empty))
      (raise-type-error 'make-namespace-with-mred
			"flag symbol, one of 'mred, 'initial, or 'empty"
			flag))
    (let ([orig (current-namespace)]
	  [mred-name ((current-module-name-resolver)
		      '(lib "mred.ss" "mred") #f #f)]
	  [ns (make-namespace (if (eq? flag 'empty) 'empty 'initial))])
      (parameterize ([current-namespace ns])
	(namespace-attach-module orig mred-name)
	(when (eq? flag 'mred)
	  (namespace-require '(lib "mred.ss" "mred"))
	  (namespace-require '(lib "class.ss"))))
      ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax propagate
  (lambda (stx)
    (syntax-case stx ()
      [(_ n ...)
       (let ([ns (syntax->list (syntax (n ...)))])
	 (with-syntax ([(k:n ...)
			(map
			 (lambda (n)
			   (datum->syntax-object
			    n
			    (string->symbol
			     (format 
			      "wx:~a"
			      (syntax-e n)))
			    #f))
			 ns)])
	   (syntax (begin
		     (provide (rename k:n n) ...)))))])))

(propagate add-color<%>
	   add-editor-keymap-functions
	   add-text-keymap-functions
	   add-pasteboard-keymap-functions
	   begin-busy-cursor
	   bell
	   bitmap%
	   brush%
	   brush-list%
	   editor-data%
	   editor-data-class%
	   editor-data-class-list<%>
	   check-for-break
	   clipboard<%>
	   clipboard-client%
	   color%
	   color-database<%>
	   control-event%
	   current-eventspace
	   current-ps-setup
	   cursor%
	   dc<%>
	   get-display-depth
	   end-busy-cursor
	   event%
	   event-dispatch-handler
	   eventspace?
	   find-graphical-system-path
	   flush-display
	   font%
	   font-list%
	   font-name-directory<%>
	   get-face-list
	   get-resource
	   get-the-editor-data-class-list
	   get-the-snip-class-list
	   image-snip%
	   is-busy?
	   is-color-display?
	   key-event%
	   keymap%
	   make-eventspace
	   editor-admin%
	   editor-set-x-selection-mode
	   editor-snip-editor-admin<%>
	   editor-stream-in%
	   editor-stream-in-base%
	   editor-stream-in-bytes-base%
	   editor-stream-out%
	   editor-stream-out-base%
	   editor-stream-out-bytes-base%
	   editor-wordbreak-map%
	   mouse-event%
	   mult-color<%>
	   pen%
	   pen-list%
	   point%
	   ps-setup%
	   read-editor-global-footer
	   read-editor-global-header
	   read-editor-version
	   region%
	   scroll-event%
	   snip%
	   snip-admin%
	   snip-class%
	   snip-class-list<%>
	   special-control-key
	   special-option-key
	   label->plain-label
	   string-snip%
	   style<%>
	   style-delta%
	   style-list%
	   tab-snip%
	   write-editor-global-footer
	   write-editor-global-header
	   write-editor-version
	   write-resource
	   queue-callback
	   yield
	   eventspace-shutdown?
	   get-panel-background
	   send-event
	   gl-context<%>)

(define the-color-database (wx:get-the-color-database))
(define the-font-name-directory (wx:get-the-font-name-directory))
(define the-clipboard (wx:get-the-clipboard))
(define the-font-list (wx:get-the-font-list))
(define the-pen-list (wx:get-the-pen-list))
(define the-brush-list (wx:get-the-brush-list))
(define the-style-list (wx:get-the-style-list))
(define the-editor-wordbreak-map (wx:get-the-editor-wordbreak-map))

(define file-creator-and-type
  (case-lambda
   [(fn) (wx:file-creator-and-type fn)]
   [(fn c t) (wx:file-creator-and-type fn c t)]))

(provide button%
	canvas%
	check-box%
	choice%
	dialog%
	frame%
	gauge%
	tab-panel%
	group-box-panel%
	list-box%
	editor-canvas%
	message%
	pane%
	horizontal-pane%
	vertical-pane%
	grow-box-spacer-pane%
	panel%
	horizontal-panel%
	vertical-panel%
	radio-box%
	slider%
	text-field%
	window<%>
	area<%>
	top-level-window<%>
	subarea<%>
	subwindow<%>
	area-container<%>
	area-container-window<%>
	canvas<%>
	control<%>
	list-control<%>
	menu-item<%>
	separator-menu-item%
	selectable-menu-item<%>
	labelled-menu-item<%>
	menu-item%
	checkable-menu-item%
	menu-item-container<%>
	menu%
	menu-bar%
	popup-menu%
	get-top-level-windows
	editor-snip%
	editor<%>
	text%
	pasteboard%
	graphical-read-eval-print-loop
	message-box
	message-box/custom
	get-file
	get-file-list
	put-file
	get-directory
	get-choices-from-user
	get-text-from-user
	get-ps-setup-from-user
	play-sound
	get-display-size
	get-display-left-top-inset
	get-color-from-user
	get-font-from-user
	append-editor-operation-menu-items
	append-editor-font-menu-items
	get-top-level-focus-window
	get-top-level-edit-target-window
	register-collecting-blit
	unregister-collecting-blit
	bitmap-dc%
	post-script-dc%
	printer-dc%
	current-text-keymap-initializer
	sleep/yield
	get-window-text-extent
	get-family-builtin-face
	send-message-to-window
	the-clipboard
	the-editor-wordbreak-map
	the-brush-list
	the-color-database
	the-font-name-directory
	the-pen-list
	the-font-list
	the-style-list
	timer%
	readable-snip<%>
	open-input-text-editor
	open-input-graphical-file
	text-editor-load-handler
	application-about-handler
	application-preferences-handler
	application-quit-handler
	application-file-handler
	current-eventspace-has-standard-menus?
	current-eventspace-has-menu-root?
	eventspace-handler-thread
	make-namespace-with-mred
	file-creator-and-type
	current-ps-afm-file-paths
	current-ps-cmap-file-paths)


) ;; end of module

