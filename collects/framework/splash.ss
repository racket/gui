; filename : splash-image-path
; title : title of window
; width-default : number 

(module splash mzscheme
  (require (lib "class100.ss")
           (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (provide splash)

  (define (splash filename title width-default)
    (let/ec k
      (letrec-values
       ([(splash-eventspace) (make-eventspace)]
        [(no-splash) (lambda () (k #f #f splash-eventspace void void void))]
	[(funny?) (let ([date (seconds->date (current-seconds))])
		    (and (= (date-day date) 25)
			 (= (date-month date) 12)))]

        [(funny-bitmap)
         (make-object bitmap%
           (build-path (collection-path "icons") "touch.bmp"))]
        [(funny-value) 0]
	[(funny-gauge%)
	 (class100 canvas% (_max-value parent)
	   (inherit get-dc min-width min-height stretchable-width stretchable-height)
           (private-field
             [max-value _max-value])
	   (public
	     [set-value
	      (lambda (new-value)
		(let ([before-x
		       (floor (* (send funny-bitmap get-width) (/ funny-value max-value)))]
		      [after-x
		       (ceiling (* (send funny-bitmap get-width)
                                   (/ (- new-value funny-value)
                                      max-value)))])
		  (send (get-dc) draw-line
			(+ before-x 2) 0
			(+ after-x 2) 0)
		  (send (get-dc) draw-line
			(+ before-x 2) (+ (send funny-bitmap get-height) 4)
			(+ after-x 2) (+ (send funny-bitmap get-height) 4))
		  (send (get-dc) draw-bitmap-section bitmap
			(+ 2 before-x) 2
			before-x 0
			after-x (send funny-bitmap get-height)))
		(set! funny-value new-value))])
	   (override
	    [on-paint
	     (lambda ()
	       (let ([dc (get-dc)])
		 (send dc clear)
		 (send dc draw-rectangle 0 0
		       (+ (send funny-bitmap get-width) 4)
		       (+ (send funny-bitmap get-height) 4))
		 (send dc draw-bitmap-section funny-bitmap
		       2 2 0 0
		       (* (send funny-bitmap get-width) (/ funny-value max-value))
		       (send funny-bitmap get-height))))])
	   (sequence
	     (super-init parent)
	     (min-width (+ (send funny-bitmap get-width) 4))
	     (min-height (+ (send funny-bitmap get-height) 4))
	     (stretchable-width #f)
	     (stretchable-height #f)))]

	[(splash-get-resource)
	 (lambda (name default)
	   (let ([b (box 0)])
	     (if (get-resource "mred" name b #f)
		 (unbox b)
		 default)))]
	[(set-resource)
	 (lambda (name value)
	   (write-resource "mred" name value (find-graphical-system-path 'setup-file)))]
	[(_1)
	 (begin
	   (unless filename
	     (no-splash))
	   (unless (file-exists? filename)
	     (fprintf (current-error-port) "WARNING: bitmap path ~s not found~n" filename)
	     (no-splash)))]

	[(splash-width-resource) (format "~a-splash-max-width" title)]
	[(splash-max-width) (max 1 (splash-get-resource splash-width-resource width-default))]
	
	[(splash-current-width) 0]

	[(splitup-path)
	 (lambda (f)
	   (let*-values ([(absf) (if (relative-path? f)
				     (build-path (current-directory) f)
				     f)]
			 [(base name _1) (split-path absf)])

	     (if base
		 (let-values ([(base2 name2 _2) (split-path base)])
		   (if base2
		       (let-values ([(base3 name3 _2) (split-path base2)])
			 (build-path name3 name2 name))
		       (build-path name2 name)))
		 name)))]

	[(quit-on-close?) #t]
	[(dropped-files) null]
	[(get-dropped-files) (lambda () dropped-files)]
	[(splash-frame%)
	 (class100 frame% (title)
	   (override
	    [on-drop-file
	     (lambda (filename)
	       (set! dropped-files (cons filename dropped-files)))]
	    [on-close
	     (lambda ()
	       (when quit-on-close?
		 (exit)))])
	   (sequence (super-init title)))]
	[(frame) (parameterize ([current-eventspace splash-eventspace])
		   (make-object splash-frame% title))]
	[(_0) (send frame accept-drop-files #t)]
	[(bitmap-flag)
	 (let ([len (string-length filename)])
	   (if (<= len 4)
	       'guess
	       (let ([suffix (substring filename (- len 4) len)])
		 (cond
		  [(string-ci=? ".xpm" suffix) 'xpm]
		  [(string-ci=? ".xbm" suffix) 'xbm]
		  [(string-ci=? ".gif" suffix) 'gif]
		  [(string-ci=? "pict" suffix) 'pict]
		  [else 'xpm]))))]
	[(bitmap) (make-object bitmap% filename bitmap-flag)]
	[(_2) (unless (send bitmap ok?)
		(fprintf (current-error-port) "WARNING: bad bitmap ~s~n" filename)
		(no-splash))]
	[(splash-canvas%)
	 (class100 canvas% args
	   (inherit get-dc)
	   (override
	    [on-paint
	     (lambda ()
	       (send (get-dc) draw-bitmap bitmap 0 0))])
	   (sequence
	     (apply super-init args)))]
	[(panel) (make-object vertical-pane% frame)]
	[(logo-canvas) (make-object splash-canvas% panel)]
	[(h-panel) (make-object horizontal-pane% panel)]
	[(gauge)
	 (if funny?
	     (make-object funny-gauge% splash-max-width h-panel)
	     (make-object gauge% #f splash-max-width h-panel '(horizontal)))]
	[(spacer) (make-object grow-box-spacer-pane% h-panel)]
	[(_3) (begin
		(send frame set-alignment 'center 'center)
		(send panel stretchable-width #f)
		(send panel stretchable-height #f)
		(send h-panel set-alignment 'center 'top)
		(send logo-canvas min-width (send bitmap get-width))
		(send logo-canvas min-height (send bitmap get-height))
		(send logo-canvas stretchable-width #f)
		(send logo-canvas stretchable-height #f)
		(send frame center 'both)
		(send frame show #t)
		(flush-display) (yield) (sleep)
		(flush-display) (yield) (sleep))]
	[(inc-splash)
	 (lambda ()
	   (set! splash-current-width (+ splash-current-width 1))
	   (when (<= splash-current-width splash-max-width)
	     (send gauge set-value splash-current-width)))]
	[(splash-load-handler)
	 (lambda (old-load f)
	   (let ([finalf (splitup-path f)])
	     (inc-splash)
	     (old-load f)))]
	[(_4) (current-load
	       (let ([old-load (current-load)])
		 (lambda (f)
		   (splash-load-handler old-load f))))]
	[(shutdown-splash)
	 (lambda ()
	   (set! splash-load-handler (lambda (old-load f) (old-load f))))]
	[(close-splash)
	 (lambda ()
	   (inc-splash)
	   (unless (= splash-max-width splash-current-width)
	     (set-resource splash-width-resource (max 1 splash-current-width)))
	   (set! quit-on-close? #f)
	   (send frame show #f))])
       (values
        bitmap
        logo-canvas
        splash-eventspace
	get-dropped-files
	shutdown-splash
	close-splash)))))
