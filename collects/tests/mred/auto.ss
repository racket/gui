
(define errs null)
(define test-count 0)

(define (test expect name got)
  (set! test-count (add1 test-count))
  (unless (equal? expect got)
    (let ([s (format "~a: expected ~a; got ~a" name expect got)])
      (printf "ERROR: ~a~n" s)
      (set! errs (cons s errs)))))


(define-macro st
  (lambda (val obj method . args)
    `(test ,val ',method (send ,obj ,method ,@args))))

(define-macro stv
  (lambda args
    `(st (void) ,@args)))

(define-macro stvals
  (lambda (vals obj method . args)
    `(test ,vals ',method (call-with-values (lambda () (send ,obj ,method ,@args)) list))))

(define-macro FAILS void)

(define (pause)
  (let ([s (make-semaphore)])
    (flush-display)
    (thread (lambda () (sleep 0.01) (semaphore-post s)))
    (yield s)))

(define (enable-tests f)
  (printf "Enable ~a~n" f)
  (st #t f is-enabled?)
  (stv f enable #f)
  (st #f f is-enabled?)
  (stv f enable #t)
  (st #t f is-enabled?))

(define (drop-file-tests f)
  (printf "Drop File ~a~n" f)
  (st #f f accept-drop-files)
  (stv f accept-drop-files #t)
  (st #t f accept-drop-files)
  (stv f accept-drop-files #f)
  (st #f f accept-drop-files))

(define (client->screen-tests f)
  (printf "Client<->Screen ~a~n" f)
  (let-values ([(x y) (send f client->screen 0 0)])
    (stvals '(0 0) f screen->client x y))
  (let-values ([(x y) (send f screen->client 0 0)])
    (stvals '(0 0) f client->screen x y))
  (let-values ([(cw ch) (send f get-client-size)]
	       [(w h) (send f get-size)])
    (test #t 'client-size (and (<= 1 cw w) (<= 1 ch h))))
  (stv f refresh))

(define (area-tests f sw? sh?)
  (printf "Area ~a~n" f)
  (let ([x (send f min-width)]
	[y (send f min-height)])
    (st sw? f stretchable-width)
    (st sh? f stretchable-height)
    (pause) ; to make sure size has taken effect
    (let-values ([(w h) (send f get-size)])
      (printf "Size ~a x ~a~n" w h)
      (stv f min-width w) ; when we turn of stretchability, don't resize
      (stv f min-height h)
      (stv f stretchable-width #f)
      (stv f stretchable-height #f)
      (st #f f stretchable-width)
      (st #f f stretchable-height)
      (stv f stretchable-width #t)
      (stv f stretchable-height #t)
      (st #t f stretchable-width)
      (st #t f stretchable-height)
      (stv f stretchable-width sw?)
      (stv f stretchable-height sh?))
    (stv f min-width x)
    (stv f min-height y)))

(define (containee-tests f sw? sh? m)
  (area-tests f sw? sh?)
  (printf "Containee ~a~n" f)
  (st m f horiz-margin)
  (st m f vert-margin)
  (stv f horiz-margin 3)
  (st 3 f horiz-margin)
  (st m f vert-margin)
  (stv f horiz-margin m)
  (stv f vert-margin 3)
  (st m f horiz-margin)
  (st 3 f vert-margin)
  (stv f vert-margin m))

(define (container-tests f)
  (printf "Container ~a~n" f)
  (let-values ([(x y) (send f get-alignment)])
    (stv f set-alignment 'right 'bottom)
    (stvals '(right bottom) f get-alignment)
    (stv f set-alignment x y))
  (test #t 'get-label-font-kind (is-a? (send f get-label-font) font%))
  (test #t 'get-label-font-kind (is-a? (send f get-control-font) font%))
  (st (send f get-label-font) f get-control-font)
  (let ([fnt (send f get-label-font)]
	[other-font (make-object font% 20 'decorative 'normal 'bold)])
    (st 'system fnt get-family)
    (st 'normal fnt get-style)
    (st 'normal fnt get-weight)
    (stv f set-label-font other-font)
    (st other-font f get-label-font)
    (stv f set-label-font fnt)
    (stv f set-control-font other-font)
    (st other-font f get-control-font)
    (stv f set-control-font fnt))
  (st 'horizontal f get-label-position)
  (stv f set-label-position 'vertical)
  (st 'vertical f get-label-position)
  (stv f set-label-position 'horizontal))

(define (cursor-tests f)
  (printf "Cursor ~a~n" f)
  (let ([c (send f get-cursor)])
    (stv f set-cursor c)
    (st c f get-cursor)
    (begin-busy-cursor)
    (end-busy-cursor)
    (st c f get-cursor)
    (stv f set-cursor #f)
    (st #f f get-cursor)
    (begin-busy-cursor)
    (end-busy-cursor)
    (st #f f get-cursor)
    (stv f set-cursor c)))

(define (window-tests f sw? sh? parent top m)
  (st parent f get-parent)
  (st top f get-top-level-window)
  (enable-tests f)
  (drop-file-tests f)
  (client->screen-tests f)
  (containee-tests f sw? sh? m)
  (cursor-tests f))

(define (test-control-event e types)
  (test #t 'event-instance (is-a? e control-event%)) 
  (test #t 'event-type (pair? (memq (send e get-event-type) types))))

(define (label-test b l)
  (let ([&-l (format "&~a" l)]
	[my-l (format "My ~a" l)]
	[&-my-l (format "&My ~a" l)])
    (st &-l b get-label)
    (st l b get-plain-label)
    (stv b set-label &-my-l)
    (st &-my-l b get-label)
    (st my-l b get-plain-label)
    (stv b set-label &-l)))
			      
(let ([f (make-object frame% "Yes & No" #f 150 151 20 21)])
  (let ([init-tests
	 (lambda ()
	   (st "Yes & No" f get-label)
	   (st "Yes  No" f get-plain-label)
	   (stv f set-label "Yeah & Nay")
	   (st "Yeah & Nay" f get-label)
	   (st "Yeah  Nay" f get-plain-label)
	   (stv f set-label "Yes & No")
	   (st #f f get-parent)
	   (st f f get-top-level-window)
	   (st 20 f get-x)
	   (st 21 f get-y)
	   (st 150 f get-width)
	   (st 151 f get-height)
	   (stvals (list (send f get-width) (send f get-height)) f get-size)
	   (st #f f has-status-line?)
	   (st #f f is-iconized?)
	   (st #f f get-menu-bar))]
	[space-tests
	 (lambda ()
	   (printf "Spacing~n")
	   (let ([b (send f border)])
	     (stv f border 25)
	     (st 25 f border)
	     (stv f border b))
	   (let ([s (send f spacing)])
	     (stv f spacing 7)
	     (st 7 f spacing)
	     (stv f spacing s)))]
	[enable-tests
	 (lambda () (enable-tests f))]
	[drop-file-tests
	 (lambda ()
	   (drop-file-tests f))]
	[client->screen-tests
	 (lambda ()
	   (printf "Client<->Screen~n")
	   (let-values ([(x y) (send f client->screen 0 0)])
	     (stvals '(0 0) f screen->client x y))
	   (let-values ([(x y) (send f screen->client 0 0)])
	     (stvals '(0 0) f client->screen x y)))]
	[container-tests
	 (lambda ()
	   (printf "Container~n")
	   (area-tests f #t #t)
	   (let-values ([(x y) (send f container-size null)])
	     (st x f min-width)
	     (st y f min-height))
	   (container-tests f))]
	[cursor-tests
	 (lambda ()
	   (test #t 'get-cursor-kind (is-a? (send f get-cursor) cursor%))
	   (cursor-tests f))])

    (st (current-eventspace) f get-eventspace)
    (st #t f can-close?)
    (st #t f can-exit?)
    (stv f focus)

    (space-tests)
    (enable-tests)
    (client->screen-tests)
    (container-tests)
    (cursor-tests)

    (printf "Init~n")
    (init-tests)
    (stv f show #t)
    (pause)
    (printf "Show Init~n")
    (init-tests)
    (stv f show #f)
    (pause)
    (printf "Hide Init~n")
    (init-tests)
    (send f show #t)
    (pause)

    (space-tests)
    (enable-tests)
    (client->screen-tests)
    (container-tests)

    (stv f change-children values)
    
    (printf "Iconize~n")
    (stv f iconize #t)
    (pause)
    (pause)
    (st #t f is-iconized?)
    (stv f show #t)
    (pause)
    (st #f f is-iconized?)
    
    (stv f maximize #t)
    (pause)
    (stv f maximize #f)
    (pause)

    (printf "Move~n")
    (stv f move 34 37)
    (pause)
    (FAILS (st 34 f get-x))
    (FAILS (st 37 f get-y))
    (st 150 f get-width)
    (st 151 f get-height)

    (printf "Resize~n")
    (stv f resize 56 57)
    (pause)
    (FAILS (st 34 f get-x))
    (FAILS (st 37 f get-y))
    (st 56 f get-width)
    (st 57 f get-height)

    (stv f center)
    (pause)
    (st 56 f get-width)
    (st 57 f get-height)

    (client->screen-tests)

    (stv f create-status-line)
    (stv f set-status-text "Hello")

    (stv f change-children values)
    (st null f get-children)
    (stvals '(center top) f get-alignment)

    (stv f focus)

    (cursor-tests)

    (printf "Menu Bar~n")
    (let ([mb (make-object menu-bar% f)])
      (st mb f get-menu-bar)
      (st f mb get-frame)
      (st null f get-children)

      (st #t mb is-enabled?)
      (stv mb enable #f)
      (st #f mb is-enabled?)
      (stv mb enable #t)
      (st #t mb is-enabled?)
      
      (st null mb get-items)

      (printf "Menu 1~n")
      (let* ([m (make-object menu% "&File" mb)]
	     [i (send m get-item)]
	     [delete-enable-test (lambda (i parent empty)
				   (printf "Item~n")
				   (st #f i is-deleted?)
				   (st #t i is-enabled?)
				   
				   (stv i delete)
				   (st #t i is-deleted?)
				   (st empty parent get-items)
				   (stv i restore)
				   (st #f i is-deleted?)
				   
				   (stv i enable #f)
				   (st #f i is-enabled?)
				   (stv i enable #t)
				   (st #t i is-enabled?)

				   (stv i delete)
				   (st #t i is-enabled?)
				   (stv i enable #f)
				   (st #f i is-enabled?)
				   (stv i restore)
				   (st #f i is-deleted?)
				   (st #f i is-enabled?)
				   (stv i enable #t)

				   (let ([l (send i get-help-string)])
				     (stv i set-help-string "Yikes")
				     (st "Yikes" i get-help-string)
				     (stv i set-help-string #f)
				     (st #f i get-help-string)
				     (stv i set-help-string l))

				   (let ([l (send i get-label)])
				     (stv i set-label "Matthew")
				     (st "Matthew" i get-label)
				     (stv i set-label l)))]
	     [hit #f])
	(st (list i) mb get-items)
	(st m i get-menu)
	(st mb i get-parent)

	(st "&File" i get-label)
	(st "File" i get-plain-label)
	(st #f i get-help-string)

	(delete-enable-test i mb null)

	(st null m get-items)

	(printf "Menu Items~n")
	(let ([i1 (make-object menu-item% "&Plain" m 
			       (lambda (i e)
				 (test-control-event e '(menu))
				 (test hit 'expected-plain-menu i)
				 (set! hit 'plain)
				 'oops)
			       #f "Help")]
	      [i2 (make-object separator-menu-item% m)]
	      [i3 (make-object checkable-menu-item% "Che&ckable" m 
			       (lambda (i e)
				 (test-control-event e '(menu))
				 (test hit 'expected-check-menu i)
				 (set! hit 'check)
				 'oops)
			       #\C)]
	      [shortcut-test
	       (lambda (i empty name)
		 (delete-enable-test i m empty)
		 
		 (printf "Shortcut~n")
		 (set! hit i)
		 (stv i command (make-object control-event% 'menu))
		 (test name 'hit-command hit)
		 
		 (let ([c (send i get-shortcut)])
		   (stv i set-shortcut #\M)
		   (st #\M i get-shortcut)
		   (stv i set-shortcut #f)
		   (st #f i get-shortcut)
		   (stv i set-shortcut c))
		 
		 (st 'meta i get-x-shortcut-prefix)
		 (let ([p (send i get-x-shortcut-prefix)])
		   (stv i set-x-shortcut-prefix 'alt)
		   (st 'alt i get-x-shortcut-prefix)
		   (stv i set-x-shortcut-prefix 'ctl)
		   (st 'ctl i get-x-shortcut-prefix)
		   (stv i set-x-shortcut-prefix 'ctl-m)
		   (st 'ctl-m i get-x-shortcut-prefix)
		   (stv i set-x-shortcut-prefix 'alt)))])
	  (st (list i1 i2 i3) m get-items)
	  
	  (st "&Plain" i1 get-label)
	  (st "Plain" i1 get-plain-label)
	  (st "Help" i1 get-help-string)
	  (st #f i1 get-shortcut)

	  (st "Che&ckable" i3 get-label)
	  (st "Checkable" i3 get-plain-label)
	  (st #f i3 get-help-string)
	  (st #\C i3 get-shortcut)

	  (shortcut-test i1 (list i2 i3) 'plain)
	  (shortcut-test i3 (list i2 i1) 'check)

	  (st (list i2 i1 i3) m get-items)
	  (stv i2 delete)
	  (st #t i2 is-deleted?)
	  (st (list i1 i3) m get-items)
	  (stv i2 restore)
	  (st #f i2 is-deleted?)
	  (st (list i1 i3 i2) m get-items)

	  'done)

	(printf "Menu 2~n")
	(let* ([m2 (make-object menu% "&Edit" mb "Help Edit")]
	       [i2 (send m2 get-item)])
	  (st (list i i2) mb get-items)
	  (st m2 i2 get-menu)
	  (st mb i2 get-parent)

	  (st "&Edit" i2 get-label)
	  (st "Edit" i2 get-plain-label)
	  (st "Help Edit" i2 get-help-string)

	  (delete-enable-test i2 mb (list i))

	  (st null m2 get-items)

	  ; Move orig to end
	  (stv i delete)
	  (stv i restore)
	  (st (list i2 i) mb get-items)))

      'done)))

(define frame (let ([l (get-top-level-windows)])
		(test 1 'list-size (length l))
		(car l)))
(st "Yes & No" frame get-label)

(define (test-controls parent frame)
  (define side-effect #f)
  
  (printf "Buttons~n")
  (letrec ([b (make-object button% 
			   "&Button"
			   parent
			   (lambda (bt e)
			     (test bt 'same-button b)
			     (test-control-event e '(button))
			     (set! side-effect 'button)
			     'oops)
			   '(border))])
    (label-test b "Button")    
    (stv b command (make-object control-event% 'button))
    (test 'button 'button-callback side-effect)
    
    (window-tests b #f #f parent frame 2))

  (printf "Check Box~n")
  (letrec ([c (make-object check-box% 
			   "&Check Box"
			   parent
			   (lambda (cb e)
			     (test cb 'same-check c)
			     (test-control-event e '(check-box))
			     (set! side-effect 'check-box)
			     'oops)
			   null)])
    (label-test c "Check Box")
    (stv c command (make-object control-event% 'check-box))
    (test 'check-box 'check-box-callback side-effect)
    
    (st #f c get-value)
    (stv c set-value #t)
    (st #t c get-value)
    (stv c set-value #f)
    (st #f c get-value)
    
    (window-tests c #f #f parent frame 2))

  (printf "Radio Box~n")
  (letrec ([r (make-object radio-box%
			   "&Radio Box"
			   (list "O&ne" "T&wo" "T&hree")
			   parent
			   (lambda (rb e)
			     (test rb 'same-radio r)
			     (test-control-event e '(radio-box))
			     (set! side-effect 'radio-box)
			     'oops)
			   '(vertical))])
    (label-test r "Radio Box")
    (stv r command (make-object control-event% 'radio-box))
    (test 'radio-box 'radio-box-callback side-effect)

    ; Try every combination of enable states:
    (let ([try-all
	   (lambda ()
	     (let loop ([n 7])
	       (let ([0? (positive? (bitwise-and n 1))]
		     [1? (positive? (bitwise-and n 2))]
		     [2? (positive? (bitwise-and n 4))])
		 (st 0? r is-enabled? 0)
		 (st 1? r is-enabled? 1)
		 (st 2? r is-enabled? 2)
		 (let ([0? (positive? (bitwise-and (sub1 n) 1))]
		       [1? (positive? (bitwise-and (sub1 n) 2))]
		       [2? (positive? (bitwise-and (sub1 n) 4))])
		   (stv r enable 0 0?)
		   (stv r enable 1 1?)
		   (stv r enable 2 2?)
		   (unless (zero? n)
		     (loop (sub1 n))))))
	     (st #t r is-enabled? 0)
	     (st #t r is-enabled? 1)
	     (st #t r is-enabled? 2))])
      (try-all)
      (stv r enable #f)
      (try-all)
      (stv r enable #t))

    (st "O&ne" r get-item-label 0)
    (st "T&wo" r get-item-label 1)
    (st "T&hree" r get-item-label 2)
    (st "One" r get-item-plain-label 0)
    (st "Two" r get-item-plain-label 1)
    (st "Three" r get-item-plain-label 2)

    (st 3 r get-number)

    (st 0 r get-selection)
    (stv r set-selection 1)
    (st 1 r get-selection)
    (stv r set-selection 2)
    (st 2 r get-selection)
    (stv r set-selection 1)
    (st 1 r get-selection)
    (stv r set-selection 0)
    (st 0 r get-selection)
    
    (window-tests r #f #f parent frame 2))

  (printf "Gauge~n")
  (letrec ([g (make-object gauge% 
			   "&Gauge"
			   10
			   parent
			   '(horizontal))])
    (label-test g "Gauge")
    
    (st 0 g get-value)
    (stv g set-value 8)
    (st 8 g get-value)
    (stv g set-value 0)
    (st 0 g get-value)
    (stv g set-value 10)
    (st 10 g get-value)

    (st 10 g get-range)
    (stv g set-range 11)
    (st 11 g get-range)
    (st 10 g get-value)
    (stv g set-range 8)
    (st 8 g get-range)
    (st 8 g get-value)
    (stv g set-range 1)
    (st 1 g get-range)
    (st 1 g get-value)
    (stv g set-range 10)
    (st 10 g get-range)
    (st 1 g get-value)
    
    (window-tests g #t #f parent frame 2))

  (printf "Slider~n")
  (letrec ([s (make-object slider% 
			   "&Slider"
			   -2 8
			   parent
			   (lambda (sl e)
			     (test sl 'same-slider s)
			     (test-control-event e '(slider))
			     (set! side-effect 'slider)
			     'oops)
			   3
			   '(horizontal))])
    (label-test s "Slider")
    (stv s command (make-object control-event% 'slider))
    (test 'slider 'slider-callback side-effect)
    
    (st 3 s get-value)
    (stv s set-value 4)
    (st 4 s get-value)
    (stv s set-value -2)
    (st -2 s get-value)
    (stv s set-value 8)
    (st 8 s get-value)

    (window-tests s #t #f parent frame 2))

  (let ([test-list-control
	 (lambda (l choice? multi?)
	   (st 3 l get-number)
	   
	   (st "A" l get-string 0)
	   (st "B" l get-string 1)
	   (st "C & D" l get-string 2)

	   (unless choice?
	     (st 'a l get-data 0)
	     (st #f l get-data 1)
	     (st 'c-&-d l get-data 2))
	   
	   (st 0 l find-string "A")
	   (st 1 l find-string "B")
	   (st 2 l find-string "C & D")
	   (st #f l find-string "C")

	   (stv l set-selection 2)
	   (st 2 l get-selection)
	   (st "C & D" l get-string-selection)
	   (stv l set-selection 1)
	   (st 1 l get-selection)
	   (st "B" l get-string-selection)
	   (stv l set-selection 0)
	   (st 0 l get-selection)
	   (st "A" l get-string-selection)
	   
	   (stv l set-string-selection "C & D")
	   (st 2 l get-selection)
	   (st "C & D" l get-string-selection)
	   (stv l set-string-selection "B")
	   (st 1 l get-selection)
	   (st "B" l get-string-selection)
	   (stv l set-string-selection "A")
	   (st 0 l get-selection)
	   (st "A" l get-string-selection)

	   (stv l set-selection 2)
	   
	   (unless choice?
	     (st '(2) l get-selections)
	     (stv l set-selection 1)
	     (st #t l is-selected? 1)
	     (st #f l is-selected? 2)
	     (st '(1) l get-selections)
	     (stv l set-selection 2)
	     (st #f l is-selected? 1)
	     (st #t l is-selected? 2)
	     
	     (stv l select 2 #f)
	     (st '() l get-selections)
	     (st #f l get-selection)
	     (stv l select 0 #t)
	     (st '(0) l get-selections)

	     (stv l select 2 #t)
	     (st (if multi? '(0 2) '(2)) l get-selections)
	     (stv l select 1 #t)
	     (st (if multi? '(0 1 2) '(1)) l get-selections)
	     (stv l select 1 #f)
	     (st (if multi? '(0 2) '()) l get-selections)
	     (st (if multi? 0 #f) l get-selection)
	     (stv l select 2 #t)
	     (st (if multi? '(0 2) '(2)) l get-selections)
	     (st (if multi? 0 2) l get-selection)
	     (st multi? l is-selected? 0)
	     (st #t l is-selected? 2)
	     (stv l set-selection 2)
	     (st '(2) l get-selections))

	   (if choice?
	       (stv l append "E")
	       (stv l append "E" 'e))
	   (st 4 l get-number)
	   (st 2 l get-selection)
	   (unless choice?
	     (st 'e l get-data 3))
	   (stv l append "F & G")
	   (st 5 l get-number)
	   (st 2 l get-selection)
	   (unless choice?
	     (st #f l get-data 4))

	   (stv l set-selection 4)
	   (st 4 l get-selection)
	   (st "F & G" l get-string-selection)
	   (stv l set-selection 2)
	   (stv l set-string-selection "F & G")
	   (st 4 l get-selection)
	   (st "F & G" l get-string-selection)

	   (unless choice?
	     (stv l delete 1)
	     (st 4 l get-number)
	     (st "A" l get-string 0)
	     (st 'a l get-data 0)
	     (st "C & D" l get-string 1)
	     (st 'c-&-d l get-data 1)

	     (stv l delete 0)
	     (st 3 l get-number))

	   (stv l clear)
	   (st 0 l get-number)
	   (st #f l get-selection)
	   (st #f l get-string-selection)

	   (stv l append "Z")
	   (st 1 l get-number)
	   (when choice?
	     (st 0 l get-selection)
	     (st "Z" l get-string-selection))

	   'done-list)])

    (printf "Choice~n")
    (letrec ([c (make-object choice%
			     "&Choice"
			     '("A" "B" "C & D")
			     parent
			     (lambda (ch e)
			       (test ch 'same-choice c)
			       (test-control-event e '(choice))
			       (set! side-effect 'choice)
			       'oops)
			     null)])
      (label-test c "Choice")
      (stv c command (make-object control-event% 'choice))
      (test 'choice 'choice-callback side-effect)
      
      (st 0 c get-selection)

      (test-list-control c #t #f)
      
      (window-tests c #f #f parent frame 2))

    (let ([mk-list
	   (lambda (style)
	     (printf "List Box: ~a~n" style)
	     (letrec ([l (make-object list-box%
				      "&List Box"
				      '("A" "B" "C & D")
				      parent
				      (lambda (lb e)
					(test lb 'same-list-box l)
					(test-control-event e '(list-box))
					(set! side-effect 'list-box)
					'oops)
				      (list style))])
	       (label-test l "List Box")
	       (stv l command (make-object control-event% 'list-box))
	       (test 'list-box 'list-box-callback side-effect)
	       
	       (stv l set-data 0 'a)
	       (stv l set-data 2 'c-&-d)
	       
	       (test-list-control l #f (and (memq style '(multiple extended)) #t))
	       
	       (window-tests l #t #t parent frame 2)

	       (stv parent delete-child l)))])

      (mk-list 'single)
      (mk-list 'multiple)
      (mk-list 'extended))

    'done-lists)

  (let ([c (make-object canvas% parent '(hscroll vscroll))])

    (printf "Tab Focus~n")
    (st #f c accept-tab-focus)
    (stv c accept-tab-focus #t)
    (st #t c accept-tab-focus)
    (stv c accept-tab-focus #f)
    (st #f c accept-tab-focus)

    (stv c set-scrollbars 100 101 5 6 2 3 10 20 #t)
    (let-values ([(w h) (send c get-virtual-size)]
		 [(cw ch) (send c get-client-size)]
		 [(s1x s1y) (values 0 0)])
      (printf "Canvas size: Virtual: ~a x ~a  Client: ~a x ~a~n" w h cw ch)
      (let ([check-scroll
	     (lambda (xpos ypos)
	       (let-values ([(x y) (send c get-view-start)])
		 (test (* xpos s1x) `(canvas-view-x ,xpos ,ypos ,x ,cw) x)
		 (test (* ypos s1y) `(canvas-view-y ,xpos ,ypos ,y ,ch) y)))])
	(test (* 100 5) 'canvas-virt-w-size w)
	(test (* 101 6) 'canvas-virt-h-size h)
	
	(let-values ([(x y) (send c get-view-start)])
	  (printf "Canvas Init View: ~a ~a~n" x y)
	  (test #t 'canvas-view-x (<= (- w cw) x (+ cw (- w cw))))
	  (test #t 'canvas-view-y (<= (- h ch) y (+ ch (- h ch)))))
	
	(st 0 c get-scroll-pos 'horizontal)
	(st 0 c get-scroll-pos 'vertical)
	(st 0 c get-scroll-page 'horizontal)
	(st 0 c get-scroll-page 'vertical)
	(st 0 c get-scroll-range 'horizontal)
	(st 0 c get-scroll-range 'vertical)
	
	(stv c scroll 1 1)
	(let-values ([(x y) (send c get-view-start)])
	  (set! s1x x)
	  (set! s1y y))
	(check-scroll 1 1)
	(stv c scroll #f 2)
	(check-scroll 1 2)
	(stv c scroll 0 #f)
	(check-scroll 0 2)
	
	'done-sb))

    (stv c set-scrollbars 100 101 5 6 2 3 10 20 #f)
    (let-values ([(w h) (send c get-virtual-size)]
		 [(cw ch) (send c get-client-size)])
      (let ([check-scroll
	     (lambda (xpos ypos)
	       (st xpos c get-scroll-pos 'horizontal)
	       (st ypos c get-scroll-pos 'vertical)
	
	       (test cw 'canvas-virt-w-size w)
	       (test ch 'canvas-virt-h-size h)
	       
	       (let-values ([(x y) (send c get-view-start)])
		 (test 0 'canvas-view-x x)
		 (test 0 'canvas-view-y y)))])
	
	(check-scroll 5 6)
	
	(st 2 c get-scroll-page 'horizontal)
	(st 3 c get-scroll-page 'vertical)
	(st 5 c get-scroll-range 'horizontal)
	(st 6 c get-scroll-range 'vertical)
	
	(stv c scroll 1 1)
	(check-scroll 1 1)
	(stv c scroll #f 2)
	(check-scroll 1 2)
	(stv c scroll 0 #f)
	(check-scroll 0 2)

	(stv c set-scroll-pos 'horizontal 1)
	(check-scroll 1 2)
	(stv c set-scroll-pos 'vertical 0)
	(check-scroll 1 0)
	
	(stv c set-scroll-page 'horizontal 1)
	(st 1 c get-scroll-page 'horizontal)
	(st 3 c get-scroll-page 'vertical)
	(stv c set-scroll-page 'vertical 2)
	(st 1 c get-scroll-page 'horizontal)
	(st 2 c get-scroll-page 'vertical)
		
	'done-sb))

    (window-tests c #t #t parent frame 0))

  'done)

(test-controls frame frame)

(newline)
(if (null? errs)
    (printf "Passed all ~a tests~n" test-count)
    (begin
      (printf "~a Error(s) in ~a tests~n" (length errs) test-count)
      '(for-each
       (lambda (s)
	 (printf "~a~n" s))
       (reverse errs))))
