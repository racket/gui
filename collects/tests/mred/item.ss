
(define my-txt #f)
(define my-lb #f)

(define special-font (send wx:the-font-list find-or-create-font
			   20 'decorative 
			   'normal 'bold
			   #f))

(define (make-h&s cp f)
  (make-object button% "Hide and Show" cp
	       (lambda (b e) (send f show #f) (send f show #t))))

(define (add-hide name w cp)
  (let ([c (make-object check-box% (format "Show ~a" name) cp
			(lambda (c e) (send w show (send c get-value))))])
    (send c set-value #t)))

(define (add-disable name w ep)
  (let ([c (make-object check-box% (format "Enable ~a" name) ep
			(lambda (c e) (send w enable (send c get-value))))])
    (send c set-value #t)))

(define (add-disable-radio name w i ep)
  (let ([c (make-object check-box% (format "Enable ~a" name) ep
			(lambda (c e) (send w enable i (send c get-value))))])
    (send c set-value #t)))

(define (add-change-label name w lp orig other)
  (make-object button% (format "Relabel ~a" name) lp
	       (let ([orig-name (if orig orig (send w get-label))]
		     [changed? #f])
		 (lambda (b e)
		   (if changed?
		       (unless (null? orig-name)
			 (send w set-label orig-name))
		       (send w set-label other))
		   (set! changed? (not changed?))))))

(define (add-focus-note frame panel)
  (define m (make-object message% "focus: ??????????????????????????????" panel))
  (send
   (make-object
    (class-asi wx:timer%
      (inherit start)
      (public
	[notify
	 (lambda ()
	   (when (send frame is-shown?)
	     (send m set-label
		   (let* ([w (with-handlers ([void (lambda (x) #f)]) (mred:test:get-focused-window))]
			  [l (and w (send w get-label))])
		     (format "focus: ~a ~a" (or l "") w)))
	     (start 1000 #t)))])))
   start 1000 #t))

(define (add-pre-note frame panel)
  (define m (make-object message% "pre: ??????????????????????????????" panel))
  (define cm (make-object check-box% "Drop Mouse Events" panel void))
  (define ck (make-object check-box% "Drop Key Events" panel void))
  (lambda (win e)
    (let ([m? (is-a? e wx:mouse-event%)])
      (send m set-label
	    (format "pre: ~a ~a"
		    (if m? "mouse" "key")
		    (let ([l (send win get-label)])
		      (if (not l)
			  win
			  l))))
      (and (not (or (eq? win cm) (eq? win ck)))
	   (or (and m? (send cm get-value))
	       (and (not m?) (send ck get-value)))))))

(define (add-cursors frame panel ctls)
  (let ([old #f]
	[f-old #f]
	[bc (make-object wx:cursor% 'bullseye)]
	[cc (make-object wx:cursor% 'cross)])
    (make-object check-box% "Control Bullseye Cursors" panel
		 (lambda (c e)
		   (if (send c get-value)
		       (set! old 
			     (map (lambda (b) 
				    (begin0
				     (send b get-cursor)
				     (send b set-cursor bc)))
				  ctls))
		       (map (lambda (b c) (send b set-cursor c))
			    ctls old))))
    (make-object check-box% "Frame Cross Cursor" panel
		 (lambda (c e)
		   (if (send c get-value)
		       (begin
			 (set! f-old (send frame get-cursor))
			 (send frame set-cursor cc))
		       (send frame set-cursor f-old))))
    (make-object check-box% "Busy Cursor" panel
		 (lambda (c e)
		   (if (send c get-value)
		       (wx:begin-busy-cursor)
		       (wx:end-busy-cursor))))))
		       
(define OTHER-LABEL "XXXXXXXXXXXXXXXXXXXXXX")

(define-values (icons-path local-path)
  (let ([d (current-load-relative-directory)])
    (values
     (lambda (n)
       (build-path (collection-path "icons") n))
     (lambda (n)
       (build-path d n)))))

(define popup-test-canvas%
  (class canvas% (objects names . args)
    (inherit popup-menu get-dc)
    (public
      [last-m null]
      [last-choice #f]
      [on-paint
       (lambda ()
	 (let ([dc (get-dc)])
	   (send dc clear)
	   (send dc draw-text "Left: popup hide state" 0 0)
	   (send dc draw-text "Right: popup previous" 0 20)
	   (send dc draw-text (format "Last pick: ~s" last-choice) 0 40)))]
      [on-event
       (lambda (e)
	 (if (send e button-down?)
	     (let ([x (send e get-x)]
		   [y (send e get-y)]
		   [m (if (or (null? last-m)
			      (send e button-down? 1))
			  (let ([m (make-object popup-menu% "Title")]
				[make-callback 
				 (let ([id 0])
				   (lambda (m e)
				     (set! id (add1 id))
				     (let ([id id])
				       (lambda ()
					 (set! last-choice id)
					 (on-paint)))))])
			    (for-each
			     (lambda (obj name)
			       (make-object menu-item%
					    (string-append
					     name ": "
					     (if (send obj is-shown?)
						 "SHOWN"
						 "<h i d d e n>"))
					    m
					    (make-callback)))
			     objects names)
			    m)
			  last-m)])
	       (set! last-m m)
	       (popup-menu m x y))))])
    (sequence
      (apply super-init args))))

(define prev-frame #f)

(define bitmap%
  (class wx:bitmap% args
    (inherit ok?)
    (sequence
      (apply super-init args)
      (unless (ok?)
	(printf "bitmap failure: ~s~n" args)))))

(define active-frame%
  (class-asi frame%
    (private (pre-on void))
    (public [pre-on-event (lambda args (apply pre-on args))]
	    [pre-on-char pre-on-event]
	    [set-info
	     (lambda (ep)
	       (set! pre-on (add-pre-note this ep)))])))

(define (make-ctls ip cp lp add-testers ep radio-h? label-h? null-label? stretchy?)
  
  (define return-bmp 
    (make-object bitmap% (icons-path "return.xbm") 'xbm))
  (define bb-bmp
    (make-object bitmap% (icons-path "bb.gif") 'gif))
  (define mred-bmp
    (make-object bitmap% (icons-path "mred.xbm") 'xbm))
  (define nruter-bmp
    (make-object bitmap% (local-path "nruter.xbm") 'xbm))
  
  (define :::dummy:::
    (when (not label-h?)
      (send ip set-label-position 'vertical)))
  
  (define-values (l il)
    (let ([p (make-object horizontal-panel% ip)])
      (send p stretchable-width stretchy?)
      (send p stretchable-height stretchy?)
      
      (begin
	(define l (make-object message% "Me&ssage" p))
	(define il (make-object message% return-bmp p))
	
	(add-testers "Message" l)
	(add-change-label "Message" l lp #f OTHER-LABEL)
	
	(add-testers "Image Message" il)
	(add-change-label "Image Message" il lp return-bmp nruter-bmp)
	
	(values l il))))
  
  (define b (make-object button% "He&llo" ip void))
  
  (define ib (make-object button% bb-bmp ip void))
  
  ; (define ib2 (make-object button% return-bmp ip void))
  
  (define lb (make-object list-box%
			  (if null-label? #f "L&ist")
			  '("Apple" "Banana" "Coconut & Donuts")
			  ip void))
  
  (define cb (make-object check-box% "C&heck" ip void))
  
  (define icb (make-object check-box% mred-bmp ip void))
  
  (define rb (make-object radio-box%
			  (if null-label? #f "R&adio")
			  '("First" "Dos" "T&rio")
			  ip void 
			  (if radio-h?
			      '(horizontal)
			      '(vertical))))
  
  (define irb (make-object radio-box%
			   (if null-label? #f "Image Ra&dio")
			   (list return-bmp nruter-bmp)
			    ip void 
			    (if radio-h?
				'(horizontal)
				'(vertical))))
  
  (define ch (make-object choice% 
			  (if null-label? #f "Ch&oice")
			  '("Alpha" "Beta" "Gamma" "Delta & Rest")
			  ip void))
  
  (define txt (make-object text%
			   (if null-label? #f "T&ext")
			   ip void
			   "initial & starting"))
  
  (set! my-txt txt)
  (set! my-lb lb)

  (add-testers "Button" b)
  (add-change-label "Button" b lp #f OTHER-LABEL)
  
  (add-testers "Image Button" ib)
  (add-change-label "Image Button" ib lp bb-bmp return-bmp)
  
  (add-testers "List" lb)
  (add-change-label "List" lb lp #f OTHER-LABEL)
  
  (add-testers "Checkbox" cb)
  (add-change-label "Checkbox" cb lp #f OTHER-LABEL)
  
  (add-testers "Image Checkbox" icb)
  (add-change-label "Image Checkbox" icb lp mred-bmp bb-bmp)
  
  (add-testers "Radiobox" rb)
  (add-disable-radio "Radio Item `First'" rb 0 ep)
  (add-disable-radio "Radio Item `Dos'" rb 1 ep)
  (add-disable-radio "Radio Item `Trio'" rb 2 ep)
  (add-change-label "Radiobox" rb lp #f OTHER-LABEL)
  
  (add-testers "Image Radiobox" irb)
  (add-disable-radio "Radio Image Item 1" irb 0 ep)
  (add-disable-radio "Radio Image Item 2" irb 1 ep)
  (add-change-label "Image Radiobox" irb lp #f OTHER-LABEL)
  
  (add-testers "Choice" ch)
  (add-change-label "Choice" ch lp #f OTHER-LABEL)
  
  (add-testers "Text" txt)
  (add-change-label "Text" txt lp #f OTHER-LABEL)
  
  (let ([items (list l il 
		     b ib 
		     lb
		     cb icb 
		     rb irb 
		     ch
		     txt)])
    (cons (make-object popup-test-canvas% 
		       items
		     (list "label" "image label"
			   "button" "image button"
			   "list box"
			   "checkbox" "image checkbox"
			   "radio box" "image radiobox"
			   "choice"
			   "text")
		     cp)
	  items)))

(define (big-frame h-radio? v-label? null-label? stretchy? special-label-font? special-button-font?)
  (define f (make-object active-frame% "Tester"))
  
  (define hp (make-object horizontal-panel% f))
  
  (define ip (make-object vertical-panel% hp))
  (define cp (make-object vertical-panel% hp))
  (define ep (make-object vertical-panel% hp))
  (define lp (make-object vertical-panel% hp))
  
  (define (basic-add-testers name w)
    (add-hide name w cp)
    (add-disable name w ep))
  
  (define add-testers
    (if stretchy?
	(lambda (name control)
	  (send control stretchable-width #t)
	  (send control stretchable-height #t)
	  (basic-add-testers name control))
	basic-add-testers))
  
  (define fp (make-object vertical-panel% ip))
  
  (define tp (make-object vertical-panel% fp))

  (make-h&s cp f)
  
  (add-testers "Sub-panel" fp)
  
  (add-testers "Sub-sub-panel" tp)

  (when special-label-font?
    (send tp set-label-font special-font))
  (when special-button-font?
    (send tp set-control-font special-font))
    
  (let ([ctls (make-ctls tp cp lp add-testers ep h-radio? v-label? null-label? stretchy?)])
    (add-focus-note f ep)
    (send f set-info ep)
    
    (add-cursors f lp ctls))

  (send f show #t)
  (set! prev-frame f)
  f)

(define (med-frame radio-h? label-h? null-label? stretchy? special-label-font? special-button-font?)
  (define f2 (make-object active-frame% "Tester2"))

  (define hp2 (make-object horizontal-panel% f2))
  
  (define ip2-0 (make-object vertical-panel% hp2))
  (define cp2 (make-object vertical-panel% hp2))
  (define ep2 (make-object vertical-panel% hp2))
  (define lp2 (make-object vertical-panel% hp2))
  
  (define (basic-add-testers2 name w)
    (add-hide name w cp2)
    (add-disable name w ep2))
  
  (define add-testers2
    (if stretchy?
	(lambda (name control)
	  (send control stretchable-width #t)
	  (send control stretchable-height #t)
	  (basic-add-testers2 name control))
	basic-add-testers2))

  (define fp2 (make-object vertical-panel% ip2-0))  
  (define ip2 (make-object vertical-panel% fp2))

  (make-h&s cp2 f2)
  
  (add-testers2 "Sub-panel" fp2)
  (add-testers2 "Sub-sub-panel" ip2)
  
  (when prev-frame
    (add-disable "Previous Tester Frame" prev-frame ep2))
  
  (when (not label-h?)
    (send ip2 set-label-position 'vertical))

  (when special-label-font?
    (send ip2 set-label-font special-font))
  (when special-button-font?
    (send ip2 set-control-font special-font))
  
  (begin
    (define sh (make-object slider% 
			    (if null-label? #f "H S&lider") 0 10 ip2
			    (lambda (s e)
			      (send gh set-value (send sh get-value)))
			    5
			    '(horizontal)))
    
    (define sv (make-object slider% 
			    (if null-label? #f "V Sl&ider") 0 10 ip2 
			    (lambda (s e)
			      (send gv set-value (send sv get-value)))
			    5
			    '(vertical)))
    
    (define gh (make-object gauge% 
			    (if null-label? #f "H G&auge") ip2
			    10 '(horizontal)))
    
    (define gv (make-object gauge% 
			    (if null-label? #f "V Ga&uge") ip2
			    10 '(vertical)))
    
#|
    (define cmt (make-object canvas-message% ip2
			     "Howdy"))
    
    (define cmi (make-object mred:canvas-message% ip2
			     (make-object bitmap% (icons-path "bb.gif")
					  wx:const-bitmap-type-gif)))
|#  

    (define txt (make-object multi-text% 
			     (if null-label? #f "T&ext") ip2 void 
			     "initial & starting"))

    (add-testers2 "Horiz Slider" sh)
    (add-testers2 "Vert Slider" sv)
    (add-testers2 "Horiz Gauge" gh)
    (add-testers2 "Vert Gauge" gv)
    ; (add-testers2 "Text Message" cmt)
    ; (add-testers2 "Image Message" cmi)
    (add-testers2 "Text" txt)
    
    (add-change-label "Horiz Slider" sh lp2 #f OTHER-LABEL)
    (add-change-label "Vert Slider" sv lp2 #f OTHER-LABEL)
    (add-change-label "Horiz Gauge" gh lp2 #f OTHER-LABEL)
    (add-change-label "Vert Gauge" gv lp2 #f OTHER-LABEL)
    (add-change-label "Text" txt lp2 #f OTHER-LABEL)
    

    (let* ([items (list sh sv
			gh gv
			; cmt cmi
			txt)]
	   [canvas  (make-object popup-test-canvas% 
				 items
				 (list "h slider" "v slider"
				       "v gauge" "v gauge"
				       ; "text msg" "image msg"
				       "text")
				 cp2)])
      
      (add-focus-note f2 ep2)
      (send f2 set-info ep2)
      
      (add-cursors f2 lp2 (cons canvas items)))
    
    (send f2 show #t)
    (set! prev-frame f2)
    f2))

; Need: check, check-test, and enable via menubar
; All operations on Submenus
(define f%
  (class frame% args
    (private
      ADD-APPLE
      ADD-BANANA
      ADD-COCONUT
      DELETE-APPLE
      DELETE-EXTRA-BANANA
      DELETE-BANANA
      DELETE-COCONUT-0
      DELETE-COCONUT
      DELETE-COCONUT-2
      COCONUT-ID
      DELETE-ONCE
      APPLE-CHECK-ID)
    (private
      menu-bar
      main-menu
      apple-menu
      banana-menu
      coconut-menu
      baseball-ids
      hockey-ids
      enable-item)
    (sequence (apply super-init args))
    (public
      [make-menu-bar
       (lambda ()
	 (let* ([mb (make-object menu-bar% this)]
		[menu (make-object menu% "Tester" mb)]
		[new (case-lambda 
		      [(l help parent) (make-object menu-item% l parent callback #f help)]
		      [(l help) (make-object menu-item% l menu callback #f help)]
		      [(l) (make-object menu-item% l menu callback)])]
		[sep (lambda () (make-object separator-menu-item% menu))])
	   (set! menu-bar mb)
	   (set! main-menu menu)
	   
	   (set! ADD-APPLE (new "Add Apple" "Adds the Apple menu"))
	   (set! ADD-BANANA (new "Add Banana"))
	   (set! ADD-COCONUT (new "Add Coconut"))
	   
	   (make-object menu-item% "Append Donut" menu
			(lambda (m e) 
			  (make-object menu-item% "Donut" apple-menu void)))
	   (sep)
	   (set! DELETE-COCONUT-0 (new "Delete Coconut"))
	   (make-object menu-item% "Delete Apple" menu
			(lambda (m e) 
			  (send (send apple-menu get-item) delete)
			  (set! apple-installed? #f)))
	   
	   (sep)
	   (set! enable-item
		 (make-object checkable-menu-item% "Apple Once Disabled" menu
			      (lambda (m e)
				(send DELETE-ONCE enable
				      (not (send enable-item is-checked?))))))
	   
	   (let ([mk-enable (lambda (on?)
			      (lambda (m e)
				(let ([l (send menu-bar get-items)])
				  (unless (null? (cdr l))
				    (send (cadr l) enable on?)))))])
	     (make-object menu-item% "Disable Second" menu (mk-enable #f))
	     (make-object menu-item% "Enable Second" menu (mk-enable #t)))

	   (sep)
	   '(set! baseball-ids
		  (send menu append-check-set
			(list "Astros" "Braves" "Cardinals")
			(lambda (which)
			  (wx:message-box (format "~s Checked" which)))))
	   (sep)
	   '(set! hockey-ids
		  (send menu append-check-set
			`(("Aeros" . Houston) 
			  ("Bruins" . Boston)
			  ("Capitols" . Washington))
			(lambda (which)
			  (wx:message-box (format "~s Checked" which)))))
	   
	   (let ([make-menu
		  (opt-lambda (title parent help-string)
		    (let ([m (make-object menu% title parent help-string)])
		      (send (send m get-item) delete)
		      m))])
	     (set! apple-menu (make-menu "Apple" mb #f))
	     (set! banana-menu (make-menu "Banana" mb #f))
	     (set! coconut-menu (make-menu "Coconut" apple-menu "Submenu")))
	   
	   (set! COCONUT-ID (send coconut-menu get-item))

	   (set! DELETE-ONCE (new "Delete Once" #f apple-menu))
	   (set! DELETE-APPLE (new "Delete Apple" "Deletes the Apple menu" apple-menu))
	   (set! APPLE-CHECK-ID (make-object checkable-menu-item% "Checkable" apple-menu void))

	   (set! DELETE-BANANA (new "Delete Banana" #f banana-menu))
	   (set! DELETE-EXTRA-BANANA (new "Delete First Banana Item" #f banana-menu))

	   (set! DELETE-COCONUT (new "Delete Coconut" #f coconut-menu))
	   (set! DELETE-COCONUT-2 (new "Delete Coconut By Position" #f coconut-menu))))]
      
      [callback
       (lambda (op ev)
	 (cond
	  [(eq? op ADD-APPLE)
	   (send (send apple-menu get-item) restore)
	   (set! apple-installed? #t)]
	  [(eq? op ADD-BANANA)
	   (send (send banana-menu get-item) restore)]
	  [(eq? op ADD-COCONUT)
	   (send (send coconut-menu get-item) restore)]
	  [(eq? op DELETE-ONCE)
	   (send DELETE-ONCE delete)]
	  [(eq? op DELETE-APPLE)
	   (send (send apple-menu get-item) delete)
	   (set! apple-installed? #f)]
	  [(eq? op DELETE-BANANA)
	   (send (send banana-menu get-item) delete)]
	  [(eq? op DELETE-EXTRA-BANANA)
	   (send (car (send banana-menu get-items)) delete)]
	  [(or (eq? op DELETE-COCONUT) (eq? op DELETE-COCONUT-0))
	   (send COCONUT-ID delete)]
	  [(eq? op DELETE-COCONUT-2)
	   (send (list-ref (send apple-menu get-items) 3) delete)]))])
    (public
	[mfp (make-object vertical-panel% this)]
	[mc (make-object media-canvas% mfp)]
	[restp (make-object vertical-panel% mfp)]
	[sbp (make-object horizontal-panel% restp)]
	[mfbp (make-object horizontal-panel% restp)]
	[lblp (make-object horizontal-panel% restp)]
	[badp (make-object horizontal-panel% restp)]
	[e (make-object media-edit%)])
      (sequence
	(send restp stretchable-height #f)
	(send mc min-height 250)
	(send mc set-media e)
	(send e load-file (local-path "menu-steps.txt")))
      (public
	[make-test-button
	 (lambda (name pnl menu id)
	   (make-object button%
			(format "Test ~a" name) pnl 
			(lambda (b e)
			  (wx:message-box
			   (if (send id is-checked?)
			       "yes"
			       "no")
			   "Checked?"))))]
	[compare
	 (lambda (expect v kind)
	   (unless (or (and (string? expect) (string? v)
			    (string=? expect v))
		       (eq? expect v))
	     (error 'test-compare "~a mismatch: ~s != ~s" kind expect v)))]
	[check-parent
	 (lambda (menu id)
	   (unless use-menubar?
	     (unless (eq? (send id get-parent) menu)
	       (error 'check-parent "parent mismatch: for ~a, ~a != ~a"
		      (send id get-label)
		      (send (send menu get-item) get-label)
		      (send (send (send id get-parent) get-item) get-label)))))]
	[label-test
	 (lambda (menu id expect)
	   (check-parent menu id)
	   (let ([v (send id get-label)])
	     (compare expect v "label")))]
	[top-label-test
	 (lambda (pos expect)
	   (let ([i (send menu-bar get-items)])
	     (and (> (length i) pos)
		  (let ([v (send (list-ref i pos) get-label)])
		    (compare expect v "top label")))))]
	[help-string-test
	 (lambda (menu id expect)
	   (check-parent menu id)
	   (let ([v (send id get-help-string)])
	     (compare expect v "help string")))]
	[find-test
	 (lambda (menu title expect string)
	   (letrec ([find
		     (lambda (menu str)
		       (let ([items (send menu get-items)])
			 (ormap (lambda (i)
				  (and (is-a? i labelled-menu-item<%>)
				       (equal? (send i get-plain-label) str)
				       i))
				items)))]
		    [find-item
		     (lambda (menu str)
		       (or (find menu str)
			   (let ([items (send menu get-items)])
			     (ormap (lambda (i)
				      (and (is-a? i submenu-item<%>)
					   (find-item (send i get-menu) str)))
				    items))))]
		    [v (if use-menubar? 
			   (let ([item (find menu-bar title)])
			     (if item
				 (find-item (send item get-menu) string)
				 -1))
			   (find-item menu string))])
	     (compare expect v (format "label search: ~a" string))))]
	[tell-ok
	 (lambda ()
	   (printf "ok~n"))]
	[temp-labels? #f]
	[use-menubar? #f]
	[apple-installed? #f]
	[via (lambda (menu) (if use-menubar? menu-bar menu))]
	[tmp-pick (lambda (a b) (if temp-labels? a b))]
	[apple-pick (lambda (x a b) (if (and use-menubar? (not apple-installed?))
					x
					(tmp-pick a b)))])
      (sequence
	(make-menu-bar)

	(send (send apple-menu get-item) restore)

	(make-object button%
		     "Delete Tester" sbp 
		     (lambda args
		       (send (send main-menu get-item) delete)))
	(make-object button%
		     "Delete First Menu" sbp
		     (lambda args
		       (send (car (send menu-bar get-items)) delete)))
	(make-object button%
		     "Add Tester" sbp
		     (lambda args
		       (send (send main-menu get-item) restore)))
	(make-object button%
		     "Add Delete Banana" sbp
		     (lambda args
		       (send DELETE-BANANA restore)))
	(make-object button%
		     "Counts" sbp
		     (lambda args
		       (wx:message-box
			(format "MB: ~a; T: ~a; A: ~a; B: ~a"
				(length (send menu-bar get-items))
				(length (send main-menu get-items))
				(length (send apple-menu get-items))
				(length (send banana-menu get-items)))
			"Counts")))

	'(make-test-button "Aeros" mfbp main-menu (list-ref hockey-ids 0))
	'(make-test-button "Bruins" mfbp main-menu (list-ref hockey-ids 1))
	'(make-test-button "Capitols" mfbp main-menu (list-ref hockey-ids 2))
	(make-test-button "Apple Item" mfbp apple-menu APPLE-CHECK-ID)
	(make-object button%
		     "Check in Apple" mfbp
		     (lambda args
		       (send APPLE-CHECK-ID check #t)))
	
	(make-object button%
		     "Test Labels" lblp 
		     (lambda args
		       (label-test (via main-menu) ADD-APPLE (tmp-pick "Apple Adder" "Add Apple"))
		       (help-string-test (via main-menu) ADD-APPLE (tmp-pick "ADDER" "Adds the Apple menu"))
		       '(label-test (via main-menu) (car baseball-ids) (tmp-pick "'Stros" "Astros"))
		       '(help-string-test (via main-menu) (car baseball-ids) (tmp-pick "Houston" #f))
		       '(label-test (via main-menu) (cadr hockey-ids) "Bruins")
		       (label-test (via apple-menu) DELETE-APPLE (apple-pick #f "Apple Deleter" "Delete Apple"))
		       (help-string-test (via apple-menu) DELETE-APPLE (apple-pick #f "DELETER"
										   "Deletes the Apple menu"))
		       (label-test (via apple-menu) COCONUT-ID (apple-pick #f "Coconut!" "Coconut"))
		       (help-string-test (via apple-menu) COCONUT-ID (apple-pick #f "SUBMENU" "Submenu"))
		       (label-test (via coconut-menu) DELETE-COCONUT (apple-pick #f "Coconut Deleter" "Delete Coconut")) ; submenu test
		       (help-string-test (via coconut-menu) DELETE-COCONUT (apple-pick #f "CDELETER" #f))
		       (top-label-test 0 (if temp-labels? "Hi" "Tester"))
		       (top-label-test 1 (if apple-installed? "Apple" #f))
		       (tell-ok)))
	(make-object button%
		     "Find Labels" lblp
		     (lambda args
		       (find-test main-menu (tmp-pick "Hi" "Tester")
				  ADD-APPLE (tmp-pick "Apple Adder" "Add Apple"))
		       (find-test apple-menu "Apple" (apple-pick -1 DELETE-APPLE DELETE-APPLE)
				  (tmp-pick "Apple Deleter" "Delete Apple"))
		       (find-test apple-menu "Apple" (apple-pick -1 COCONUT-ID COCONUT-ID)
				  (tmp-pick "Coconut!" "Coconut"))
		       (find-test apple-menu "Apple" (apple-pick -1 DELETE-COCONUT DELETE-COCONUT)
				  (tmp-pick "Coconut Deleter" "Delete Coconut"))
		       (tell-ok)))
	(make-object button%
		     "Toggle Labels" lblp
		     (lambda args
		       (set! temp-labels? (not temp-labels?))
		       (let ([menu (via main-menu)])
			 (send ADD-APPLE set-label (tmp-pick "Apple Adder" "Add Apple"))
			 '(send (car baseball-ids) set-label (tmp-pick "'Stros" "Astros"))
			 (send DELETE-APPLE set-label (tmp-pick "Apple Deleter" "Delete Apple"))
			 (send COCONUT-ID set-label (tmp-pick "Coconut!" "Coconut"))
			 (send DELETE-COCONUT set-label (tmp-pick "Coconut Deleter" "Delete Coconut"))
			 (send ADD-APPLE set-help-string (tmp-pick "ADDER" "Adds the Apple menu"))
			 '(send (car baseball-ids) set-help-string (tmp-pick "Houston" #f))
			 (send DELETE-APPLE set-help-string (tmp-pick "DELETER" "Deletes the Apple menu"))
			 (send COCONUT-ID set-help-string (tmp-pick "SUBMENU" "Submenu"))
			 (send DELETE-COCONUT set-help-string (tmp-pick "CDELETER" #f))
			 (send (send main-menu get-item) set-label (if temp-labels? "Hi" "Tester")))))
	(letrec ([by-bar (make-object check-box%
				      "Via Menubar" lblp
				      (lambda args
					(set! use-menubar? (send by-bar get-value))))])
	  by-bar)
	
	#f)))

(define (menu-frame)
  (define mf (make-object f% "Menu Test"))
  (set! prev-frame mf)
  (send mf show #t)
  mf)

(define (check-callback-event orig got e types silent?)
  (unless (eq? orig got)
    (error "object not the same"))
  (unless (is-a? e wx:control-event%)
    (error "bad event object"))
  (let ([type (send e get-event-type)])
    (unless (memq type types)
      (error (format "bad event type: ~a" type))))
  (unless silent?
    (printf "Callback Ok~n")))

(define (instructions v-panel file)
  (define c (make-object media-canvas% v-panel))
  (define m (make-object media-edit%))
  (send c set-media m)
  (send m load-file (local-path file))
  (send m lock #t)
  (send c min-width 520)
  (send c min-height 200))

(define (button-frame mred:frame% style)
  (define f (make-object frame% "Button Test"))
  (define p (make-object vertical-panel% f))
  (define old-list null)
  (define commands (list 'button))
  (define hit? #f)
  (define b (make-object button%
			 "Hit Me" p
			 (lambda (bx e)
			   (set! hit? #t)
			   (set! old-list (cons e old-list))
			   (check-callback-event b bx e commands #f))
			 style))
  (define c (make-object button%
			 "Check" p
			 (lambda (c e)
			   (for-each
			    (lambda (e)
			      (check-callback-event b b e commands #t))
			    old-list)
			   (printf "All Ok~n"))))
  (define e (make-object button%
			 "Disable Test" p
			 (lambda (c e)
			   (sleep 1)
			   (set! hit? #f)
			   (let ([sema (make-semaphore)])
			     (send b enable #f)
			     (thread (lambda () (sleep 0.5) (semaphore-post sema)))
			     (wx:yield sema)
			     (when hit?
			       (printf "un-oh~n"))
			     (send b enable #t)))))
  (instructions p "button-steps.txt")
  (send f show #t))

(define (checkbox-frame)
  (define f (make-object frame% "Checkbox Test"))
  (define p (make-object vertical-panel% f))
  (define old-list null)
  (define commands (list 'check-box))
  (define cb (make-object check-box%
			  "On" p
			  (lambda (cx e)
			    (set! old-list (cons e old-list))
			    (check-callback-event cb cx e commands #f))))
  (define t (make-object button%
			 "Toggle" p
			 (lambda (t e)
			   (let ([on? (send cb get-value)])
			     (send cb set-value (not on?))))))
  (define t2 (make-object button%
			  "Simulation Toggle" p
			  (lambda (t e)
			    (let ([on? (send cb get-value)]
				  [e (make-object wx:control-event% 'check-box)])
			      (send cb set-value (not on?))
			      (send cb command e)))))
  (define c (make-object button%
			 "Check" p
			 (lambda (c e)
			   (for-each
			    (lambda (e)
			      (check-callback-event cb cb e commands #t))
			    old-list)
			   (printf "All Ok~n"))))
  (instructions p "checkbox-steps.txt")
  (send f show #t))

(define (radiobox-frame)
  (define f (make-object frame% "Radiobox Test"))
  (define p (make-object vertical-panel% f))
  (define old-list null)
  (define commands (list 'radio-box))
  (define hp (make-object horizontal-panel% p))
  (define _ (send hp stretchable-height #f))
  (define callback (lambda (rb e)
		     (set! old-list (cons (cons rb e) old-list))
		     (check-callback-event rb rb e commands #f)))
  (define rb1-l (list "Singleton"))
  (define rb1 (make-object radio-box% "&Left" rb1-l hp callback))
  (define rb2-l (list "First" "Last"))
  (define rb2 (make-object radio-box% "&Center" rb2-l hp callback))
  (define rb3-l (list "Top" "Middle" "Bottom"))
  (define rb3 (make-object radio-box% "&Right" rb3-l hp callback))

  (define rbs (list rb1 rb2 rb3))
  (define rbls (list rb1-l rb2-l rb3-l))
  (define normal-sel (lambda (rb p) (send rb set-selection p)))
  (define simulate-sel (lambda (rb p)
			 (let ([e (make-object wx:control-event% 'radio-box)])
			   (send rb set-selection p)
			   (send rb command e))))

  (define do-sel (lambda (sel n) (for-each (lambda (rb) (sel rb (n rb))) rbs)))
  (define sel-minus (lambda (sel) (do-sel sel (lambda (rb) -1))))
  (define sel-first (lambda (sel) (do-sel sel (lambda (rb) 0))))
  (define sel-middle (lambda (sel) (do-sel sel (lambda (rb) (floor (/ (send rb get-number) 2))))))
  (define sel-last (lambda (sel) (do-sel sel (lambda (rb) (sub1 (send rb get-number))))))
  (define sel-N (lambda (sel) (do-sel sel (lambda (rb) (send rb get-number)))))
  (define (make-selectors title sel)
    (define hp2 (make-object horizontal-panel% p))
    (send hp2 stretchable-height #f)
    (make-object button% (format "Select -1~a" title) hp2 (lambda (b e) (sel-minus sel)))
    (make-object button% (format "Select First~a" title) hp2 (lambda (b e) (sel-first sel)))
    (make-object button% (format "Select Middle ~a" title) hp2 (lambda (b e) (sel-middle sel)))
    (make-object button% (format "Select Last~a" title) hp2 (lambda (b e) (sel-last sel)))
    (make-object button% (format "Select N~a" title) hp2 (lambda (b e) (sel-N sel))))
  (make-selectors "" normal-sel)
  (make-selectors " by Simulate" simulate-sel)
  (make-object button% "Check" p
	       (lambda (c e)
		 (for-each
		  (lambda (rb l)
		    (let loop ([n 0][l l])
		      (unless (null? l)
			(let ([a (car l)]
			      [b (send rb get-item-label n)])
			  (unless (string=? a b)
			    (error "item name mismatch: ~s != ~s" a b)))
			(loop (add1 n) (cdr l)))))
		  rbs rbls)
		 (for-each
		  (lambda (rbe)
		    (check-callback-event (car rbe) (car rbe) (cdr rbe) commands #t))
		  old-list)
		 (printf "All Ok~n")))
  (instructions p "radiobox-steps.txt")
  (send f show #t))

(define (choice-or-list-frame list? list-style empty?)
  (define f (make-object mred:frame% null (if list? "List Test" "Choice Test")))
  (define p (make-object mred:vertical-panel% f))
  (define-values (actual-content actual-user-data)
    (if empty?
	(values null null)
	(values '("Alpha" "Beta" "Gamma")
		(list null null null))))
  (define commands 
    (if list?
	(list wx:const-event-type-listbox-command)
	(list wx:const-event-type-choice-command)))
  (define old-list null)
  (define multi? (or (= list-style wx:const-multiple)
		     (= list-style wx:const-extended)))
  (define callback
    (lambda (cx e)
      (when (zero? (send c get-number))
	    (error "Callback for empty choice/list"))
      (set! old-list (cons (list e
				 (send e get-command-int)
				 (send e get-command-string))
			   old-list))
      (cond
       [(or (not list?) (send e is-selection?))
	; selection
	(printf "Selected ~a~n" (send e get-command-int))
	(when multi?
	  (error "Single-selection message for multi-selection list"))
	(unless (or (not list?) (= (length (send c get-selections)) 1))
	  (error "Single-selection message with zero/multiple selections"))
	(unless (= (send e get-command-int) (send c get-selection))
	  (error "event selection value mismatch"))
	(unless (string=? (send e get-command-string)
			  (send c get-string-selection)
			  (send c get-string (send c get-selection)))
	  (error "selection string mismatch"))]
       [(send e is-double-click?)
	; double-click
	(printf "Double-click~n")
	(unless (= -1 (send e get-command-int))
	  (error "selection index is not -1"))
	(unless (null? (send e get-command-string))
	  (error "string selection not null:" (send e get-command-string)))]
       [else
	; misc multi-selection
	(printf "Changed~n")
	(unless multi?
	  (error "unknown event for a single-selection list"))
	(unless (= -1 (send e get-selection))
	  (error "selection is not -1"))
	(unless (null? (send e get-string))
	  (error "string selection is not null:" (send e get-string)))])
      (check-callback-event c cx e commands #f)))
  (define c (if list?
		(make-object mred:list-box% p
			 callback
			 "Tester"
			 list-style
			 -1 -1 -1 -1
			 actual-content)
		(make-object mred:choice% p
			     callback
			     "Tester"
			     -1 -1 -1 -1
			     actual-content)))
  (define counter 0)
  (define append-with-user-data? #f)
  (define ab (make-object mred:button% p
			  (lambda (b e)
			    (set! counter (add1 counter))
			    (let ([naya (format "~aExtra ~a" 
						(if (= counter 10)
						    (string-append
						     "This is a Really Long Named Item That Would Have Used the Short Name, Yes "
						     "This is a Really Long Named Item That Would Have Used the Short Name ")
						    "")
						counter)]
				  [naya-data (box 0)])
			      (set! actual-content (append actual-content (list naya)))
			      (set! actual-user-data (append actual-user-data (list naya-data)))
			      (if (and list? append-with-user-data?)
				  (send c append naya naya-data)
				  (begin
				    (send c append naya)
				    (when list?
					  (send c set-client-data 
						(sub1 (send c number))
						naya-data))))
			      (set! append-with-user-data?
				    (not append-with-user-data?))))
			  "Append"))
  (define cs (when list? 
	       (make-object mred:button% p
			    (lambda (b e)
			      (printf "top: ~a~nvisible count: ~a~n"
				      (send c get-first-item)
				      (send c number-of-visible-items)))
			    "Visible Indices")))
  (define cdp (make-object mred:horizontal-panel% p))
  (define rb (make-object mred:button% cdp
			  (lambda (b e)
			    (set! actual-content null)
			    (set! actual-user-data null)
			    (send c clear))
			  "Clear"))
  (define (delete p)
    (send c delete p)
    (when (<= 0 p (sub1 (length actual-content)))
      (if (zero? p)
	  (begin
	    (set! actual-content (cdr actual-content))
	    (set! actual-user-data (cdr actual-user-data)))
	  (begin
	    (set-cdr! (list-tail actual-content (sub1 p)) 
		      (list-tail actual-content (add1 p)))
	    (set-cdr! (list-tail actual-user-data (sub1 p)) 
		      (list-tail actual-user-data (add1 p)))))))
  (define db (if list?
		 (make-object mred:button% cdp
			      (lambda (b e)
				(let ([p (send c get-selection)])
				  (delete p)))
			      "Delete")
		 null))
  (define dab (if list?
		  (make-object mred:button% cdp
			       (lambda (b e)
				 (let ([p (send c get-selection)])
				   (delete (sub1 p))))
			       "Delete Above")
		  null))
  (define dbb (if list?
		  (make-object mred:button% cdp
			       (lambda (b e)
				 (let ([p (send c get-selection)])
				   (delete (add1 p))))
			       "Delete Below")
		  null))
  (define setb (if list?
		   (make-object mred:button% cdp
				(lambda (b e)
				  (send c set '("Alpha" "Beta" "Gamma"))
				  (set! actual-content '("Alpha" "Beta" "Gamma"))
				  (set! actual-user-data (list null null null)))
				"Reset")
		   null))
  (define (make-selectors method mname numerical?)
    (define p2 (make-object mred:horizontal-panel% p))
    (send p2 stretchable-height #f)
    (when numerical?
	  (make-object mred:button% p2
		       (lambda (b e)
			 (method -1))
		       (string-append "Select Bad -1" mname)))
    (make-object mred:button% p2
		 (lambda (b e)
		   (method 0))
		 (string-append "Select First" mname))
    (make-object mred:button% p2
		 (lambda (b e)
		   (method (floor (/ (send c number) 2))))
		 (string-append "Select Middle" mname))
    (make-object mred:button% p2
		 (lambda (b e)
		   (method (sub1 (send c number))))
		 (string-append  "Select Last" mname))
    (make-object mred:button% p2
		 (lambda (b e)
		   (method (if numerical?
			       (send c number)
			       #f)))
		 (string-append "Select Bad X" mname))
    #f)
  (define dummy-1 (make-selectors (ivar c set-selection) "" #t))
  (define dummy-2 (make-selectors (lambda (p) 
				    (if p
					(when (positive? (length actual-content))
					      (send c set-string-selection 
						    (list-ref actual-content p)))
					(send c set-string-selection "nada")))
				  " by Name"
				  #f))
  (define dummy-3 (make-selectors (lambda (p)
				    (let ([e (make-object wx:command-event%
							  (if list?
							      wx:const-event-type-listbox-command
							      wx:const-event-type-choice-command))])
				      (send e set-command-int p)
				      (send e set-extra-long 1)
				      (send e set-event-object c)
				      (send e set-command-string 
					    (if (< -1 p (length actual-content))
						(list-ref actual-content p)
						null))
				      (when list? (send c set-first-item p))
				      (send c command e)))
				  " by Simulate" #t))
  (define tb (make-object mred:button% p
			  (lambda (b e)
			    (let ([c (send c number)])
			      (unless (= c (length actual-content))
				      (error "bad number response")))
			    (let loop ([n 0][l actual-content][lud actual-user-data])
			      (unless (null? l)
				      (let ([s (car l)]
					    [sud (car lud)]
					    [sv (send c get-string n)]
					    [sudv (if list?
						      (send c get-client-data n)
						      #f)])
					(unless (string=? s sv)
						(error "get-string mismatch"))
					(unless (or (not list?) (eq? sud sudv))
						(error "get-user-data mismatch"))
					(unless (= n (send c find-string s))
						(error "bad find-string result")))
				      (loop (add1 n) (cdr l) (cdr lud))))
			    (unless (and (null? (send c get-string -1))
					 (null? (send c get-string (send c number))))
				    (error "out-of-bounds did not return null"))
			    (unless (= -1 (send c find-string "nada"))
				    (error "bad find-string result for nada"))
			    (for-each
			     (lambda (eis)
			       (let ([e (car eis)]
				     [i (cadr eis)]
				     [s (caddr eis)])
				 (unless (= (send e get-command-int) i)
					 (error "event selection value mismatch"))
				 (unless (or (and (null? s) (null? (send e get-command-string)))
					     (string=? (send e get-command-string) s))
				   (error "selection string mismatch"))
				 (check-callback-event c c e commands #t)))
			     old-list)
			    (printf "content: ~s~n" actual-content)
			    (when multi?
			      (printf "selections: ~s~n" (send c get-selections))))
			  "Check"))
  (instructions p "choice-list-steps.txt")
  (send f show #t))

(define (slider-frame)
  (define f (make-object mred:frame% null "Slider Test"))
  (define p (make-object mred:vertical-panel% f))
  (define old-list null)
  (define commands (list wx:const-event-type-slider-command))
  (define s (make-object mred:slider% p
			 (lambda (sl e)
			   (unless (= (send s get-value) (send e get-selection))
			     (error "slider value mismatch"))
			   (check-callback-event s sl e commands #f))
			 "Slide Me"
			 3 -1 11 -1))
  (define c (make-object mred:button% p
			 (lambda (c e)
			   (for-each
			    (lambda (e)
			      (check-callback-event s s e commands #t))
			    old-list)
			   (printf "All Ok~n"))
			 "Check"))
  (define (simulate v)
    (let ([e (make-object wx:command-event% wx:const-event-type-slider-command)])
      (send e set-command-int v)
      (send e set-event-object s)
      (send s command e)))
  (define p2 (make-object mred:horizontal-panel% p))
  (define p3 (make-object mred:horizontal-panel% p))
  (send p3 stretchable-height #f)
  (make-object mred:button% p2
	       (lambda (c e)
		 (send s set-value (add1 (send s get-value))))
	       "Up")
  (make-object mred:button% p2
	       (lambda (c e)
		 (send s set-value (sub1 (send s get-value))))
	       "Down")
  (make-object mred:button% p2
	       (lambda (c e)
		 (simulate (add1 (send s get-value))))
	       "Simulate Up")
  (make-object mred:button% p2
	       (lambda (c e)
		 (simulate (sub1 (send s get-value))))
	       "Simulate Down")
  (instructions p "slider-steps.txt")
  (send f show #t))

(define (gauge-frame)
  (define f (make-object mred:frame% null "Gauge Test"))
  (define p (make-object mred:vertical-panel% f))
  (define g (make-object mred:gauge% p "Tester" 10))
  (define (move d name)
    (make-object mred:button% p
		 (lambda (c e)
		   (send g set-value (+ d (send g get-value))))
		 name))
  (define (size d name)
    (make-object mred:button% p
		 (lambda (c e)
		   (send g set-range (+ d (send g get-range))))
		 name))
  (move 1 "+")
  (move -1 "-")
  (size 1 "Bigger")
  (size -1 "Smaller")
  (instructions p "gauge-steps.txt")
  (send f show #t))

(define (text-frame mred:text% style)
  (define (handler get-this)
    (lambda (c e)
      (unless (eq? c (get-this))
	      (printf "callback: bad item: ~a~n" c))
      (unless (eq? c (send e get-event-object))
	      (printf "callback: bad item in event: ~a~n" (send e get-event-object)))
      (let ([t (send e get-event-type)])
	(cond
	 [(= t wx:const-event-type-text-command)
	  (printf "Changed: ~a~n" (send e get-command-string))]
	 [(= t wx:const-event-type-text-enter-command)
	  (printf "Return: ~a~n" (send e get-command-string))]
	 [(= t wx:const-event-type-set-focus)
	  (printf "Focus in~n")]
	 [(= t wx:const-event-type-kill-focus)
	  (printf "Focus out~n")]))))

  (define f (make-object mred:frame% null "Text Test"))
  (define p (make-object (class-asi mred:vertical-panel%
			    (public
			     [on-default-action
			      (lambda (v)
				(printf "Panel default action~n"))]))
			 f))
  (define t1 (make-object mred:text% p (handler (lambda () t1)) null "This should just fit!"
			  -1 -1 -1 -1 style))
  (define t2 (make-object mred:text% p (handler (lambda () t2)) "Another" "This too!"
			  -1 -1 -1 -1 style))
  (define junk (send p set-label-position wx:const-vertical))
  (define t3 (make-object mred:text% p (handler (lambda () t3)) "Catch Returns" "And, yes, this!"
			  -1 -1 -1 -1 (+ style wx:const-process-enter)))
  (send t1 stretchable-width #f)
  (send t2 stretchable-width #f)
  (send t3 stretchable-width #f)
  (send f show #t))

(define (canvas-frame flags)
  (define f (make-object mred:frame% null "Canvas Test" -1 -1 -1 250))
  (define p (make-object mred:vertical-panel% f))
  (define c% (class mred:canvas% (name swapped-name p)
		    (inherit clear draw-text draw-line set-clipping-region
			     get-scroll-pos get-scroll-range get-scroll-page
			     get-client-size get-virtual-size)
		    (public
		     [vw 10]
		     [vh 10]
		     [on-paint
		      (lambda ()
			(let ([s (format "V: p: ~s r: ~s g: ~s H: ~s ~s ~s"
					 (get-scroll-pos wx:const-vertical)
					 (get-scroll-range wx:const-vertical)
					 (get-scroll-page wx:const-vertical)
					 (get-scroll-pos wx:const-horizontal)
					 (get-scroll-range wx:const-horizontal)
					 (get-scroll-page wx:const-horizontal))]
			      [w (box 0)][w2 (box 0)]
			      [h (box 0)][h2 (box 0)])
			  (get-client-size w h)
			  (get-virtual-size w2 h2)
			  ; (set-clipping-region 0 0 (unbox w2) (unbox h2))
			  (clear)
			  (draw-text (if (send ck-w get-value) swapped-name name) 3 3)
			  ; (draw-line 3 12 40 12)
			  (draw-text s 3 15)
			  (draw-text (format "client: ~s x ~s  virtual: ~s x ~s" 
					     (unbox w) (unbox h)
					     (unbox w2) (unbox h2))
				     3 27)
			  (draw-line 0 vh vw vh)
			  (draw-line vw 0 vw vh)))]
		     [set-vsize (lambda (w h) (set! vw w) (set! vh h))]
		     [on-scroll
		      (lambda (e) (on-paint))])
		    (sequence
		      (super-init p -1 -1 -1 -1 flags))))
  (define un-name "Unmanaged scroll")
  (define m-name "Automanaged scroll")
  (define c1 (make-object c% un-name m-name p))
  (define c2 (make-object c% m-name un-name p))
  (define (reset-scrolls for-small?)
    (let* ([h? (send ck-h get-value)]
	   [v? (send ck-v get-value)]
	   [small? (send ck-s get-value)]
	   [swap? (send ck-w get-value)])
      (send c1 set-vsize 10 10)
      (send c1 set-scrollbars (if h? 1 -1) (if v? 1 -1) 10 10 3 3 1 1 swap?)
      (send c2 set-vsize (if small? 50 500) (if small? 20 200))
      (send c2 set-scrollbars (if h? 25 -1) (if v? 10 -1) (if small? 2 20) (if small? 2 20) 
	    3 3 1 1 (not swap?))
      (if for-small?
	  ; Specifically refresh the bottom canvas
	  (send c2 refresh)
	  ; Otherwise, we have to specifically refresh the unmanaged canvas
	  (send (if swap? c2 c1) refresh))))
  (define p2 (make-object mred:horizontal-panel% p))
  (define junk (send p2 stretchable-height #f))
  (define ck-v (make-object mred:check-box% p2 (lambda (b e) (reset-scrolls #f)) "Vertical Scroll"))
  (define ck-h (make-object mred:check-box% p2 (lambda (b e) (reset-scrolls #f)) "Horizontal Scroll"))
  (define ck-s (make-object mred:check-box% p2 (lambda (b e) (reset-scrolls #t)) "Small"))
  (define ck-w (make-object mred:check-box% p2 (lambda (b e) (reset-scrolls #f)) "Swap"))
  (define ip (make-object mred:horizontal-panel% p))
  (send ip stretchable-height #f)
  (make-object mred:button% ip
	       (lambda (b e) 
		 (send (send (mred:edit-file (local-path "canvas-steps.txt")) get-edit) lock #t))
	       "Get Instructions")
  (send c1 set-vsize 10 10)
  (send c2 set-vsize 500 200)
  (send f show #t))

;----------------------------------------------------------------------

(define selector (make-object frame% "Test Selector"))
(define ap (make-object vertical-panel% selector))

; Test timers while we're at it. And create the "Instructions" button.
(let ([clockp (make-object horizontal-panel% ap)]
      [selector selector])
  (make-object button% "Get Instructions" clockp
	       (lambda (b e) 
		 (send (send (mred:edit-file (local-path "frame-steps.txt")) get-edit) lock #t)))
  (make-object vertical-panel% clockp) ; filler
  (let ([time (make-object message% "XX:XX:XX" clockp)])
    (make-object
     (class wx:timer% ()
	    (inherit start)
	    (public
	     [notify
	      (lambda ()
		(let* ([now (seconds->date (current-seconds))]
		       [pad (lambda (pc d)
			      (let ([s (number->string d)])
				(if (= 1 (string-length s))
				    (string-append pc s)
				    s)))]
		       [s (format "~a:~a:~a"
				  (pad " " (let ([h (modulo (date-hour now) 12)])
					     (if (zero? h)
						 12
						 h)))
				  (pad "0" (date-minute now))
				  (pad "0" (date-second now)))])
		  (send time set-label s)
		  (when (send selector is-shown?)
			(start 1000 #t))))])
	    (sequence
	      (super-init)
	      (start 1000 #t))))))

(define bp (make-object vertical-panel% ap '(border)))
(define bp1 (make-object horizontal-panel% bp))
(define bp2 (make-object horizontal-pane% bp))
(define mp (make-object vertical-panel% ap '(border)))
(define mp1 (make-object horizontal-panel% mp))
(define mp2 (make-object horizontal-pane% mp))

(send bp1 set-label-position 'vertical)
(send mp1 set-label-position 'vertical)

(make-object button% "Make Menus Frame" ap (lambda (b e) (menu-frame)))
(define bp (make-object horizontal-pane% ap))
(send bp stretchable-width #f)
(make-object button% "Make Button Frame" bp (lambda (b e) (button-frame frame% null)))
(make-object button% "Make Default Button Frame" bp (lambda (b e) (button-frame frame% '(default))))
(make-object button% "Make Button Dialog Box" bp (lambda (b e) (button-frame dialog-box% null)))
(define crp (make-object horizontal-pane% ap))
(send crp stretchable-height #f)
(make-object button% "Make Checkbox Frame" crp (lambda (b e) (checkbox-frame)))
(make-object vertical-pane% crp) ; filler
(make-object button% "Make Radiobox Frame" crp (lambda (b e) (radiobox-frame)))
(define cp (make-object horizontal-pane% ap))
(send cp stretchable-width #f)
(make-object button% "Make Choice Frame" cp (lambda (b e) (choice-or-list-frame #f 0 #f)))
(make-object button% "Make Empty Choice Frame" cp (lambda (b e) (choice-or-list-frame #f 0 #t)))
(define lp (make-object horizontal-pane% ap))
(send lp stretchable-width #f)
(make-object button% "Make List Frame" lp (lambda (b e) (choice-or-list-frame #t wx:const-single #f)))
(make-object button% "Make Empty List Frame" lp (lambda (b e) (choice-or-list-frame #t wx:const-single #t)))
(make-object button% "Make MultiList Frame" lp (lambda (b e) (choice-or-list-frame #t wx:const-multiple #f)))
(make-object button% "Make MultiExtendList Frame" lp (lambda (b e) (choice-or-list-frame #t wx:const-extended #f)))
(define gsp (make-object horizontal-pane% ap))
(send gsp stretchable-height #f)
(make-object button% "Make Gauge Frame" gsp (lambda (b e) (gauge-frame)))
(make-object vertical-pane% gsp) ; filler
(make-object button% "Make Slider Frame" gsp (lambda (b e) (slider-frame)))
(define tp (make-object horizontal-pane% ap))
(send tp stretchable-width #f)
(make-object button% "Make Text Frame" tp (lambda (b e) (text-frame text% 0)))
(make-object button% "Make Multitext Frame" tp (lambda (b e) (text-frame multi-text% 0)))
(define tp2 (make-object horizontal-pane% ap))
(send tp2 stretchable-width #f)
(make-object button% "Make Multitext Frame/HScroll" tp2 (lambda (b e) (text-frame multi-text% '(hscroll))))
(make-object button% "Make Media Multitext Frame/HScroll" tp2 (lambda (b e) (text-frame multi-text% '(hscroll))))

(define cnp (make-object horizontal-pane% ap))
(send cnp stretchable-width #f)
(let ([mkf (lambda (flags name)
	     (make-object button%
			  (format "Make ~aCanvas Frame" name) cnp 
			  (lambda (b e) (canvas-frame flags))))])
  (mkf '(hscroll vscroll) "HV")
  (mkf '(hscroll) "H")
  (mkf '(vscroll) "V")
  (mkf null ""))

(define (choose-next radios)
  (let loop ([l radios])
    (let* ([c (car l)]
	   [rest (cdr l)]
	   [n (send c number)]
	   [v (send c get-selection)])
      (if (< v (sub1 n))
	  (send c set-selection (add1 v))
	  (if (null? rest)
	      (map (lambda (c) (send c set-selection 0)) radios)
	      (begin
		(send c set-selection 0)
		(loop rest)))))))

(define make-next-button
  (lambda (p l)
    (make-object button%
		 "Next Configuration" p
		 (lambda (b e) (choose-next l)))))

(define make-selector-and-runner
  (lambda (p1 p2 radios? size maker)
    (define radio-h-radio
      (if radios?
	  (make-object radio-box% "Radio Box Orientation" '("Vertical" "Horizontal")
		       p1 void)
	  #f))
    (define label-h-radio
      (make-object radio-box% "Label Orientation" '("Vertical" "Horizontal")
		   p1 void))
    (define label-null-radio
      (make-object radio-box% "Optional Labels" '("Use Label" "No Label")
		   p1 void))
    (define stretchy-radio
      (make-object radio-box% "Stretchiness" '("Normal" "All Stretchy")
		   p1 void))
    (define label-font-radio
      (make-object radio-box% "Label Font" '("Normal" "Big")
		    p1 void))
    (define button-font-radio
      (make-object radio-box% "Button Font" '("Normal" "Big")
		    p1 void))
    (define next-button
      (let ([basic-set (list label-h-radio label-null-radio stretchy-radio label-font-radio button-font-radio)])
	(make-next-button p2 
			  (if radios?
			      (cons radio-h-radio basic-set)
			      basic-set))))
    (define go-button
      (make-object button% (format "Make ~a Frame" size) p2
		   (lambda (b e)
		     (maker
		      (if radios?
			  (positive? (send radio-h-radio get-selection))
			  #f)
		      (positive? (send label-h-radio get-selection))
		      (positive? (send label-null-radio get-selection))
		      (positive? (send stretchy-radio get-selection))
		      (positive? (send label-font-radio get-selection))
		      (positive? (send button-font-radio get-selection))))))
    #t))

(make-selector-and-runner bp1 bp2 #t "Big" big-frame)
(make-selector-and-runner mp1 mp2 #f "Medium" med-frame)

(send selector show #t)

; (define e (make-object wx:key-event% wx:const-event-type-char))
; (send e set-key-code 65)
; (send e set-shift-down #t)
