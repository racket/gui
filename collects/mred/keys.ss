
  (unit/sig mred:keymap^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:preferences : mred:preferences^]
	    [mred:exit : mred:exit^]
	    [mred:finder : mred:finder^]
	    [mred:handler : mred:handler^]
	    [mred:find-string : mred:find-string^]
	    [mred:scheme-paren : mred:scheme-paren^]
	    [mred:gui-utils : mred:gui-utils^])
	    
    (mred:debug:printf 'invoke "mred:keymap@")

    ; This is a list of keys that are typed with the SHIFT key, but
    ;  are not normally thought of as shifted. It will have to be
    ;  changed for different keyboards.
    (define shifted-key-list
      '("<" ">" "?" ":" "~" "\""
	    "{" "}"
	    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+"
	    "|"))

    (define keyerr
      (lambda (str)
	(display str (current-error-port))
	(newline (current-error-port))))

    (define (set-keymap-error-handler keymap)
      (send keymap set-error-callback keyerr))

    (define (set-keymap-implied-shifts keymap)
      (map (lambda (k) (send keymap implies-shift k)) 
	   shifted-key-list))

    (define (make-meta-prefix-list key)
      (list (string-append "m:" key)
	    (string-append "c:[;" key)
	    (string-append "ESC;" key)))

    (define send-map-function-meta
      (lambda (keymap key func)
	(for-each (lambda (key)
		    (send keymap map-function key func))
		  (make-meta-prefix-list key))))

    (mred:preferences:set-preference-default 'mred:delete-forward? (not (eq? wx:platform 'unix)))

    ; This installs the standard keyboard mapping
    (define setup-global-keymap
      ; Define some useful keyboard functions
      (let* ([rcs
	      (let ([last-checkin-string ""])
		(mred:preferences:set-preference-default 
		 'rcs-pathname (list "/usr/local/RCS/" "/usr/bin/" "/usr/local/bin/"))
		(lambda (edit event)
		  (let/ec k
		    (let* ([rcs-binaries (list "ci" "co" "rlog")]
			   [rcs-pathname (let loop ([paths (mred:preferences:get-preference 'rcs-pathname)])
					   (cond 
					     [(null? paths) (k (wx:message-box "could not find RCS binaries."))]
					     [else (if (andmap (lambda (b)
								 (file-exists? (build-path (car paths) b)))
							       rcs-binaries)
						       (car paths)
						       (loop (cdr paths)))]))]
			   [filename (send edit get-filename)]
			   [username (wx:get-user-id)])
		      (when (null? filename)
			(k (wx:message-box "no file associated with this edit")))
		      (let-values ([(my-out my-in my-pid my-err)
				    (apply values (process* (build-path rcs-pathname "rlog")
							    "-L" "-R" (string-append "-l" username) filename))])
			(let-values ([(their-out their-in their-pid their-err)
				      (apply values (process* (build-path rcs-pathname "rlog")
							      "-L" "-R" "-l" filename))])
			  (let ([my-lock? (not (eof-object? (read my-out)))]
				[locked? (not (eof-object? (read their-out)))])
			    (for-each close-input-port (list my-out my-err their-out their-err))
			    (for-each close-output-port (list my-in their-in))
			    (cond
			      [(not (system* (build-path rcs-pathname "rlog") "-h" "-q" filename))
			       (system* (build-path rcs-pathname "ci") "-t-" filename)
			       (wx:message-box "Initial Checkin Completed")]
			      [my-lock?
			       (when (send edit modified?)
				 (case (mred:gui-utils:unsaved-warning (send edit get-filename) "Checkin" #t)
				   [(save) (send edit save-file (send edit get-filename) 
						 (send edit get-file-format))]
				   [(cancel) (k (void))]
				   [else (void)]))
			       (let* ([msg (wx:get-text-from-user "Please Enter Log Message"
								  "Check In"
								  last-checkin-string)]
				      [result (system* (build-path rcs-pathname "ci")
						       "-u" (string-append "-m" msg) filename)])
				 (set! last-checkin-string msg)
				 (if result
				     (send edit load-file
					   (send edit get-filename)
					   (send edit get-file-format))
				     (wx:message-box "Checkin Unsucessful")))]
			      [locked? (wx:message-box "Someone else has the lock")]
			      [else
			       (let ([current-dir (current-directory)])
				 (let-values ([(base name _) (split-path filename)])
				   (unless (eq? 'relative base)
				     (current-directory base))
				   (let ([res (system* (build-path rcs-pathname "co")
						       "-q" "-l" name)])
				     (current-directory current-dir)
				     (if res
					 (send edit load-file
					       (send edit get-filename)
					       (send edit get-file-format))
					 (wx:message-box "Checkout Failed")))))]))))))))]
	     
	     [ring-bell
	      (lambda (edit event)
		(let ([c (send edit get-canvas)])
		  (when c
		    (let ([f (let loop ([f c])
			       (if (is-a? f wx:frame%)
				   f
				   (loop (send f get-parent))))])
		      (send f hide-search))))
		(wx:bell))]
	     [save-file-as
	      (lambda (edit event)
		(let ([file (mred:finder:put-file)])
		  (if file
		      (send edit save-file file)))
		#t)]
	     [save-file
	      (lambda (edit event)
		(if (null? (send edit get-filename))
		    (save-file-as edit event)
		    (send edit save-file))
		#t)]
	     [load-file
	      (lambda (edit event)
		(mred:handler:open-file)
		#t)]
	     [find-string
	      (lambda (edit event . extras)
		(let ([x-box (box 0)]
		      [y-box (box 0)]
		      [canvas (send event get-event-object)])
		  (send event position x-box y-box)
		  (send canvas client-to-screen x-box y-box)
		  (mred:find-string:find-string canvas ()
						 (- (unbox x-box) 30)
						 (- (unbox y-box) 30)
						 (cons 'ignore-case extras))
		  #t))]
	     [find-string-reverse
	      (lambda (edit event)
		(find-string edit event 'reverse))]
	     [find-string-replace
	      (lambda (edit event)
		(find-string edit event 'replace))]
	     
	     [toggle-anchor
	      (lambda (edit event)
		(send edit set-anchor
		      (not (send edit get-anchor))))]
	     [center-view-on-line
	      (lambda (edit event)
		(let ([new-mid-line (send edit position-line
					  (send edit get-start-position))]
		      [bt (box 0)]
		      [bb (box 0)])
		  (send edit get-visible-line-range bt bb)
		  (let* ([half (sub1 (quotient (- (unbox bb) (unbox bt)) 2))]
			 [top-pos (send edit line-start-position 
					(max (- new-mid-line half) 0))]
			 [bottom-pos (send edit line-start-position 
					   (min (+ new-mid-line half)
						(send edit position-line 
						      (send edit last-position))))])
		    (send edit scroll-to-position 
			  top-pos
			  #f
			  bottom-pos)))
		#t)]
	     [flash-paren-match
	      (lambda (edit event)
		(send edit on-default-char event)
		(let ([pos (mred:scheme-paren:scheme-backward-match 
			    edit
			    (send edit get-start-position)
			    0)])
		  (when pos
		    (send edit flash-on pos (+ 1 pos))))
		#t)]
	     [collapse-variable-space
	      (lambda (leave-one? edit event)
		(letrec ([find-nonwhite
			  (lambda (pos d)
			    (let ([c (send edit get-character pos)])
			      (cond
			       [(char=? #\newline c) pos]
			       [(char-whitespace? c) 
				(find-nonwhite (+ pos d) d)]
			       [else pos])))])
		  (let ([sel-start (send edit get-start-position)]
			[sel-end (send edit get-end-position)])
		    (if (= sel-start sel-end)
			(let ([start (+ (find-nonwhite (- sel-start 1) -1)
					(if leave-one? 2 1))]
			      [end (find-nonwhite sel-start 1)])
			  (if (< start end)
			      (begin
				(send edit begin-edit-sequence)
				(send edit delete start end)
				(if (and leave-one?
					 (not (char=? #\space
						      (send edit get-character
							    (sub1 start)))))
				    (send edit insert " " (sub1 start) start))
				(send edit set-position start)
				(send edit end-edit-sequence))
			      (if leave-one?
				  (let ([at-start
					 (send edit get-character sel-start)]
					[after-start
					 (send edit get-character 
					       (sub1 sel-start))])
				    (cond
				     [(char-whitespace? at-start)
				      (if (not (char=? at-start #\space))
					  (send edit insert " " sel-start 
						(add1 sel-start)))
				      (send edit set-position (add1 sel-start))]
				     [(char-whitespace? after-start)
				      (if (not (char=? after-start #\space))
					  (send edit insert " " (sub1 sel-start)
						sel-start))]
				     [else (send edit insert " ")])))))))))]

	     [collapse-space
	      (lambda (edit event)
		(collapse-variable-space #t edit event))]

	     [remove-space
	      (lambda (edit event)
		(collapse-variable-space #f edit event))]

	     [collapse-newline
	      (lambda (edit event)
		(letrec ([find-nonwhite
			  (lambda (pos d offset)
			    (call/ec
                              (lambda (escape)
                                (let ([max (if (> offset 0)
					    (send edit last-position)
					    -1)])
			       (let loop ([pos pos])
				 (if (= pos max)
				     (escape pos)
				     (let ([c (send edit get-character 
						    (+ pos offset))])
				       (cond
					[(char=? #\newline c)
					 (loop (+ pos d))
					 (escape pos)]
					[(char-whitespace? c) 
					 (loop (+ pos d))]
					[else pos]))))))))])
		  (let ([sel-start (send edit get-start-position)]
			[sel-end (send edit get-end-position)])
		    (if (= sel-start sel-end)
			(let* ([pos-line
				(send edit position-line sel-start #f)]
			       [pos-line-start
				(send edit line-start-position pos-line)]
			       [pos-line-end
				(send edit line-end-position pos-line)]

			       [whiteline?
				(let loop ([pos pos-line-start])
				  (if (>= pos pos-line-end)
				      #t
				      (and (char-whitespace?
					    (send edit get-character pos))
					   (loop (add1 pos)))))]
			       
			       [start (find-nonwhite pos-line-start -1 -1)]
			       [end (find-nonwhite pos-line-end 1 0)]

			       [start-line 
				(send edit position-line start #f)]
			       [start-line-start
				(send edit line-start-position start-line)]
			       [end-line
				(send edit position-line end #f)]
			       [end-line-start
				(send edit line-start-position (add1 end-line))])
			  (cond
			   [(and whiteline?
				 (= start-line pos-line)
				 (= end-line pos-line))
			    ; Special case: just delete this line
			    (send edit delete pos-line-start (add1 pos-line-end))]
			   [(and whiteline? (< start-line pos-line))
			    ; Can delete before & after
			    (send* edit 
				   (begin-edit-sequence)
				   (delete (add1 pos-line-end) end-line-start)
				   (delete start-line-start pos-line-start)
				   (end-edit-sequence))]
			   [else
			    ; Only delete after
			    (send edit delete (add1 pos-line-end) 
				  end-line-start)]))))))]

	     [open-line
	      (lambda (edit event)
		(let ([sel-start (send edit get-start-position)]
		      [sel-end (send edit get-end-position)])
		  (if (= sel-start sel-end)
		      (send* edit 
			     (insert #\newline)
			     (set-position sel-start)))))]

	     [transpose-chars
	      (lambda (edit event)
		(let ([sel-start (send edit get-start-position)]
		      [sel-end (send edit get-end-position)])
		  (when (= sel-start sel-end)
		    (let ([sel-start
			   (if (= sel-start
				  (send edit line-end-position
					(send edit position-line sel-start)))
			       (sub1 sel-start)
			       sel-start)])
		      (let ([s (send edit get-text
				     sel-start (add1 sel-start))])
			(send* edit
			       (begin-edit-sequence)
			       (delete sel-start (add1 sel-start))
			       (insert s (- sel-start 1))
			       (set-position (add1 sel-start))
			       (end-edit-sequence)))))))]

	     [transpose-words
	      (lambda (edit event)
		(let ([sel-start (send edit get-start-position)]
		      [sel-end (send edit get-end-position)])
		  (when (= sel-start sel-end)
		    (let ([word-1-start (box sel-start)])
		      (send edit find-wordbreak word-1-start ()
			    wx:const-break-for-caret)
		      (let ([word-1-end (box (unbox word-1-start))])
			(send edit find-wordbreak () word-1-end
			      wx:const-break-for-caret)
			(let ([word-2-end (box (unbox word-1-end))])
			  (send edit find-wordbreak () word-2-end
				wx:const-break-for-caret)
			  (let ([word-2-start (box (unbox word-2-end))])
			    (send edit find-wordbreak word-2-start ()
				  wx:const-break-for-caret)
			    (let ([text-1 (send edit get-text
						(unbox word-1-start)
						(unbox word-1-end))]
				  [text-2 (send edit get-text
						(unbox word-2-start)
						(unbox word-2-end))])
			      (send* edit
				     (begin-edit-sequence)
				     (insert text-1 
					     (unbox word-2-start)
					     (unbox word-2-end))
				     (insert text-2 
					     (unbox word-1-start)
					     (unbox word-1-end))
				     (set-position (unbox word-2-end))
				     (end-edit-sequence))))))))))]

	     [capitalize-it
	      (lambda (edit all? char-case char-case2)
		(let ([sel-start (send edit get-start-position)]
		      [sel-end (send edit get-end-position)]
		      [real-end (send edit last-position)])
		  (when (= sel-start sel-end)
		    (let ([end-box (box sel-start)])
		      (send edit find-wordbreak () end-box 
			    wx:const-break-for-caret)
		      (dynamic-wind
		       (lambda ()
			 (send edit begin-edit-sequence))
		       (lambda ()
			 (let loop ([pos sel-start][char-case char-case])
			   (if (< pos real-end)
			       (let ([c (send edit get-character pos)])
				 (if (char-alphabetic? c)
				     (begin
				       (send edit insert 
					     (list->string
					      (list (char-case c)))
					     pos (add1 pos))
				       (if (and all? (< (add1 pos) 
							(unbox end-box)))
					   (loop (add1 pos) char-case2)))
				     (loop (add1 pos) char-case))))))
		       (lambda ()
			 (send edit end-edit-sequence)))
		      (send edit set-position (unbox end-box))))))]

	     [capitalize-word
	      (lambda (edit event)
		(capitalize-it edit #t char-upcase char-downcase))]
	     [upcase-word
	      (lambda (edit event)
		(capitalize-it edit #t char-upcase char-upcase))]
	     [downcase-word
	      (lambda (edit event)
		(capitalize-it edit #t char-downcase char-downcase))]

	     [kill-word
	      (lambda (edit event)
		(let ([sel-start (send edit get-start-position)]
		      [sel-end (send edit get-end-position)])
		  (let ([end-box (box sel-end)])
		    (send edit find-wordbreak () end-box 
			  wx:const-break-for-caret)
		    (send edit kill 0 sel-start (unbox end-box)))))]

	     [backward-kill-word
	      (lambda (edit event)
		(let ([sel-start (send edit get-start-position)]
		      [sel-end (send edit get-end-position)])
		  (let ([start-box (box sel-start)])
		    (send edit find-wordbreak start-box ()
			  wx:const-break-for-caret)
		    (send edit kill 0 (unbox start-box) sel-end))))]

	     [region-click
	      (lambda (edit event f)
		(when (send event button-down?)
		  (let ([x-box (box (send event get-x))]
			[y-box (box (send event get-y))]
			[eol-box (box #f)])
		    (send edit global-to-local x-box y-box)
		    (let ([click-pos (send edit find-position 
					   (unbox x-box)
					   (unbox y-box)
					   eol-box)]
			  [start-pos (send edit get-start-position)]
			  [end-pos (send edit get-end-position)])
		      (let ([eol (unbox eol-box)])
			(if (< start-pos click-pos)
			    (f click-pos eol start-pos click-pos)
			    (f click-pos eol click-pos end-pos)))))))]
	     [copy-click-region
	      (lambda (edit event)
		(region-click edit event
			      (lambda (click eol start end)
				(send edit flash-on start end)
				(send edit copy #f 0 start end))))]
	     [cut-click-region
	      (lambda (edit event)
		(region-click edit event
			      (lambda (click eol start end)
				(send edit cut #f 0 start end))))]
	     [paste-click-region
	      (lambda (edit event)
		(region-click edit event
			      (lambda (click eol start end)
				(send edit set-position click)
				(send edit paste 0 click))))]

	     [mouse-copy-clipboard
	      (lambda (edit event)
		(send edit copy #f (send event get-time-stamp)))]

	     [mouse-paste-clipboard
	      (lambda (edit event)
		(send edit paste (send event get-time-stamp)))]

	     [mouse-cut-clipboard
	      (lambda (edit event)
		(send edit cut #f (send event get-time-stamp)))]

	     [select-click-word
	      (lambda (edit event)
		(region-click edit event
			      (lambda (click eol start end)
				(let ([start-box (box click)]
				      [end-box (box click)])
				  (send edit find-wordbreak 
					start-box
					end-box
					wx:const-break-for-selection)
				  (send edit set-position
					(unbox start-box)
					(unbox end-box))))))]
	     [select-click-line
	      (lambda (edit event)
		(region-click edit event
			      (lambda (click eol start end)
				(let* ([line (send edit position-line 
						   click eol)]
				       [start (send edit line-start-position
						    line #f)]
				       [end (send edit line-end-position
						  line #f)])
				  (send edit set-position start end)))))]

	     [goto-line
	      (lambda (edit event)
		(let ([num-str (wx:get-text-from-user "Goto Line:" "Goto Line")])
		  (if (string? num-str)
		      (let ([line-num (string->number num-str)])
			(if line-num
			    (let ([pos (send edit line-start-position 
					     (sub1 line-num))])
			      (send edit set-position pos))))))
		#t)]
	     [goto-position
	      (lambda (edit event)
		(let ([num-str (wx:get-text-from-user "Goto Position:" 
						      "Goto Position")])
		  (if (string? num-str)
		      (let ([pos (string->number num-str)])
			(if pos
			    (send edit set-position (sub1 pos))))))
		#t)]
	     [repeater
	      (lambda (n edit)
		(let* ([km (send edit get-keymap)]
		       [done
			(lambda ()
			  (send km set-break-sequence-callback void)
			  (send km remove-grab-key-function))])
		  (send km set-grab-key-function
			(lambda (name local-km edit event)
			  (if (null? name)
			      (let ([k (send event get-key-code)])
				(if (<= (char->integer #\0) k (char->integer #\9))
				    (set! n (+ (* n 10) (- k (char->integer #\0))))
				    (begin
				      (done)
				      (dynamic-wind
				       (lambda ()
					 (send edit begin-edit-sequence))
				       (lambda ()
					 (let loop ([n n])
					   (unless (zero? n)
					     (send edit on-char event)
					     (loop (sub1 n)))))
				       (lambda ()
					 (send edit end-edit-sequence))))))
			      (begin
				(done)
				(dynamic-wind
				 (lambda ()
				   (send edit begin-edit-sequence))
				 (lambda ()
				   (let loop ([n n])
				     (unless (zero? n)
				       (send local-km call-function name edit event)
				       (loop (sub1 n)))))
				 (lambda ()
				   (send edit end-edit-sequence)))))			       
			  #t))
		  (send km set-break-sequence-callback done)
		  #t))]
	     [make-make-repeater
	      (lambda (n)
		(lambda (edit event)
		  (repeater n edit)))]
	     [current-macro '()] 
	     [building-macro #f] [build-macro-km #f] [build-protect? #f]
	     [do-macro
	      (lambda (edit event)
		; If c:x;e during record, copy the old macro
		(when building-macro
		  (set! building-macro (append (reverse current-macro) 
					       (cdr building-macro))))
		(let ([bm building-macro]
		      [km (send edit get-keymap)])
		  (dynamic-wind
		   (lambda ()
		     (set! building-macro #f)
		     (send edit begin-edit-sequence))
		   (lambda ()
		     (let/ec escape
		       (for-each
			(lambda (f)
			  (let ([name (car f)]
				[event (cdr f)])
			    (if (null? name)
				(send edit on-char event)
				(if (not (send km call-function 
					       name edit event #t))
				    (escape #t)))))
			current-macro)))
		   (lambda ()
		     (send edit end-edit-sequence)
		     (set! building-macro bm))))
		#t)]
	     [start-macro
	      (lambda (edit event)
		(if building-macro
		    (send build-macro-km break-sequence)
		    (letrec* ([km (send edit get-keymap)]
			      [done
			       (lambda ()
				 (if build-protect?
				     (send km set-break-sequence-callback done)
				     (begin
				       (set! building-macro #f)
				       (send km set-break-sequence-callback void)
				       (send km remove-grab-key-function))))])
		      (set! building-macro '())
		      (set! build-macro-km km)
		      (send km set-grab-key-function
			    (lambda (name local-km edit event)
			      (dynamic-wind
			       (lambda ()
				 (set! build-protect? #t))
			       (lambda ()
				 (if (null? name)
				     (send edit on-default-char event)
				     (send local-km call-function name edit event)))
			       (lambda ()
				 (set! build-protect? #f)))
			      (when building-macro
				(set! building-macro 
				      (cons (cons name 
						  (duplicate-key-event event))
					    building-macro)))
			      #t))
		      (send km set-break-sequence-callback done)))
		#t)]
	     [end-macro
	      (lambda (edit event)
		(when building-macro
		  (set! current-macro (reverse building-macro))
		  (set! build-protect? #f)		    
		  (send build-macro-km break-sequence))
		#t)]
	     [delete-key
	      (lambda (edit event)
		(let ([kmap (send edit get-keymap)])
		  (send kmap call-function
			(if (mred:preferences:get-preference 'mred:delete-forward?)
			    "delete-next-character"
			    "delete-previous-character")
			edit event #t)))]
	     [toggle-overwrite
	      (lambda (edit event)
		(send edit set-overwrite-mode
		      (not (send edit get-overwrite-mode))))])
	(lambda (kmap)
	  ; Redirect keymapping error messages to stderr
	  (send kmap set-error-callback keyerr)
	  ; Set the implied shifting map
	  (map (lambda (k) (send kmap implies-shift k)) shifted-key-list)
	  (let* ([map (lambda (key func) 
			(send kmap map-function key func))]
		 [map-meta (lambda (key func)
			     (send-map-function-meta kmap key func))]
		 [add (lambda (name func)
			(send kmap add-key-function name func))]
		 [add-m (lambda (name func)
			  (send kmap add-mouse-function name func))])
	    
	    ; Standards
	    (wx:add-media-buffer-functions kmap)
	    (wx:add-media-editor-functions kmap)
	    (wx:add-media-pasteboard-functions kmap)

	    ; Map names to keyboard functions
	    (add "toggle-overwrite" toggle-overwrite)

	    (add "rcs" rcs)

	    (add "exit" (lambda (edit event)
			  (let ([frame (send edit get-frame)])
			    (if frame
				((ivar frame file-menu:quit))
				(wx:bell)))))

	    (add "ring-bell" ring-bell)

	    (add "save-file" save-file)
	    (add "save-file-as" save-file-as)
	    (add "load-file" load-file)

	    (add "find-string" find-string)
	    (add "find-string-reverse" find-string-reverse)
	    (add "find-string-replace" find-string-replace)

	    (add "flash-paren-match" flash-paren-match)

	    (add "toggle-anchor" toggle-anchor)
	    (add "center-view-on-line" center-view-on-line)
	    (add "collapse-space" collapse-space)
	    (add "remove-space" remove-space)
	    (add "collapse-newline" collapse-newline)
	    (add "open-line" open-line)
	    (add "transpose-chars" transpose-chars)
	    (add "transpose-words" transpose-words)
	    (add "capitalize-word" capitalize-word)
	    (add "upcase-word" upcase-word)
	    (add "downcase-word" downcase-word)
	    (add "kill-word" kill-word)
	    (add "backward-kill-word" backward-kill-word)

	    (let loop ([n 9])
	      (unless (negative? n)
		(let ([s (number->string n)])
		  (add (string-append "command-repeat-" s)
		       (make-make-repeater n))
		  (loop (sub1 n)))))

	    (add "do-saved-macro" do-macro)
	    (add "start-macro-record" start-macro)
	    (add "end-macro-record" end-macro)

	    (add-m "copy-clipboard" mouse-copy-clipboard)
	    (add-m "cut-clipboard" mouse-cut-clipboard)
	    (add-m "paste-clipboard" mouse-paste-clipboard)
	    (add-m "copy-click-region" copy-click-region)
	    (add-m "cut-click-region" cut-click-region)
	    (add-m "paste-click-region" paste-click-region)
	    (add-m "select-click-word" select-click-word)
	    (add-m "select-click-line" select-click-line)

	    (add "goto-line" goto-line)
	    (add "goto-position" goto-position)

	    (add "delete-key" delete-key)

	    ; Map keys to functions

	    ; this is not for export -- too much chance it's wrong
	    ; outside of Rice.
	    (when (eq? wx:platform 'unix)
	      '(map "c:x;c:q" "rcs"))

	    (map "c:g" "ring-bell")
	    (map-meta "c:g" "ring-bell")
	    (map "c:x;c:g" "ring-bell")
	    (map "c:c;c:g" "ring-bell")

	    (map ")" "flash-paren-match")
	    (map "]" "flash-paren-match")
	    (map "}" "flash-paren-match")
	    (map "\"" "flash-paren-match")
	    
	    (map "c:p" "previous-line")
	    (map "up" "previous-line")
	    (map "s:c:p" "select-up")
	    (map "s:up" "select-up")

	    (map "c:n" "next-line")
	    (map "down" "next-line")
	    (map "s:c:n" "select-down")
	    (map "s:down" "select-down")

	    (map "c:e" "end-of-line")
	    (map "d:RIGHT" "end-of-line")
	    (map "m:RIGHT" "end-of-line")
	    (map "END" "end-of-line")
	    (map "d:s:RIGHT" "select-to-end-of-line")
	    (map "m:s:RIGHT" "select-to-end-of-line")
	    (map "s:END" "select-to-end-of-line")
	    (map "s:c:e" "select-to-end-of-line")

	    (map "c:a" "beginning-of-line")
	    (map "d:LEFT" "beginning-of-line")
	    (map "m:LEFT" "beginning-of-line")
	    (map "HOME" "beginning-of-line")
	    (map "d:s:LEFT" "select-to-beginning-of-line")
	    (map "m:s:LEFT" "select-to-beginning-of-line")
	    (map "s:HOME" "select-to-beginning-of-line")
	    (map "s:c:a" "select-to-beginning-of-line")

	    (map "c:f" "forward-character")
	    (map "right" "forward-character")
	    (map "s:c:f" "forward-select")
	    (map "s:right" "forward-select")

	    (map "c:b" "backward-character")
	    (map "left" "backward-character")
	    (map "s:c:b" "backward-select")
	    (map "s:left" "backward-select")

	    (map-meta "f" "forward-word")
	    (map "a:RIGHT" "forward-word")
	    (map "c:RIGHT" "forward-word")
	    (map-meta "s:f" "forward-select-word")
	    (map "a:s:RIGHT" "forward-select-word")
	    (map "c:s:RIGHT" "forward-select-word")

	    (map-meta "b" "backward-word")
	    (map "a:LEFT" "backward-word")
	    (map "c:left" "backward-word")
	    (map-meta "s:b" "backward-select-word")
	    (map "a:s:LEFT" "backward-select-word")
	    (map "c:s:left" "backward-select-word")

	    (map-meta "<" "beginning-of-file")
	    (map "d:UP" "beginning-of-file")
	    (map "c:HOME" "beginning-of-file")
	    (map "s:c:home" "select-to-beginning-of-file")
	    (map "s:d:up" "select-to-beginning-of-file")

	    (map-meta ">" "end-of-file")
	    (map "d:DOWN" "end-of-file")
	    (map "c:end" "end-of-file")
	    (map "s:c:end" "select-to-end-of-file")
	    (map "s:d:down" "select-to-end-of-file")

	    (map "c:v" "next-page")
	    (map "a:DOWN" "next-page")
	    (map "pagedown" "next-page")
	    (map "c:DOWN" "next-page")
	    (map "s:c:v" "select-page-down")
	    (map "a:s:DOWN" "select-page-down")
	    (map "s:pagedown" "select-page-down")
	    (map "s:c:DOWN" "select-page-down")

	    (map-meta "v" "previous-page")
	    (map "a:up" "previous-page")
	    (map "pageup" "previous-page")
	    (map "c:up" "previous-page")
	    (map-meta "s:v" "select-page-up")
	    (map "s:a:up" "select-page-up")
	    (map "s:pageup" "select-page-up")
	    (map "s:c:up" "select-page-up")

	    (map "c:h" "delete-previous-character")
	    (map "c:d" "delete-next-character")
	    (map "del" "delete-key")

	    (map-meta "d" "kill-word")
	    (map-meta "del" "backward-kill-word")
	    (map-meta "c" "capitalize-word")
	    (map-meta "u" "upcase-word")
	    (map-meta "l" "downcase-word")

	    (map "c:l" "center-view-on-line")
	    
	    (map "c:k" "delete-to-end-of-line")
	    (map "c:y" "paste-clipboard")
	    (map-meta "y" "paste-next")
	    (map "a:v" "paste-clipboard")
	    (map "d:v" "paste-clipboard")
	    (map "c:_" "undo")
	    (map "c:+" "redo")
	    (map "a:z" "undo")
	    (map "d:z" "undo")
	    (map "c:x;u" "undo")
	    (map "c:w" "cut-clipboard")
	    (map "a:x" "cut-clipboard")
	    (map "d:x" "cut-clipboard")
	    (map-meta "w" "copy-clipboard")
	    (map "a:c" "copy-clipboard")
	    (map "d:c" "copy-clipboard")

	    (map "c:x;c:s" "save-file")
	    (map "d:s" "save-file")
	    (map "c:x;c:w" "save-file-as")
	    (map "c:x;c:f" "load-file")

	    (map "c:s" "find-string")
	    (map "c:r" "find-string-reverse")
	    (map-meta "%" "find-string-replace")

	    (map-meta "space" "collapse-space")
	    (map-meta "\\" "remove-space")
	    (map "c:x;c:o" "collapse-newline")
	    (map "c:o" "open-line")
	    (map "c:t" "transpose-chars")
	    (map-meta "t" "transpose-words")

	    (map "c:space" "toggle-anchor")

	    (map "insert" "toggle-overwrite")
	    (map-meta "o" "toggle-overwrite")

	    (map-meta "g" "goto-line")
	    (map-meta "p" "goto-position")

	    (map "c:u" "command-repeat-0")
	    (let loop ([n 9])
	      (unless (negative? n)
		(let ([s (number->string n)])
		  (map-meta s (string-append "command-repeat-" s))
		  (loop (sub1 n)))))

	    (map "c:x;e" "do-saved-macro")
	    (map "c:x;(" "start-macro-record")
	    (map "c:x;)" "end-macro-record")

	    (map "leftbuttontriple" "select-click-line")
	    (map "leftbuttondouble" "select-click-word")

	    (map "c:x;c:c" "exit")

	    (map "rightbutton" "copy-click-region")
	    (map "rightbuttondouble" "cut-click-region")
	    (map "middlebutton" "paste-click-region")
	    (map "c:rightbutton" "copy-clipboard")))))

    (define global-keymap (make-object wx:keymap%))
					     
    (setup-global-keymap global-keymap))
