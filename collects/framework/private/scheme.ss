;; originally by Dan Grossman
;; 6/30/95

(module scheme mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   "sig"
	   "../macro.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "list.ss")
	   (lib "thread.ss")
	   (lib "etc.ss"))

  (provide scheme@)

  (define scheme@
(unit/sig framework:scheme^
  (import mred^
	  [preferences : framework:preferences^]
	  [match-cache : framework:match-cache^]
	  [paren : framework:paren^] 
	  [scheme-paren : framework:scheme-paren^]
	  [icon : framework:icon^]
	  [keymap : framework:keymap^]
	  [text : framework:text^]
	  [frame : framework:frame^])
  
  (rename [-text% text%]
	  [-text<%> text<%>])

  (define -text<%>
    (interface ()
      highlight-parens
      get-limit
      balance-quotes
      balance-parens
      tabify-on-return?
      tabify
      tabify-selection
      tabify-all
      insert-return
      comment-out-selection
      uncomment-selection
      get-forward-sexp
      remove-sexp
      forward-sexp
      flash-forward-sexp
      get-backward-sexp
      flash-backward-sexp
      backward-sexp
      find-up-sexp
      up-sexp
      find-down-sexp
      down-sexp
      remove-parens-forward

      select-forward-sexp
      select-backward-sexp
      select-up-sexp
      select-down-sexp
      transpose-sexp
      get-tab-size
      set-tab-size))

  (define init-wordbreak-map
    (lambda (map)
      (let ([v (send map get-map #\-)])
	(send map set-map 
	      #\-
	      '(line)))))
  (define wordbreak-map (make-object editor-wordbreak-map%))
  (define (get-wordbreak-map) wordbreak-map)
  (init-wordbreak-map wordbreak-map)

  (define style-list (make-object style-list%))
  (define (get-style-list) style-list)
  (define delta
    (let ([delta (make-object style-delta% 'change-normal)])
      (send delta set-delta 'change-family 'modern)
      delta))
  (let ([style (send style-list find-named-style "Standard")])
    (if style
	(send style set-delta delta)
	(send style-list new-named-style "Standard"
	      (send style-list find-or-create-style
		    (send style-list find-named-style "Basic")
		    delta))))

  (define text-mixin 
    (mixin (text:basic<%>) (-text<%>) args
      (inherit begin-edit-sequence
	       delete
	       end-edit-sequence
	       local-edit-sequence?
	       find-string
	       get-character
	       get-keymap
	       get-text
	       get-start-position
	       get-end-position
	       flash-on
	       highlight-range
	       insert
	       kill
	       last-position
	       paragraph-start-position
	       paragraph-end-position
	       position-line
	       position-paragraph
	       set-keymap
	       set-load-overwrites-styles
	       set-position
	       set-wordbreak-map
	       set-tabs
	       set-style-list
	       set-styles-fixed)
      (rename [super-on-char on-char])

      (private
	[in-single-line-comment?
	 (lambda (position)
	   (let ([line (position-line position)])
	     (ormap
	      (lambda (comment-start)
		(let loop ([f (find-string comment-start 'backward position)])
		  (if f
		      (cond
                       [(= (position-line f) line)
                        (let ([f-1 (- f 2)]) ;; -1 to go back one, -1 to be before char
                          (cond
			   [(< f-1 0)
			    #t]
			   [(and (= (position-line f-1) line)
				 (not (char=? (get-character f-1) #\\ )))
			    #t]
			   [else 
			    (loop (find-string comment-start 'backward f-1))]))]
                       [else 
                        #f])
                      #f)))
	      (scheme-paren:get-comments))))])
      (private
	[remove-indents-callback
	 (preferences:add-callback
	  'framework:tabify
	  (lambda (p value)
	    (set! indents value)))])
      (private
	[indents (preferences:get 'framework:tabify)]
	[backward-cache (make-object match-cache:%)]
	[forward-cache (make-object match-cache:%)])
      
      (private
	[in-highlight-parens? #f]
	[delay-highlight? (lambda () (local-edit-sequence?))])
			    

      (inherit get-styles-fixed)
      (rename [super-on-focus on-focus]
	      [super-after-change-style after-change-style]
	      [super-after-edit-sequence after-edit-sequence]
	      [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-after-set-size-constraint after-set-size-constraint]
	      [super-after-set-position after-set-position])
      (inherit has-focus? find-snip split-snip)
      (override
       [on-focus
	(lambda (on?)
	  (super-on-focus on?)
	  (highlight-parens (not on?)))]
       [after-change-style
	(lambda (start len)
	  (unless (delay-highlight?)
	    (unless (get-styles-fixed)
	      (when (has-focus?)
		(highlight-parens))))
	  (super-after-change-style start len))]
       [after-edit-sequence
	(lambda ()
	  (super-after-edit-sequence)
	  (unless (delay-highlight?)
	    (when (has-focus?)
	      (unless in-highlight-parens?
		(highlight-parens)))))]
       [after-insert
	(lambda (start size)
	  (send backward-cache invalidate start)
	  (send forward-cache forward-invalidate start size)
	  (unless (delay-highlight?)
	    (when (has-focus?)
	      (highlight-parens)))
	  (super-after-insert start size))]
       [after-delete
	(lambda (start size)
	  (super-after-delete start size)
	  (send backward-cache invalidate start)
	  (send forward-cache forward-invalidate (+ start size) (- size))
	  (unless (delay-highlight?)
	    (when (has-focus?)
	      (highlight-parens))))]
       [after-set-size-constraint
	(lambda ()
	  (unless (delay-highlight?)
	    (when (has-focus?)
	      (highlight-parens)))
	  (super-after-set-size-constraint))]
       [after-set-position 
	(lambda ()
	  (unless (delay-highlight?)
	    (when (has-focus?)
	      (highlight-parens)))
	  (super-after-set-position))])

      (private
	[highlight-parens? (preferences:get 'framework:highlight-parens)]
	[remove-paren-callback (preferences:add-callback
				'framework:highlight-parens 
				(lambda (p value)
				  (set! highlight-parens? value)))]

	[find-enclosing-paren
	 (lambda (pos)
	   (let loop ([pos pos])
	     (let ([paren-pos 
		    (let loop ([pairs (scheme-paren:get-paren-pairs)]
			       [curr-max #f])
		      (cond
		       [(null? pairs) curr-max]
		       [else (let* ([pair (car pairs)]
				    [fnd (find-string (car pair) 'backward pos 'eof #f)])
			       (if (and fnd curr-max)
				   (loop (cdr pairs)
					 (max fnd curr-max))
				   (loop (cdr pairs)
					 (or fnd curr-max))))]))])
	       (cond
		 [(not paren-pos) #f]
		 [else
		  (let ([semi-pos (find-string ";" 'backward paren-pos)])
		    (cond
		      [(or (not semi-pos)
			   (< semi-pos (paragraph-start-position
					(position-paragraph paren-pos))))
		       paren-pos]
		      [else (loop (- semi-pos 1))]))]))))])
      (public
	[highlight-parens
	 (let* ([clear-old-locations void]
		[old-gray-level 192]
		[gray-level (if (eq? (system-type) 'windows)
				(* 3/4 256)
				(- (* 7/8 256) 1))]
		[match-color (make-object color% gray-level gray-level gray-level)]
                [mismatch-color (make-object color% "PINK")])
	   (opt-lambda ([just-clear? #f])
	     (when highlight-parens?
	       (set! in-highlight-parens? #t)
	       (begin-edit-sequence)
	       (clear-old-locations)
	       (set! clear-old-locations void)
	       (unless just-clear?
		 (let* ([here (get-start-position)]
			[there (get-end-position)]
			[slash?
			 (lambda (before after)
			   (and (>= before 0)
				(>= after 0)
				(let ([text (get-text before after)])
				  (and (string? text)
				       (>= (string-length text) 1)
				       (char=? #\\ (string-ref text 0))))))]
			[is-paren?
			 (lambda (f)
			   (lambda (char)
			     (ormap (lambda (x) (char=? char (string-ref (f x) 0)))
				    (scheme-paren:get-paren-pairs))))]
			[is-left-paren? (is-paren? car)]
			[is-right-paren? (is-paren? cdr)])
		   (when (= here there)

		     ;; before, after : (list number number boolean) 
		     ;;  numbers indicate the range to highlight
		     ;;  boolean indicates if it is an errorneous highlight
		     (let ([before
			    (cond
			     [(and (> here 0)
				   (is-right-paren? (get-character (sub1 here)))
				   (not (in-single-line-comment? here)))
			      (cond
			       [(slash? (- here 2) (- here 1)) #f]
			       [(scheme-paren:backward-match
				 this here (get-limit here)
				 backward-cache)
				=>
				(lambda (end-pos) 
				  (list end-pos here #f))]
			       [else (list (- here 1) here #t)])]
			     [else #f])]
			   [after
			    (cond
			     [(and (is-left-paren? (get-character here))
				   (not (in-single-line-comment? here)))
			      (cond
			       [(slash? (- here 1) here) #f]
			       [(scheme-paren:forward-match
				 this here (last-position)
				 forward-cache)
				=>
				(lambda (end-pos)
				  (list here end-pos #f))]
			       [else (list here (+ here 1) #t)])]
			     [else #f])]
			   [handle-single
			    (lambda (single)
			      (let* ([left (first single)]
				     [right (second single)]
				     [error? (third single)]
				     [off (highlight-range 
					   left
					   right
					   (if error? mismatch-color match-color)
					   (icon:get-paren-highlight-bitmap)
					   (= there here left))])
				(set! clear-old-locations
				      (let ([old clear-old-locations])
					(lambda ()
					  (old)
					  (off))))))])
		       
		       (cond
			[(and after before)
			 (handle-single after)
			 (handle-single before)]
			[after (handle-single after)]
			[before (handle-single before)]
			[else (void)])))))
	       (end-edit-sequence)
	       (set! in-highlight-parens? #f))))]
	
	[get-limit (lambda (pos) 0)]
	
	[balance-quotes
	 (lambda (key)
	   (let* ([char (send key get-key-code)]) ;; must be a character because of the mapping setup
		                                  ;; this function is only bound to ascii-returning keys
	     (insert char)
	     (let* ([start-pos (get-start-position)]
		    [limit (get-limit start-pos)]
		    [match (scheme-paren:backward-match
			    this start-pos limit backward-cache)])
	       (when match
		 (flash-on match (add1 match))))))]
	[balance-parens
	 (let-struct string/pos (string pos)
	   (lambda (key-event)
	     (letrec ([char (send key-event get-key-code)] ;; must be a character. See above.
		      [here (get-start-position)]
		      [limit (get-limit here)]
		      [paren-match? (preferences:get 'framework:paren-match)]
		      [fixup-parens? (preferences:get 'framework:fixup-parens)]
		      [find-match
		       (lambda (pos)
			 (let loop ([parens (scheme-paren:get-paren-pairs)])
			   (cond
			     [(null? parens) #f]
			     [else (let* ([paren (car parens)]
					  [left (car paren)]
					  [right (cdr paren)])
				     (if (string=? left (get-text pos (+ pos (string-length left))))
					 right
					 (loop (cdr parens))))])))])
	       (cond
		 [(in-single-line-comment? here)
		  (insert char)]
		 [(and (not (= 0 here))
		       (char=? (string-ref (get-text (- here 1) here) 0) #\\))
		  (insert char)]
		 [(or paren-match? fixup-parens?)
		  (let* ([end-pos (scheme-paren:backward-containing-sexp 
				   this here limit
				   backward-cache)])
		    (cond
		      [end-pos
		       (let* ([left-paren-pos (find-enclosing-paren end-pos)]
			      [match (and left-paren-pos
					  (find-match left-paren-pos))])
			 (cond
			   [match
			     (insert (if fixup-parens? match char))
			     (when paren-match?
			       (flash-on
				left-paren-pos
				(+ left-paren-pos (string-length match))))]
			   [else
			    (insert char)]))]
		      [else (insert char)]))]
		 [else (insert char)])
	       #t)))]
	[tabify-on-return? (lambda () #t)]
	[tabify    
	 (opt-lambda ([pos (get-start-position)])
	   (let* ([last-pos (last-position)]
		  [para (position-paragraph pos)]
		  [okay (> para 0)]
		  [end (if okay (paragraph-start-position para) 0)]
		  [limit (get-limit pos)]
		  [contains 
		   (if okay
		       (scheme-paren:backward-containing-sexp 
			this end limit backward-cache)
		       #f)]
		  [contain-para (and contains
				     (position-paragraph contains))]
		  [last 
		   (if contains
		       (scheme-paren:backward-match this end limit backward-cache)
		       #f)]
		  [last-para (and last
				  (position-paragraph last))])
	     (letrec	
		 ([find-offset
		   (lambda (pos)
		     (let loop ([p pos][o 0])
		       (let ([c (get-character p)])
			 (cond
			   [(char=? c #\tab)
			    (loop (add1 p) (+ o (- 8 (modulo o 8))))]
			   [(char=? c #\newline)
			    (cons o p)]
			   [(char-whitespace? c)
			    (loop (add1 p) (add1 o))]
			   [else
			    (cons o p)]))))]
		  [visual-offset
		   (lambda (pos)
		     (let loop ([p (sub1 pos)])
		       (if (= p -1)
			   0
			   (let ([c (get-character p)])
			     (cond
			      [(char=? c #\null) 0]
			      [(char=? c #\tab)
			       (let ([o (loop (sub1 p))])
				 (+ o (- 8 (modulo o 8))))]
			      [(char=? c #\newline) 0]
			      [else (add1 (loop (sub1 p)))])))))]
		  [do-indent
		   (lambda (amt)
		     (let* ([pos-start end]
			    [curr-offset (find-offset pos-start)])
		       (unless (= amt (car curr-offset))
			 (delete pos-start (cdr curr-offset))
			 (insert
			  (make-string amt #\space)
			  pos-start))))]
		  [id-walker
		   (lambda (string)
		     (let ([last (string-length string)])
		       (let loop ([index 0])
			 (if (= index last)
			     last
			     (let ([current (string-ref string index)])
			       (if (or (char-alphabetic? current)
				       (char-numeric? current))
				   (loop (add1 index))
				   (case current
				     [(#\# 
                                       #\+ #\- #\. #\* #\/ #\< #\= #\> #\! #\? #\:
                                       #\$ #\% #\_ #\& #\^ #\~)
				      (loop (add1 index))]
				     [else index])))))))]
		  [get-proc
		   (lambda ()
		     (let* ([text (get-text contains (paragraph-end-position contain-para))])
		       (hash-table-get indents
				       (string->symbol (substring text 0 (id-walker text)))
				       (lambda () 'other))))]
		  [procedure-indent
		   (lambda ()
		     (case (get-proc)
		       [(define) 1]
		       [(begin) 1]
		       [(lambda) 3]
		       [else 0]))]
		  [special-check
		   (lambda ()
		     (let* ([proc-name (get-proc)])
		       (or (eq? proc-name 'define)
			   (eq? proc-name 'lambda))))]
		  [indent-first-arg
		   (lambda (start)
		     (car (find-offset start)))])
	       (when (and okay
			  (not (char=? (get-character (sub1 end))
				       #\newline)))
		 (insert #\newline (paragraph-start-position para)))
	       (cond
		 [(let ([real-start (cdr (find-offset end))]) 
		    (and (<= (+ 3 real-start) (last-position))
                         (string=? ";;"
                                   (get-text real-start
                                             (+ 2 real-start)))))
                  (void)]
		 [(= para 0) (do-indent 0)]
		 [(not contains)
		  (do-indent 0)]
		 [(not last) ;; search backwards for the opening parenthesis, and use it to align this line
		  (let ([enclosing (find-enclosing-paren pos)])
		    (do-indent (if enclosing
				   (+ (visual-offset enclosing) 1)
				   0)))]
		 [(= contains last)
		  (do-indent (+ (visual-offset contains)
				(procedure-indent)))]
		 [(special-check)
		  (do-indent (add1 (visual-offset contains)))]
		 [(= contain-para last-para)
		  (let ([name-length 
			 (id-walker (get-text contains (paragraph-end-position contain-para)))])
		    (do-indent (+ (visual-offset contains)
				  name-length
				  (indent-first-arg (+ contains 
						       name-length)))))]
		 [else
		  (do-indent (indent-first-arg (paragraph-start-position last-para)))]))))]
	[tabify-selection
	 (opt-lambda ([start-pos (get-start-position)]
		      [end-pos (get-end-position)])
	   (let ([first-para (position-paragraph start-pos)]
		 [end-para (position-paragraph end-pos)])
	     (with-handlers ([exn:misc:user-break?
			      (lambda (x) #t)])
	       (dynamic-wind
		(lambda () 
		  (when (< first-para end-para)
		    (begin-busy-cursor))
		  (begin-edit-sequence))
		(lambda ()
		  (let loop ([para first-para])
		    (when (<= para end-para)
		      (tabify (paragraph-start-position para))
		      (dynamic-enable-break (lambda () (break-enabled)))
		      (loop (add1 para))))
		  (when (and (>= (position-paragraph start-pos) end-para)
			     (<= (paren:skip-whitespace 
				  this (get-start-position) 'backward)
				 (paragraph-start-position first-para)))
		    (set-position 
		     (let loop ([new-pos (get-start-position)])
		       (if (let ([next (get-character new-pos)])
			     (and (char-whitespace? next)
				  (not (char=? next #\newline))))
			   (loop (add1 new-pos))
			   new-pos)))))
		(lambda ()
		  (end-edit-sequence)
		  (when (< first-para end-para)
		    (end-busy-cursor)))))))]
	[tabify-all (lambda () (tabify-selection 0 (last-position)))]
	[insert-return
	 (lambda ()
	   (if (tabify-on-return?)
	       (begin 
		 (begin-edit-sequence)
		 (insert #\newline)
		 (tabify (get-start-position))
		 (set-position 
		  (let loop ([new-pos (get-start-position)])
		    (if (let ([next (get-character new-pos)])
			  (and (char-whitespace? next)
			       (not (char=? next #\newline))))
			(loop (add1 new-pos))
			new-pos)))
		 (end-edit-sequence))
	       (insert #\newline)))]
	
        
        [calc-last-para
         (lambda (last-pos)
           (let ([last-para (position-paragraph last-pos #t)])
             (if (and (> last-pos 0)
                      (> last-para 0))
                 (begin (split-snip last-pos)
                        (let ([snip (find-snip last-pos 'before)])
                          (if (member 'hard-newline (send snip get-flags))
                              (- last-para 1)
                              last-para)))
                 last-para)))]
        [comment-out-selection
	 (opt-lambda ([start-pos (get-start-position)]
		      [end-pos (get-end-position)])
	   (begin-edit-sequence)
	   (let ([first-pos-is-first-para-pos?
		  (= (paragraph-start-position (position-paragraph start-pos))
		     start-pos)])
	     (let* ([first-para (position-paragraph start-pos)]
		    [last-para (calc-last-para end-pos)])
	       (let para-loop ([curr-para first-para])
		 (if (<= curr-para last-para)
		     (let ([first-on-para (paragraph-start-position curr-para)])
		       (insert #\; first-on-para)
		       (para-loop (add1 curr-para))))))
	     (when first-pos-is-first-para-pos?
	       (set-position
		(paragraph-start-position (position-paragraph (get-start-position)))
		(get-end-position))))
	   (end-edit-sequence)
	   #t)]
	[uncomment-selection
	 (opt-lambda ([start-pos (get-start-position)]
		      [end-pos (get-end-position)])
	   (begin-edit-sequence)
	   (let* ([last-pos (last-position)]
                  [first-para (position-paragraph start-pos)]
                  [last-para (calc-last-para end-pos)])
	     (let para-loop ([curr-para first-para])
	       (if (<= curr-para last-para)
		   (let ([first-on-para
			  (paren:skip-whitespace 
			   this 
			   (paragraph-start-position curr-para)
			   'forward)])
		     (when (and (< first-on-para last-pos)
				(char=? #\; (get-character first-on-para)))
		       (delete first-on-para (+ first-on-para 1)))
		     (para-loop (add1 curr-para))))))
	   (end-edit-sequence)
	   #t)]
	[get-forward-sexp
	 (lambda (start-pos)
	   (scheme-paren:forward-match 
	    this start-pos
	    (last-position)
	    forward-cache))]
	[remove-sexp
	 (lambda (start-pos)
	   (let ([end-pos (get-forward-sexp start-pos)])
	     (if end-pos 
		 (kill 0 start-pos end-pos)
		 (bell))
	     #t))]
	[forward-sexp
	 (lambda (start-pos)
	   (let ([end-pos (get-forward-sexp start-pos)])
	     (if end-pos 
		 (set-position end-pos)
		 (bell))
	     #t))]
	[flash-forward-sexp
	 (lambda (start-pos)
	   (let ([end-pos (get-forward-sexp start-pos)])
	     (if end-pos 
		 (flash-on end-pos (add1 end-pos))
		 (bell)) 
	     #t))]	    
	[get-backward-sexp
	 (lambda (start-pos)
	   (let* ([limit (get-limit start-pos)]
		  [end-pos
		   (scheme-paren:backward-match 
		    this start-pos limit backward-cache)]
		  [min-pos
		   (scheme-paren:backward-containing-sexp 
		    this start-pos limit backward-cache)]
		  [ans
		   (if (and end-pos 
			    (or (not min-pos)
				(>= end-pos min-pos)))
		       end-pos
		       #f)])
	     ans))]
	[flash-backward-sexp
	 (lambda (start-pos)
	   (let ([end-pos (get-backward-sexp start-pos)])
	     (if end-pos
		 (flash-on end-pos (add1 end-pos))
		 (bell))
	     #t))]
	[backward-sexp
	 (lambda (start-pos)
	   (let ([end-pos (get-backward-sexp start-pos)])
	     (if end-pos
		 (set-position end-pos)
		 (bell))
	     #t))]
	[find-up-sexp
	 (lambda (start-pos)
	   (let* ([exp-pos
		   (scheme-paren:backward-containing-sexp 
		    this start-pos
		    (get-limit start-pos) 
		    backward-cache)]
		  [paren-pos ;; find the closest open paren from this pair, behind exp-pos
		   (lambda (paren-pair)
		     (find-string
			   (car paren-pair)
			   'backward
			   exp-pos))])

	     (if (and exp-pos (> exp-pos 0))
		 (let ([poss (let loop ([parens (scheme-paren:get-paren-pairs)])
                              (cond
                                [(null? parens) null]
                                [else 
                                 (let ([pos (paren-pos (car parens))])
                                   (if pos
                                       (cons pos (loop (cdr parens)))
                                       (loop (cdr parens))))]))])
		   (if (null? poss) ;; all finds failed
		       #f
                       (- (apply max poss) 1))) ;; subtract one to move outside the paren
		 #f)))]
	[up-sexp
	 (lambda (start-pos)
	   (let ([exp-pos (find-up-sexp start-pos)])
	     (if exp-pos
		 (set-position exp-pos)
		 (bell))
	     #t))]
	[find-down-sexp
	 (lambda (start-pos)
	   (let ([last (last-position)])
	     (let loop ([pos start-pos])
	       (let ([next-pos (scheme-paren:forward-match 
				this pos last
				forward-cache)])
		 (if (and next-pos (> next-pos pos))
		     (let ([back-pos
			    (scheme-paren:backward-containing-sexp 
			     this (sub1 next-pos) pos backward-cache)])
		       (if (and back-pos
				(> back-pos pos))
			   back-pos
			   (loop next-pos)))
		     #f)))))]
	[down-sexp
	 (lambda (start-pos)
	   (let ([pos (find-down-sexp start-pos)])
	     (if pos
		 (set-position pos)
		 (bell))
	     #t))]
	[remove-parens-forward
	 (lambda (start-pos)
	   (let* ([pos (paren:skip-whitespace this start-pos 'forward)]
		  [first-char (get-character pos)]
		  [paren? (or (char=? first-char #\( )
			      (char=? first-char #\[ ))]
		  [closer (if paren? 
			      (scheme-paren:forward-match 
			       this pos (last-position)
			       forward-cache))])
	     (if (and paren? closer)
		 (begin (begin-edit-sequence)
			(delete pos (add1 pos))
			(delete (-  closer 2) (- closer 1))
			(end-edit-sequence))
		 (bell))
	     #t))])

      (private
	[select-text
	 (lambda (f forward?)
	   (lambda ()
	     (let* ([start-pos (get-start-position)]
		    [end-pos (get-end-position)])
	       (let-values ([(new-start new-end)
			     (if forward?
				 (values start-pos (f end-pos))
				 (values (f start-pos) end-pos))])
		 (if (and new-start new-end) 
		     (set-position new-start new-end)
		     (bell))
		 #t))))])
      (public
	[select-forward-sexp (select-text get-forward-sexp #t)]
	[select-backward-sexp (select-text get-backward-sexp #f)]
	[select-up-sexp (select-text find-up-sexp #f)]
	[select-down-sexp (select-text find-down-sexp #t)]

	[transpose-sexp
	 (lambda (pos)
	   (let ([start-1 (get-backward-sexp pos)])
	     (if (not start-1)
		 (bell)
		 (let ([end-1 (get-forward-sexp start-1)])
		   (if (not end-1)
		       (bell)
		       (let ([end-2 (get-forward-sexp end-1)])
			 (if (not end-2)
			     (bell)
			     (let ([start-2 (get-backward-sexp end-2)])
			       (if (or (not start-2)
				       (< start-2 end-1))
				   (bell)
				   (let ([text-1 
					  (get-text start-1 end-1)]
					 [text-2 
					  (get-text start-2 end-2)])
				     (begin-edit-sequence)
				     (insert text-1 start-2 end-2)
				     (insert text-2 start-1 end-1)
				     (set-position end-2)
				     (end-edit-sequence)))))))))))])
      (private
	[tab-size 8])
      (public
	[get-tab-size (lambda () tab-size)]
	[set-tab-size (lambda (s) (set! tab-size s))])

      (sequence
	(apply super-init args)
	
	(highlight-parens #t)
	(set-load-overwrites-styles #f)
	(set-wordbreak-map wordbreak-map)
	(set-tabs null tab-size #f)
	(set-style-list style-list)
	(set-styles-fixed #t)
	(let ([k (or (get-keymap)
		     (let ([k (make-object keymap%)])
		       (set-keymap k)
		       k))])
	  (send k chain-to-keymap keymap #t)))))

  (define -text% (text-mixin text:info%))

  (define setup-keymap
    (lambda (keymap)

	(let ([add-pos-function
	       (lambda (name ivar-sym)
		 (send keymap add-function name
		       (lambda (edit event)
			 ((ivar/proc edit ivar-sym)
			  (send edit get-start-position)))))])
	  (add-pos-function "remove-sexp" 'remove-sexp)
	  (add-pos-function "forward-sexp" 'forward-sexp)
	  (add-pos-function "backward-sexp" 'backward-sexp)
	  (add-pos-function "up-sexp" 'up-sexp)
	  (add-pos-function "down-sexp" 'down-sexp)
	  (add-pos-function "flash-backward-sexp" 'flash-backward-sexp)
	  (add-pos-function "flash-forward-sexp" 'flash-forward-sexp)
	  (add-pos-function "remove-parens-forward" 'remove-parens-forward)
	  (add-pos-function "transpose-sexp" 'transpose-sexp))
	
	(let ([add-edit-function
	       (lambda (name ivar-sym)
		 (send keymap add-function name
		       (lambda (edit event)
			 ((ivar/proc edit ivar-sym)))))])
	  (add-edit-function "select-forward-sexp" 'select-forward-sexp)
	  (add-edit-function "select-backward-sexp" 'select-backward-sexp)
	  (add-edit-function "select-down-sexp" 'select-down-sexp)
	  (add-edit-function "select-up-sexp" 'select-up-sexp)
	  (add-edit-function "tabify-at-caret" 'tabify-selection)
	  (add-edit-function "do-return" 'insert-return)
	  (add-edit-function "comment-out" 'comment-out-selection)
	  (add-edit-function "uncomment" 'uncomment-selection))

	(send keymap add-function "balance-parens"
	      (lambda (edit event)
		(send edit balance-parens event)))
	(send keymap add-function "balance-quotes"
	      (lambda (edit event)
		(send edit balance-quotes event)))

	(send keymap map-function "TAB" "tabify-at-caret")

	(send keymap map-function "return" "do-return")
	(send keymap map-function "s:return" "do-return")
	(send keymap map-function "s:c:return" "do-return")
	(send keymap map-function "a:return" "do-return")
	(send keymap map-function "s:a:return" "do-return")
	(send keymap map-function "c:a:return" "do-return")
	(send keymap map-function "c:s:a:return" "do-return")
	(send keymap map-function "c:return" "do-return")
	(send keymap map-function "d:return" "do-return")

	(send keymap map-function ")" "balance-parens")
	(send keymap map-function "]" "balance-parens")
	(send keymap map-function "}" "balance-parens")
	(send keymap map-function "\"" "balance-quotes")
	(send keymap map-function "|" "balance-quotes")

	;(send keymap map-function "c:up" "up-sexp") ;; paragraph
	;(send keymap map-function "s:c:up" "select-up-sexp")

	;(send keymap map-function "c:down" "down-sexp") ;; paragraph
	;(send keymap map-function "s:c:down" "select-down-sexp")

	(let ([map-meta
	       (lambda (key func)
		 (keymap:send-map-function-meta keymap key func))]
	      [map
	       (lambda (key func)
		 (send keymap map-function key func))])

	  (map-meta "up" "up-sexp")
	  (map-meta "c:u" "up-sexp")
	  (map "a:up" "up-sexp")
	  (map-meta "s:up" "select-up-sexp")
	  (map "a:s:up" "select-up-sexp")
	  (map-meta "s:c:u" "select-up-sexp")
	  
	  (map-meta "down" "down-sexp")
	  (map "a:down" "down-sexp")
	  (map-meta "c:down" "down-sexp")
	  (map-meta "s:down" "select-down-sexp")
	  (map "a:s:down" "select-down-sexp")
	  (map-meta "s:c:down" "select-down-sexp")
	  
	  (map-meta "right" "forward-sexp")
	  (map "a:right" "forward-sexp")
	  (map-meta "s:right" "select-forward-sexp")
	  (map "a:s:right" "select-forward-sexp")
	  
	  (map-meta "left" "backward-sexp")
	  (map "a:left" "backward-sexp")
	  (map-meta "s:left" "select-backward-sexp")
	  (map "a:s:left" "select-backward-sexp")
	  
	  (map-meta "return" "do-return")
	  (map-meta "s:return" "do-return")
	  (map-meta "s:c:return" "do-return")
	  (map-meta "a:return" "do-return")
	  (map-meta "s:a:return" "do-return")
	  (map-meta "c:a:return" "do-return")
	  (map-meta "c:s:a:return" "do-return")
	  (map-meta "c:return" "do-return")

	  (map-meta "c:semicolon" "comment-out")
	  (map-meta "c:=" "uncomment")
	  (map-meta "c:k" "remove-sexp")

	  (map-meta "c:f" "forward-sexp")
	  (map-meta "s:c:f" "select-forward-sexp")

	  (map-meta "c:b" "backward-sexp")
	  (map-meta "s:c:b" "select-backward-sexp")

	  (map-meta "c:p" "flash-backward-sexp")
	  (map-meta "s:c:n" "flash-forward-sexp")

	  (map-meta "c:space" "select-forward-sexp")
	  (map-meta "c:t" "transpose-sexp"))
	(send keymap map-function "c:c;c:b" "remove-parens-forward")))

  (define keymap (make-object keymap:aug-keymap%))
  (setup-keymap keymap)
  (define (get-keymap) keymap)

  (define (add-preferences-panel)
    (preferences:add-panel
     "Indenting"
     (lambda (p)
       (let*-values
	   ([(get-keywords)
	     (lambda (hash-table)
	       (letrec ([all-keywords (hash-table-map hash-table list)]
			[pick-out (lambda (wanted in out)
				    (cond
				     [(null? in) (quicksort out string<=?)]
				     [else (if (eq? wanted (cadr (car in))) 
					       (pick-out wanted (cdr in) (cons (symbol->string (car (car in))) out))
					       (pick-out wanted (cdr in) out))]))])
		 (values  (pick-out 'begin all-keywords null)
			  (pick-out 'define all-keywords null)
			  (pick-out 'lambda all-keywords null))))]
	    [(begin-keywords define-keywords lambda-keywords)
	     (get-keywords (preferences:get 'framework:tabify))])
	 (let* ([add-callback
		 (lambda (keyword-type keyword-symbol list-box)
		   (lambda (button command)
		     (let ([new-one
			    (keymap:call/text-keymap-initializer
			     (lambda ()
			       (get-text-from-user
				(string-append "Enter new " keyword-type "-like keyword:")
				(string-append keyword-type " Keyword"))))])
		       (when new-one
			 (let ([parsed (with-handlers ((exn:read? (lambda (x) #f)))
					 (read (open-input-string new-one)))])
			   (cond
			    [(and (symbol? parsed)
				  (hash-table-get (preferences:get 'framework:tabify)
						  parsed
						  (lambda () #f)))
			     (message-box "Error"
					  (format "\"~a\" is already a specially indented keyword" parsed))]
			    [(symbol? parsed)
			     (hash-table-put! (preferences:get 'framework:tabify)
					      parsed keyword-symbol)
			     (send list-box append (symbol->string parsed))]
			    [else (message-box "Error" (format "expected a symbol, found: ~a" new-one))]))))))]
		[delete-callback
		 (lambda (list-box)
		   (lambda (button command)
		     (let* ([selections (send list-box get-selections)]
			    [symbols (map (lambda (x) (string->symbol (send list-box get-string x))) selections)])
		       (for-each (lambda (x) (send list-box delete x)) (reverse selections))
		       (let ([ht (preferences:get 'framework:tabify)])
			 (for-each (lambda (x) (hash-table-remove! ht x)) symbols)))))]
		[main-panel (make-object horizontal-panel% p)]
		[make-column
		 (lambda (string symbol keywords)
		   (let* ([vert (make-object vertical-panel% main-panel)]
			  [_ (make-object message% (string-append string "-like Keywords") vert)]
			  [box (make-object list-box% #f keywords vert void '(multiple))]
			  [button-panel (make-object horizontal-panel% vert)]
			  [add-button (make-object button% "Add" button-panel (add-callback string symbol box))]
			  [delete-button (make-object button% "Remove" button-panel (delete-callback box))])
		     (send* button-panel 
			    (set-alignment 'center 'center)
			    (stretchable-height #f))
		     (send add-button min-width (send delete-button get-width))
		     box))]
		[begin-list-box (make-column "Begin" 'begin begin-keywords)]
		[define-list-box (make-column "Define" 'define define-keywords)]
		[lambda-list-box (make-column "Lambda" 'lambda lambda-keywords)]
		[update-list-boxes
		 (lambda (hash-table)
		   (let-values ([(begin-keywords define-keywords lambda-keywords) (get-keywords hash-table)]
				[(reset) (lambda (list-box keywords)
					   (send list-box clear)
					   (for-each (lambda (x) (send list-box append x)) keywords))])
		     (reset begin-list-box begin-keywords)
		     (reset define-list-box define-keywords)
		     (reset lambda-list-box lambda-keywords)
		     #t))])
	   (preferences:add-callback 'framework:tabify (lambda (p v) (update-list-boxes v)))
	   main-panel))))))))