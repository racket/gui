;; originally by Dan Grossman
;; 6/30/95

; Scheme mode for MrEd.

(unit/sig framework:scheme^
  (import mred-interfaces^
	  [preferences : framework:preferences^]
	  [match-cache : framework:match-cache^]
	  [paren : framework:paren^] 
	  [scheme-paren : framework:scheme-paren^]
	  [icon : framework:icon^]
	  [keymap : framework:keymap^]
	  [text : framework:text^]
	  [frame : framework:frame^]
	  [mzlib:thread : mzlib:thread^])
  
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
  (define standard-style-delta
    (let ([delta (make-object style-delta% 'change-normal)])
      (send delta set-delta 'change-family 'modern)
      delta))
  (let ([style (send style-list find-named-style "Standard")])
    (if style
	(send style set-delta standard-style-delta)
	(send style-list new-named-style "Standard"
	      (send style-list find-or-create-style
		    (send style-list find-named-style "Basic")
		    standard-style-delta))))

  (define text-mixin 
    (mixin (text:basic<%>) (-text<%>) args
      (inherit begin-edit-sequence
	       delete
	       end-edit-sequence
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
		(let ([f (find-string comment-start 'backward position)])
		  (if f
		      (= (position-line f) line)
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
	[in-highlight-parens? #f])

      (inherit get-styles-fixed)
      (rename [super-on-focus on-focus]
	      [super-after-change-style after-change-style]
	      [super-after-edit-sequence after-edit-sequence]
	      [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-after-set-size-constraint after-set-size-constraint]
	      [super-after-set-position after-set-position])
      (override
       [on-focus
	(lambda (on?)
	  (super-on-focus on?)
	  (highlight-parens (not on?)))]
       [after-change-style
	(lambda (start len)
	  (unless (get-styles-fixed)
	    (highlight-parens))
	  (super-after-change-style start len))]
       [after-edit-sequence
	(lambda ()
	  (unless in-highlight-parens?
	    (highlight-parens))
	  (super-after-edit-sequence))]
       [after-insert
	(lambda (start size)
	  (send backward-cache invalidate start)
	  (highlight-parens)
	  (super-after-insert start size))]
       [after-delete
	(lambda (start size)
	  (super-after-delete start size)
	  (send backward-cache invalidate start)
	  (send forward-cache forward-invalidate (+ start size) (- size))
	  (highlight-parens))]
       [after-set-size-constraint
	(lambda ()
	  (highlight-parens)
	  (super-after-set-size-constraint))]
       [after-set-position 
	(lambda ()
	  (highlight-parens)
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
	 (let* ([clear-old-location void]
		[old-gray-level 192]
		[gray-level (if (eq? (system-type) 'windows)
				(* 3/4 256)
				(- (* 7/8 256) 1))]
		[color (make-object color% 
			 gray-level 
			 gray-level
			 gray-level)])
	   (opt-lambda ([just-clear? #f])
	     (when highlight-parens?
	       (dynamic-wind
		(lambda ()
		  (set! in-highlight-parens? #t)
		  (begin-edit-sequence))
		(lambda ()
		  (clear-old-location)
		  (set! clear-old-location void)
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
		      (when (and (= here there)
				 (not (in-single-line-comment? here)))
			(let/ec k
			  (let-values
			      ([(left right)
				(cond
				  [(and (> here 0)
					(is-right-paren? (get-character (sub1 here))))
				   (cond
				     [(slash? (- here 2) (- here 1)) (k (void))]
				     [(scheme-paren:backward-match
				       this here (get-limit here)
				       backward-cache)
				      =>
				      (lambda (end-pos) 
					(values end-pos here))]
				     [else (k (void))])]
				  [(is-left-paren? (get-character here))
				   (cond
				     [(slash? (- here 1) here) (k (void))]
				     [(scheme-paren:forward-match
				       this here (last-position)
				       forward-cache)
				      =>
				      (lambda (end-pos)
					(values here end-pos))]
				     [else (k (void))])]
				  [else (k (void))])])
			    (clear-old-location)
			    (set! clear-old-location
				  (highlight-range left right
						   color
						   (icon:get-paren-highlight-bitmap)
						   (= there here left)))))))))
		(lambda ()
		  (end-edit-sequence)
		  (set! in-highlight-parens? #f))))))]
	
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
			  (string-append
			   (make-string (quotient amt tab-size) #\tab)
			   (make-string (remainder amt tab-size)
					#\space))
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
				     [(#\+ #\- #\. #\* #\/ #\< #\= #\> #\! #\? #\:
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
		    (if (and (<= (+ 3 real-start) (last-position))
			     (string=? ";;;"
				       (get-text real-start (+ 3 real-start))))
			real-start
			#f))
		  => (lambda (x) (set-position x))]
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
		      (mzlib:thread:dynamic-enable-break (lambda () (break-enabled)))
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
	[comment-out-selection
	 (opt-lambda ([start-pos (get-start-position)]
		      [end-pos (get-end-position)])
	   (begin-edit-sequence)
	   (let* ([first-para (position-paragraph start-pos)]
		  [last-para (position-paragraph end-pos)])
	     (let para-loop ([curr-para first-para])
	       (if (<= curr-para last-para)
		   (let ([first-on-para (paragraph-start-position curr-para)])
		     (if (not
			  (char=? #\; (get-character first-on-para)))
			 (insert ";" first-on-para))
		     (para-loop (add1 curr-para))))))
	   (end-edit-sequence)
	   #t)]
	[uncomment-selection
	 (opt-lambda ([start-pos (get-start-position)]
		      [end-pos (get-end-position)])
	   (begin-edit-sequence)
	   (let* ([first-para (position-paragraph start-pos)]
		  [last-para (position-paragraph end-pos)])
	     (let para-loop ([curr-para first-para])
	       (if (<= curr-para last-para)
		   (let ([first-on-para 
			  (paren:skip-whitespace 
			   this 
			   (paragraph-start-position curr-para)
			   'forward)])
		     (delete first-on-para
			   (+ first-on-para
			      (let char-loop ([n 0])
				(if (char=? #\; 
					    (get-character 
						  (+  n first-on-para)))
				    (char-loop (add1 n))
				    n))))
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
	   (let ([end-pos (get-forward-sexp this start-pos)])
	     (if end-pos 
		 (kill 0 start-pos end-pos)
		 (bell))
	     #t))]
	[forward-sexp
	 (lambda (start-pos)
	   (let ([end-pos (get-forward-sexp this start-pos)])
	     (if end-pos 
		 (set-position end-pos)
		 (bell))
	     #t))]
	[flash-forward-sexp
	 (lambda (start-pos)
	   (let ([end-pos (get-forward-sexp this start-pos)])
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
	 (lambda (start-pos move?)
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
		 (let ([pos (apply max
				   (map paren-pos (scheme-paren:get-paren-pairs)))])
		   (if (= pos -1)  ;; all finds failed
		       #f
		       (- pos 1))) ;; subtract one to move outside the paren
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
	(keymap:set-keymap-error-handler keymap)
	(keymap:set-keymap-implied-shifts keymap)

	(let ([add-pos-function ;; wx: this needs to be cleaned up!
	       (lambda (name ivar-sym)
		 (send keymap add-key-function name
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
		 (send keymap add-key-function name
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

	(send keymap add-key-function "balance-parens"
	      (lambda (edit event)
		(send edit balance-parens event)))
	(send keymap add-key-function "balance-quotes"
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
	  (map "a:up" "up-sexp")
	  (map-meta "s:up" "select-up-sexp")
	  (map "a:s:up" "select-up-sexp")
	  
	  (map-meta "down" "down-sexp")
	  (map "a:down" "down-sexp")
	  (map-meta "s:down" "select-down-sexp")
	  (map "a:s:down" "select-down-sexp")
	  
	  (map-meta "right" "forward-sexp")
	  (map "a:right" "forward-sexp")
	  (map-meta "s:right" "select-forward-sexp")
	  (map "a:s:right" "select-forward-sexp")
	  
	  (map-meta "left" "backward-sexp")
	  (map "a:left" "backward-sexp")
	  (map-meta "s:left" "select-backward-sexp")
	  (map "a:d:left" "select-backward-sexp")
	  
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

	  (map-meta "c:u" "up-sexp")
	  (map-meta "c:d" "down-sexp")

	  (map-meta "c:p" "flash-backward-sexp")
	  (map-meta "s:c:n" "flash-forward-sexp")

	  (map-meta "c:space" "select-forward-sexp")
	  (map-meta "c:t" "transpose-sexp"))
	(send keymap map-function "c:c;c:b" "remove-parens-forward")))

  (define keymap (make-object keymap%))
  (setup-keymap keymap)
  (define (get-keymap) keymap))
