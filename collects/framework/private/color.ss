(module color mzscheme
  (require (lib "class.ss")
           (lib "etc.ss")
           (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "token-tree.ss" "syntax-color")
           (lib "paren-tree.ss" "syntax-color")
           (lib "default-lexer.ss" "syntax-color")
           "sig.ss"
           "../macro.ss")
           
  
  (provide color@)
    
  (define (should-color-type? type)
    (not (memq type '(white-space no-color))))
  
  (define color@
    (unit/sig framework:color^
      (import [preferences : framework:preferences^]
              [icon : framework:icon^]
              [mode : framework:mode^]
              [text : framework:text^]
              [color-prefs : framework:color-prefs^]
              [scheme : framework:scheme^])
  
      (rename [-text<%> text<%>]
              [-text% text%]
              [-text-mode<%> text-mode<%>])
      
      (define -text<%>
        (interface ()
          start-colorer
          stop-colorer
          force-stop-colorer
          
          is-frozen?
          freeze-colorer
          thaw-colorer

          reset-region
          update-region-end
          
          skip-whitespace
          backward-match
          backward-containing-sexp
          forward-match
          insert-close-paren
	  classify-position))

      (define text-mixin
        (mixin (text:basic<%>) (-text<%>)
          ;; ---------------------- Coloring modes ----------------------------
          
          ;; The tokenizer is stopped.  This is used by the surrogate to enter
          ;; a mode with no coloring or paren matching.
          (define stopped? #t)

          ;; The tokenizer is stopped and prevented from starting.  This is 
          ;; an internal call for debugging.
          (define force-stop? #f)

          ;; color-callback has been suspended because the text% became locked
          ;; and should be requeued when the text% is unlocked.
          (define restart-callback #f)

          ;; Some other tool wants to take over coloring the buffer, so the 
          ;; colorer shouldn't color anything.
          (define frozen? #f)
          ;; true iff the colorer must recolor from scratch when the freeze
          ;; is over.
          (define force-recolor-after-freeze #f)
          
          ;; ---------------------- Lexing state ------------------------------
          
          ;; The tree of valid tokens, starting at start-pos
          (define tokens (new token-tree%))
          
          ;; If the tree is completed
          (define up-to-date? #t)
          
          ;; The tree of tokens that have been invalidated by an edit
          ;; but might still be valid.
          (define invalid-tokens (new token-tree%))
          
          ;; The position right before the invalid-tokens tree
          (define invalid-tokens-start +inf.0)
          
          ;; The position right before the next token to be read
          (define current-pos start-pos)
          
          ;; The lexer
          (define get-token #f)
                              
          ;; ---------------------- Parnethesis matching ----------------------
          
          ;; The pairs of matching parens
          (define pairs '())
          (define parens (new paren-tree% (matches pairs)))
          
          
          ;; ---------------------- Interactions state ------------------------
          ;; The positions right before and right after the area to be tokenized
          (define start-pos 0)
          (define end-pos 'end)
          
          ;; See docs
          (define/public (reset-region start end)
            (set! start-pos start)
            (set! end-pos end)
            (reset-tokens))
          
          ;; Modify the end of the region.
          (define/public (update-region-end end)
            (set! end-pos end))
          
          ;; ---------------------- Preferences -------------------------------
          (define should-color? #t)
          (define token-sym->style #f)
                    
          ;; ---------------------- Multi-threading ---------------------------
          ;; A list of thunks that color the buffer
          (define colors null)
          ;; The thread handle to the background colorer
          (define background-thread #f)
          ;; Prevent the background thread from being put to sleep while modifying
          ;; global state
          (define mutex-lock (make-semaphore 1))
          
          (inherit change-style begin-edit-sequence end-edit-sequence highlight-range
                   get-style-list in-edit-sequence? get-start-position get-end-position
                   local-edit-sequence? get-styles-fixed has-focus?)

          (define/private (reset-tokens)
            (send tokens reset-tree)
            (send invalid-tokens reset-tree)
            (set! invalid-tokens-start +inf.0)
            (set! up-to-date? #t)
            (set! restart-callback #f)
            (set! force-recolor-after-freeze #f)
            (set! parens (new paren-tree% (matches pairs)))
            (set! current-pos start-pos)
            (set! colors null)
            (modify))
          
          ;; Let the background thread know the text has been modified.
          (define/private (modify)
            (when background-thread
              (break-thread background-thread)))
          
          ;; Actually color the buffer.
          (define/private (color)
            (unless (null? colors)
              ((car colors))
              (set! colors (cdr colors))
              (color)))
          
          ;; Discard extra tokens at the first of invalie-tokens
          (define/private (sync-invalid)
            (when (and (not (send invalid-tokens is-empty?))
                       (< invalid-tokens-start current-pos))
              (send invalid-tokens search-min!)
              (let ((length (send invalid-tokens get-root-length)))
                (send invalid-tokens remove-root!)
                (set! invalid-tokens-start (+ invalid-tokens-start length)))
              (sync-invalid)))
          
          ;; re-tokenize should be called with breaks enabled and exit with
          ;; breaks disabled re-tokenize should be called when lock is not
          ;; held.  When it exits, the lock will be held.
          (define/private (re-tokenize in in-start-pos)
            (let-values (((lexeme type data new-token-start new-token-end) (get-token in)))
              ;; breaks must be disabled before the semaphore wait so we can't be
              ;; broken out of the critical section
              (break-enabled #f)
              ;; If a break occurs while we are suspended, the break will occur
              ;; and the critical section will not be entered
              (semaphore-wait/enable-break mutex-lock)
              (unless (eq? 'eof type)
                (let ((len (- new-token-end new-token-start)))
                  (set! current-pos (+ len current-pos))
                  (sync-invalid)
                  (when (and should-color? (should-color-type? type) (not frozen?))
                    (set! colors
                          (cons
                           (let ((color (send (get-style-list) find-named-style
                                              (token-sym->style type)))
                                 (sp (+ in-start-pos (sub1 new-token-start)))
                                 (ep (+ in-start-pos (sub1 new-token-end))))
                             (lambda ()
                               (change-style color sp ep #f)))
                           colors)))
                  (insert-last! tokens (new token-tree% (length len) (data type)))
                  (send parens add-token data len)
                  (cond
                    ((and (not (send invalid-tokens is-empty?))
                          (= invalid-tokens-start current-pos))
                     (send invalid-tokens search-max!)
                     (send parens merge-tree
                           (send invalid-tokens get-root-end-position))
                     (insert-last! tokens invalid-tokens)
                     (set! invalid-tokens-start +inf.0))
                    (else
                     (semaphore-post mutex-lock)
                     (break-enabled #t)
                     (re-tokenize in in-start-pos)))))))
          
          (define/private (do-insert/delete edit-start-pos change-length)
            (unless (or stopped? force-stop?)
              (modify)
              (unless up-to-date?
                (sync-invalid))
              (cond
                (up-to-date?
                 (let-values 
                     (((orig-token-start orig-token-end valid-tree invalid-tree)
                       (send tokens split (- edit-start-pos start-pos))))
                   (send parens split-tree orig-token-start)
                   (set! invalid-tokens invalid-tree)
                   (set! tokens valid-tree)
                   (set! invalid-tokens-start
                         (if (send invalid-tokens is-empty?)
                             +inf.0
                             (+ start-pos orig-token-end change-length)))
                   (set! current-pos (+ start-pos orig-token-start))
                   (set! up-to-date? #f)
                   (queue-callback (lambda () (colorer-callback)) #f)))
                ((>= edit-start-pos invalid-tokens-start)
                 (let-values (((tok-start tok-end valid-tree invalid-tree)
                               (send invalid-tokens split (- edit-start-pos start-pos))))
                   (set! invalid-tokens invalid-tree)
                   (set! invalid-tokens-start
                         (+ invalid-tokens-start tok-end change-length))))
                ((>= edit-start-pos current-pos)
                 (set! invalid-tokens-start (+ change-length invalid-tokens-start)))
                (else
                 (let-values (((tok-start tok-end valid-tree invalid-tree)
                               (send tokens split (- edit-start-pos start-pos))))
                   (send parens truncate tok-start)
                   (set! tokens valid-tree)
                   (set! invalid-tokens-start (+ change-length invalid-tokens-start))
                   (set! current-pos (+ start-pos tok-start)))))))

          (inherit is-locked?)
          
          (define/private (colorer-driver)
            (unless up-to-date?
              (thread-resume background-thread)
              (sleep .01)    ;; This is when the background thread is working.
              (semaphore-wait mutex-lock)
              (thread-suspend background-thread)
              (semaphore-post mutex-lock)
              (begin-edit-sequence #f #f)
              (color)
              (end-edit-sequence)))
          
          (define/private (colorer-callback)
            (cond
              ((is-locked?)
               (set! restart-callback #t))
              (else
               (unless (in-edit-sequence?)
                 (colorer-driver))
               (unless up-to-date?
                 (queue-callback (lambda () (colorer-callback)) #f)))))
          
          ;; Breaks should be disabled on entry
          (define/private (background-colorer-entry)
            (thread-suspend (current-thread))
            (background-colorer))
          
          ;; Breaks should be disabled on entry
          (define/private (background-colorer)
            (let/ec restart
              (parameterize ((current-exception-handler
                              (lambda (exn)
                                ;; Lock is not held here because breaks are disabled
                                ;; whenever lock is held
                                (break-enabled #f)
                                (restart))))
                (break-enabled #t)
                (with-handlers ((not-break-exn?
                                 (lambda (exn)
                                   (parameterize ((print-struct #t))
                                     (printf "colorer thread: ~s\n" exn))
                                   (break-enabled #f)
                                   (semaphore-wait mutex-lock))))
                  (re-tokenize (open-input-text-editor this current-pos end-pos
                                                       (lambda (x) (values #f 1)))
                               current-pos))
                ;; Breaks should be disabled from exit of re-tokenize
                ;; lock will be held
                (set! up-to-date? #t)
                (semaphore-post mutex-lock)
                (thread-suspend (current-thread))))
            (background-colorer))
          
          ;; Must not be called when the editor is locked
          (define/private (finish-now)
            (unless stopped?
              (let loop ()
                (unless up-to-date?
                  (colorer-driver)
                  (loop)))))
          ;; See docs
          (define/public (start-colorer token-sym->style- get-token- pairs-)
            (unless force-stop?
              (set! stopped? #f)
              (reset-tokens)
              (set! should-color? (preferences:get 'framework:coloring-active))
              (set! token-sym->style token-sym->style-)
              (set! get-token get-token-)
              (set! pairs pairs-)
              (set! parens (new paren-tree% (matches pairs)))
              (unless background-thread
                (break-enabled #f)
                (set! background-thread
                      (thread (lambda () (background-colorer-entry))))
                (break-enabled #t))
              (do-insert/delete start-pos 0)))
            
          ;; See docs
          (define/public stop-colorer
            (opt-lambda ((clear-colors #t))
              (set! stopped? #t)
              (when (and clear-colors (not frozen?))
                (begin-edit-sequence #f #f)
                (change-style (send (get-style-list) find-named-style "Standard")
                              start-pos end-pos #f)
                (end-edit-sequence))
              (match-parens #t)
              (reset-tokens)
              (set! pairs null)
              (set! token-sym->style #f)
              (set! get-token #f)))
          
          (define/public (is-frozen?) frozen?)
          
          ;; See docs
          (define/public (freeze-colorer)
            (when (is-locked?)
              (error 'freeze-colorer "called on a locked color:text<%>."))
            (unless frozen?
              (finish-now)
              (set! frozen? #t)))
          
          ;; See docs
          (define/public thaw-colorer
            (opt-lambda ((recolor? #t)
                         (retokenize? #f))
              (when frozen?
                (set! frozen? #f)
                (cond
                  (stopped?
                   (stop-colorer))
                  ((or force-recolor-after-freeze recolor?)
                   (cond
                     (retokenize?
                      (let ((tn token-sym->style)
                            (gt get-token)
                            (p pairs))
                        (stop-colorer (not should-color?))
                        (start-colorer tn gt p)))
                     (else
                      (begin-edit-sequence #f #f)
                      (finish-now)
                      (send tokens for-each
                            (lambda (start len type)
                              (when (and should-color? (should-color-type? type))
                                (let ((color (send (get-style-list) find-named-style
                                                   (token-sym->style type)))
                                      (sp (+ start-pos start))
                                      (ep (+ start-pos (+ start len))))
                                  (change-style color sp ep #f)))))
                      (end-edit-sequence))))))))
                                
                      
          (define/private (toggle-color on?)
            (cond
              ((and frozen? (not (equal? on? should-color?)))
               (set! should-color? on?)
               (set! force-recolor-after-freeze #t))
              ((and (not should-color?) on?)
               (set! should-color? on?)
               (reset-tokens)
               (do-insert/delete start-pos 0))
              ((and should-color? (not on?))
               (set! should-color? on?)
               (begin-edit-sequence #f #f)
               (change-style (send (get-style-list) find-named-style "Standard")
                             start-pos end-pos #f)
               (end-edit-sequence))))

          ;; see docs
          (define/public (force-stop-colorer stop?)
            (set! force-stop? stop?)
            (when stop?
              (stop-colorer)
              (when background-thread
                (kill-thread background-thread)
		(set! background-thread #f))))
          
          
          ;; ----------------------- Match parentheses ----------------------------
          
          (define clear-old-locations 'dummy)
          (set! clear-old-locations void)
          
          (define mismatch-color (make-object color% "PINK"))
          (define/private (get-match-color) (preferences:get 'framework:paren-match-color))
          
          (define/private (highlight start end caret-pos error?)
            (let ([off (highlight-range (+ start-pos start) (+ start-pos end)
                                        (if error? mismatch-color (get-match-color))
                                        (and (send (icon:get-paren-highlight-bitmap)
                                                   ok?)
                                             (icon:get-paren-highlight-bitmap))
                                        (= caret-pos (+ start-pos start)))])
              (set! clear-old-locations
                    (let ([old clear-old-locations])
                      (lambda ()
                        (old)
                        (off))))))
          
          (define in-match-parens? #f)
          
          ;; the forward matcher signaled an error because not enough of the
          ;; tree has been built.
          (define/private (f-match-false-error start end error)
            (and error (<= (+ start-pos error) current-pos) (not up-to-date?)))
          
          ;; If there is no match because the buffer isn't lexed yet, this will
          ;; do nothing, but the edit sequence for changing color the colors 
          ;; will trigger a callback that will call this to try and match again.
          ;; This edit sequence is used even if the coloring is disabled in
          ;; the preferences, although nothing is actually colored during it.
          ;; This leads to the nice bahavior that we don't have to block to
          ;; highlight parens, and the parens will be highlighted as soon as
          ;; possible.
          (define/private match-parens
            (opt-lambda ([just-clear? #f])
              (unless in-match-parens?
                (set! in-match-parens? #t)
                (begin-edit-sequence #f #f)
                (clear-old-locations)
                (set! clear-old-locations void)
                (when (preferences:get 'framework:highlight-parens)
                  (unless just-clear?
                    (let* ((here (get-start-position)))
                      (when (= here (get-end-position))
                        (let-values (((start-f end-f error-f)
                                      (send parens match-forward (- here start-pos)))
                                     ((start-b end-b error-b)
                                      (send parens match-backward (- here start-pos))))
                          (when (and start-f end-f 
                                     (not (f-match-false-error start-f end-f error-f)))
                            (highlight start-f end-f here error-f))
                          (when (and start-b end-b)
                            (highlight start-b end-b here error-b)))))))
                (end-edit-sequence)
                (set! in-match-parens? #f))))
                    
          ;; See docs
          (define/public (forward-match position cutoff)
            (do-forward-match position cutoff #t))
          
          (define/private (do-forward-match position cutoff skip-whitespace?)
            (let ((position 
                   (if skip-whitespace? 
                       (skip-whitespace position 'forward #t)
                       position)))
              (let-values (((start end error)
                            (send parens match-forward (- position start-pos))))
                (cond
                  ((f-match-false-error start end error)
                   (colorer-driver)
                   (do-forward-match position cutoff #f))
                  ((and start end (not error))
                   (let ((match-pos (+ start-pos end)))
                     (cond
                       ((<= match-pos cutoff) match-pos)
                       (else #f))))
                  ((and start end error) #f)
                  (else
                   (let-values (((tok-start tok-end)
                                 (begin
                                   (tokenize-to-pos position)
                                   (send tokens search! (- position start-pos))
                                   (values (send tokens get-root-start-position)
                                           (send tokens get-root-end-position)))))
                     (cond
                       ((or (send parens is-close-pos? tok-start)
                            (= (+ start-pos tok-end) position))
                        #f)
                       (else
                        (+ start-pos tok-end)))))))))
          
                    
          ;; See docs
          (define/public (backward-match position cutoff)
            (let ((x (internal-backward-match position cutoff)))
              (cond
                ((eq? x 'open) #f)
                (else x))))
          
          (define/private (internal-backward-match position cutoff)
            (when stopped?
              (error 'backward-match "called on a color:text<%> whose colorer is stopped."))
            (let ((position (skip-whitespace position 'backward #t)))
              (let-values (((start end error)
                            (send parens match-backward (- position start-pos))))
                (cond
                  ((and start end (not error))
                   (let ((match-pos (+ start-pos start)))
                     (cond
                       ((>= match-pos cutoff) match-pos)
                       (else #f))))
                  ((and start end error) #f)
                  (else
                   (let-values (((tok-start tok-end)
                                 (begin
                                   (send tokens search!
                                         (if (> position start-pos)
                                             (- position start-pos 1)
                                             0))
                                   (values (send tokens get-root-start-position)
                                           (send tokens get-root-end-position)))))
                     (cond
                       ((or (send parens is-open-pos? tok-start)
                            (= (+ start-pos tok-start) position))
                        'open)
                       (else
                        (+ start-pos tok-start)))))))))

          ;; See docs
          (define/public (backward-containing-sexp position cutoff)
            (when stopped?
              (error 'backward-containing-sexp "called on a color:text<%> whose colorer is stopped."))
            (let loop ((cur-pos position))
              (let ((p (internal-backward-match cur-pos cutoff)))
                (cond
                  ((eq? 'open p) cur-pos)
                  ((not p) #f)
                  (else (loop p))))))

	  ;; Determines whether a position is a 'comment, 'string, etc.
	  (define/public (classify-position position)
            (when stopped?
              (error 'classify-position "called on a color:text<%> whose colorer is stopped."))
	    (tokenize-to-pos position)
	    (send tokens search! (- position start-pos))
	    (send tokens get-root-data))
            
          (define/private (tokenize-to-pos position)
            (when (and (not up-to-date?) (<= current-pos position))
              (colorer-driver)
              (tokenize-to-pos position)))
          
          (inherit last-position)
          
          ;; See docs
          (define/public (skip-whitespace position direction comments?)
            (when stopped?
              (error 'skip-whitespace "called on a color:text<%> whose colorer is stopped."))
            (cond
              ((and (eq? direction 'forward)
                    (>= position (if (eq? 'end end-pos) (last-position) end-pos)))
               position)
              ((and (eq? direction 'backward) (<= position start-pos))
               position)
              (else
               (tokenize-to-pos position)
               (send tokens search! (- (if (eq? direction 'backward) (sub1 position) position)
                                       start-pos))
                 (cond
                   ((or (eq? 'white-space (send tokens get-root-data))
                        (and comments? (eq? 'comment (send tokens get-root-data))))
                    (skip-whitespace (+ start-pos
                                        (if (eq? direction 'forward)
                                            (send tokens get-root-end-position)
                                            (send tokens get-root-start-position)))
                                     direction
                                     comments?))
                   (else position)))))
          
          (define/private (get-close-paren pos closers)
            (cond
              ((null? closers) #f)
              (else
               (let* ((c (car closers))
                      (l (string-length c)))
                 (insert c)
                 (let ((m (backward-match (+ l pos) start-pos)))
                   (cond
                     ((and m
                           (send parens is-open-pos? (- m start-pos))
                           (send parens is-close-pos? (- pos start-pos)))
                      (delete pos (+ l pos))
                      c)
                     (else
                      (delete pos (+ l pos))                      
                      (get-close-paren pos (cdr closers)))))))))
          
          (inherit insert delete flash-on)
          ;; See docs
          (define/public (insert-close-paren pos char flash? fixup?)
            (let ((closer
                   (begin
                     (begin-edit-sequence #f #f)
                     (get-close-paren pos (if fixup? (map symbol->string (map cadr pairs)) null)))))
              (end-edit-sequence)
              (let ((insert-str (if closer closer (string char))))
                (insert insert-str)
                (when flash?
                  (unless stopped?
                    (let ((to-pos (backward-match (+ (string-length insert-str) pos) 0)))
                      (when (and to-pos
                                 (send parens is-open-pos? (- to-pos start-pos))
                                 (send parens is-close-pos? (- pos start-pos)))
                        (flash-on to-pos (+ 1 to-pos)))))))))
          
          (define/public (debug-printout)
            (let* ((x null)
                   (f (lambda (a b c)
                        (set! x (cons (list a b c) x)))))
              (send tokens for-each f)
              (printf "tokens: ~e~n" (reverse x))
              (set! x null)
              (send invalid-tokens for-each f)
              (printf "invalid-tokens: ~e~n" (reverse x))
              (printf "start-pos: ~a current-pos: ~a invalid-tokens-start ~a~n"
                      start-pos current-pos invalid-tokens-start)
              (printf "parens: ~e~n" (car (send parens test)))))
          
          ;; ------------------------- Callbacks to Override ----------------------
          
          (rename (super-lock lock))
          (define/override (lock x)
            (super-lock x)
            (when (and restart-callback (not x))
              (set! restart-callback #f)
              (queue-callback (lambda () (colorer-callback)))))
          
          
          (rename (super-on-focus on-focus))
          (define/override (on-focus on?)
            (super-on-focus on?)
            (match-parens (not on?)))
          
          (rename (super-on-change on-change))
          (define/override (on-change)
            (modify))
          
          (rename (super-after-edit-sequence after-edit-sequence))
          (define/override (after-edit-sequence)
            (super-after-edit-sequence)
            (when (has-focus?)
              (match-parens)))
          
          (rename (super-after-set-position after-set-position))
          (define/override (after-set-position)
            (super-after-set-position)
            (unless (local-edit-sequence?)
              (when (has-focus?)
                (match-parens)))
            (modify))
          
          (rename (super-after-change-style after-change-style))
          (define/override (after-change-style a b)
            (super-after-change-style a b)
            (unless (get-styles-fixed)
              (unless (local-edit-sequence?)
                (when (has-focus?)
                  (match-parens))))
            (modify))
          
          (rename (super-on-set-size-constraint on-set-size-constraint))
          (define/override (on-set-size-constraint)
            (super-on-set-size-constraint)
            (unless (local-edit-sequence?)
              (when (has-focus?)
                (match-parens)))
            (modify))
          
          (rename (super-after-insert after-insert))
          (define/override (after-insert edit-start-pos change-length)
            (do-insert/delete edit-start-pos change-length)
            (super-after-insert edit-start-pos change-length))
          
          (rename (super-after-delete after-delete))
          (define/override (after-delete edit-start-pos change-length)
            (do-insert/delete edit-start-pos (- change-length))
            (super-after-delete edit-start-pos change-length))
                    
          (rename (super-change-style change-style))
          
          (super-new)
          
          ;; need pref-callback to be in a private field
          ;; so that the editor hangs on to the callback
          ;; when the editor goes away, so does the callback
          (define (pref-callback k v) (toggle-color v))
          (preferences:add-callback 'framework:coloring-active pref-callback #t)))
      
      (define -text% (text-mixin text:keymap%))
              
      (define -text-mode<%> (interface ()))
      
      (define text-mode-mixin
        (mixin (mode:surrogate-text<%>) (-text-mode<%>)
          ;; The arguments here are only used to be passed to start-colorer.  Refer to its
          ;; documentation.
          (init-field (get-token default-lexer) 
                      (token-sym->style (lambda (x) "Standard"))
                      (matches null))
          
          (rename (super-on-disable-surrogate on-disable-surrogate))
          (define/override (on-disable-surrogate text)
            (super-on-disable-surrogate text)
            (send text stop-colorer))
          
          (rename (super-on-enable-surrogate on-enable-surrogate))
          (define/override (on-enable-surrogate text)
            (super-on-enable-surrogate text)
            (send text start-colorer token-sym->style get-token matches))
          
          (super-instantiate ())))
  
      (define text-mode% (text-mode-mixin mode:surrogate-text%))))

  
  )
