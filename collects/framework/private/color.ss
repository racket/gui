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
  
  (define color@
    (unit/sig framework:color^
      (import [preferences : framework:preferences^]
              [icon : framework:icon^]
              [mode : framework:mode^]
              [text : framework:text^])
  
      (rename [-text<%> text<%>]
              [-text% text%]
              [-text-mode<%> text-mode<%>])
      
      (define -text<%>
        (interface ()
          start-colorer
          stop-colorer
          reset-region
          update-region-end))
                
      (define text-mixin
        (mixin (text:basic<%>) (-text<%>)
          ;; ---------------------- Lexing state ----------------------------------
          
          ;; The tree of valid tokens, starting at start-pos
          (define tokens (new token-tree%))
          
          ;; The tree of tokens that have been invalidated by an edit
          ;; but might still be valid.
          (define invalid-tokens (new token-tree%))
          
          ;; The position right before the invalid-tokens tree
          (define invalid-tokens-start +inf.0)
          
          ;; The position right before the next token to be read
          (define current-pos start-pos)
          
          ;; The lexer
          (define get-token #f)
          
          ;; If the tree is completed
          (define up-to-date? #t)
          
          ;; The tokenizer is stopped
          (define stopped? #t)

          ;; The tokenizer is stopped and prevented from starting
          (define force-stop? #f)
          
          ;; color-callback has been suspended because the text% became locked and
          ;; should be requeued when the text% is unlocked.
          (define restart-callback #f)
          
          ;; ---------------------- Parnethesis matching --------------------------
          
          (define pairs '())
          (define parens (new paren-tree% (matches pairs)))
          
          
          ;; ---------------------- Interactions state ----------------------------
          ;; The positions right before and right after the area to be tokenized
          (define start-pos 0)
          (define end-pos 'end)
          
          (define/public (reset-region start end)
            (set! start-pos start)
            (set! end-pos end)
            (reset-tokens))
          
          (define/public (update-region-end end)
            (set! end-pos end))
          
          ;; ---------------------- Preferences -----------------------------------
          (define should-color? #t)
          (define remove-prefs-callback-thunk #f)
          (define prefix #f)
          
          ;; ---------------------- Multi-threading -------------------------------
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
          (define/public (reset-tokens)
            (send tokens reset-tree)
            (send invalid-tokens reset-tree)
            (set! invalid-tokens-start +inf.0)
            (set! up-to-date? #t)
            (set! restart-callback #f)
            (set! parens (new paren-tree% (matches pairs)))
            (set! current-pos start-pos)
            (set! colors null)
            (modify))
          
          (define (modify)
            (when background-thread
              (break-thread background-thread)))
          
          (define (color)
            (unless (null? colors)
              ((car colors))
              (set! colors (cdr colors))
              (color)))
          
          (define (sync-invalid)
            (when (and (not (send invalid-tokens is-empty?))
                       (< invalid-tokens-start current-pos))
              (send invalid-tokens search-min!)
              (let ((length (send invalid-tokens get-root-length)))
                (send invalid-tokens remove-root!)
                (set! invalid-tokens-start (+ invalid-tokens-start length)))
              (sync-invalid)))
          
          ;; re-tokenize should be called with breaks enabled and exit with breaks disabled
          ;; re-tokenize should be called when lock is not held.  When it exits, the lock
          ;;   will be held.
          (define (re-tokenize in in-start-pos)
            (let-values (((type data new-token-start new-token-end) (get-token in)))
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
                  (when (and should-color? (not (eq? 'white-space type)))
                    (set! colors
                          (cons
                           (let ((color (send (get-style-list) find-named-style
                                              (format "syntax-coloring:~a:~a"
                                                      prefix
                                                      type)))
                                 (sp (+ in-start-pos (sub1 new-token-start)))
                                 (ep (+ in-start-pos (sub1 new-token-end))))
                             (lambda ()
                               (change-style color sp ep #f)))
                           colors)))
                  (insert-last! tokens (new token-tree% (length len) (data data)))
                  (send parens add-token data len)
                  (cond
                    ((and (not (send invalid-tokens is-empty?))
                          (= invalid-tokens-start current-pos))
                     (send invalid-tokens search-max!)
                     (send parens merge-tree (send invalid-tokens get-root-end-position))
                     (insert-last! tokens invalid-tokens)
                     (set! invalid-tokens-start +inf.0))
                    (else
                     (semaphore-post mutex-lock)
                     (break-enabled #t)
                     (re-tokenize in in-start-pos)))))))
          
          (define (do-insert/delete edit-start-pos change-length)
            (unless (or stopped? force-stop?)
              (when (> edit-start-pos start-pos)
                (set! edit-start-pos (sub1 edit-start-pos)))
              (modify)
              (cond
                (up-to-date?
                 (send tokens search! (- edit-start-pos start-pos))
                 (let-values (((orig-token-start orig-token-end valid-tree invalid-tree)
                               (send tokens split)))
                   (send parens split-tree orig-token-start)
                   (set! invalid-tokens invalid-tree)
                   (set! tokens valid-tree)
                   (set! invalid-tokens-start
                         (if (send invalid-tokens is-empty?)
                             +inf.0
                             (+ start-pos orig-token-end change-length)))
                   (set! current-pos (+ start-pos orig-token-start))
                   (set! up-to-date? #f)
                   (queue-callback colorer-callback #f)))
                ((>= edit-start-pos invalid-tokens-start)
                 (send invalid-tokens search! (- edit-start-pos invalid-tokens-start))
                 (let-values (((tok-start tok-end valid-tree invalid-tree)
                               (send invalid-tokens split)))
                   (set! invalid-tokens invalid-tree)
                   (set! invalid-tokens-start (+ invalid-tokens-start tok-end change-length))))
                ((>= edit-start-pos current-pos)
                 (set! invalid-tokens-start (+ change-length invalid-tokens-start)))
                (else
                 (send tokens search! (- edit-start-pos start-pos))
                 (let-values (((tok-start tok-end valid-tree invalid-tree)
                               (send tokens split)))
                   (send parens truncate tok-start)
                   (set! tokens valid-tree)
                   (set! invalid-tokens-start (+ change-length invalid-tokens-start))
                   (set! current-pos (+ start-pos tok-start)))))))

          (inherit is-locked?)
          
          (define (colorer-callback)
            (cond
              ((is-locked?)
               (set! restart-callback #t))
              (else
               (unless (in-edit-sequence?)
                 (thread-resume background-thread)
                 (sleep .01)    ;; This is when the background thread is working.
                 (semaphore-wait mutex-lock)
                 (thread-suspend background-thread)
                 (semaphore-post mutex-lock)
                 (begin-edit-sequence #f #f)
                 (color)
                 (end-edit-sequence))
               (unless up-to-date?
                 (queue-callback colorer-callback #f)))))
          
          
          ;; Breaks should be disabled on entry
          (define (background-colorer-entry)
            (thread-suspend (current-thread))
            (background-colorer))
          
          ;; Breaks should be disabled on entry
          (define (background-colorer)
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
                                   (printf "colorer thread: ~s\n" exn)
                                   (break-enabled #f)
                                   (semaphore-wait mutex-lock))))
                  (re-tokenize (open-input-text-editor this current-pos end-pos)
                               current-pos))
                ;; Breaks should be disabled from exit of re-tokenize
                ;; lock will be held
                (set! up-to-date? #t)
                (semaphore-post mutex-lock)
                (thread-suspend (current-thread))))
            (background-colorer))
          
          (define/public (start-colorer prefix- get-token- pairs-)
            (unless force-stop?
              (set! stopped? #f)
              (reset-tokens)
              (set! should-color?
                    (preferences:get (string->symbol (format "syntax-coloring:~a:active" prefix-))))
              (set! prefix prefix-)
              (set! get-token get-token-)
              (set! pairs pairs-)
              (set! parens (new paren-tree% (matches pairs)))
              (unless remove-prefs-callback-thunk
                (set! remove-prefs-callback-thunk
                      (preferences:add-callback
                       (string->symbol (format "syntax-coloring:~a:active" prefix))
                       (lambda (_ on?)
                         (set! should-color? on?)
                         (cond
                           (on?
                            (reset-tokens)
                            (do-insert/delete start-pos 0))
                           (else (change-style (send (get-style-list) find-named-style "Standard")
                                               start-pos end-pos #f)))))))
              (unless background-thread
                (break-enabled #f)
                (set! background-thread (thread (lambda () (background-colorer-entry))))
                (break-enabled #t))
              (do-insert/delete start-pos 0)))
            
          (define/public (stop-colorer)
            (set! stopped? #t)
            (when remove-prefs-callback-thunk
              (remove-prefs-callback-thunk)
              (set! remove-prefs-callback-thunk #f))
            (change-style (send (get-style-list) find-named-style "Standard")
                          start-pos end-pos #f)
            (match-parens #t)
            (reset-tokens)
            (set! pairs null)
            (set! prefix #f)
            (set! get-token #f))
          
          
          (define/public (force-stop-colorer x)
            (set! force-stop? x)
            (when x
              (stop-colorer)
              (when background-thread
                (kill-thread background-thread)
		(set! background-thread #f))))
            
          
          ;; ----------------------- Match parentheses ----------------------------
          
          (define clear-old-locations 'dummy)
          (set! clear-old-locations void)
          
          (define mismatch-color (make-object color% "PINK"))
          (define (get-match-color) (preferences:get 'framework:paren-match-color))
          
          (define (highlight start end caret-pos error?)
            (let ([off (highlight-range (+ start-pos start) (+ start-pos end)
                                        (if error? mismatch-color (get-match-color))
                                        (and (send (icon:get-paren-highlight-bitmap) ok?)
                                             (icon:get-paren-highlight-bitmap))
                                        (= caret-pos (+ start-pos start)))])
              (set! clear-old-locations
                    (let ([old clear-old-locations])
                      (lambda ()
                        (old)
                        (off))))))
          
          (define in-match-parens? #f)
          
          (define match-parens
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
                        (let-values (((start-f end-f error-f) (send parens match-forward (- here start-pos)))
                                     ((start-b end-b error-b) (send parens match-backward (- here start-pos))))
                          (when (and start-f end-f
                                     (not (and error-f (<= (+ start-pos error-f) current-pos) (not up-to-date?))))
                            (highlight start-f end-f here error-f))
                          (when (and start-b end-b)
                            (highlight start-b end-b here error-b)))))))
                (end-edit-sequence)
                (set! in-match-parens? #f))))
                    
          ;; ------------------------- Callbacks to Override ----------------------
          
          (rename (super-lock lock))
          (define/override (lock x)
            (super-lock x)
            (when (and restart-callback (not x))
              (set! restart-callback #f)
              (queue-callback colorer-callback)))
          
          
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
          
          (super-instantiate ())))
      
      (define -text% (text-mixin text:keymap%))
              
      (define -text-mode<%> (interface ()))
      
      (define text-mode-mixin
        (mixin (mode:surrogate-text<%>) (-text-mode<%>)
          ;; get-token takes an input port and returns 4 values:
          ;; A symbol in `(keyword string literal comment error identifier default)
          ;; Data to be kept with the token
          ;; The token's starting offset
          ;; The token's ending offset
          ;;
          ;; matches is a list of lists of matching paren types.
          ;; For example, '((|(| |)|) (|[| |]|))
          (init-field (get-token default-lexer) (prefix 'default) (matches null))
          
          (rename (super-on-disable-surrogate on-disable-surrogate))
          (define/override (on-disable-surrogate text)
            (super-on-disable-surrogate text)
            (send text stop-colorer))
          
          (rename (super-on-enable-surrogate on-enable-surrogate))
          (define/override (on-enable-surrogate text)
            (super-on-enable-surrogate text)
            (send text start-colorer prefix get-token matches))
          
          (super-instantiate ())))
  
      (define text-mode% (text-mode-mixin mode:surrogate-text%))
      
      
    )))
    