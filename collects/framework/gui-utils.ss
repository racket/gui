(module gui-utils mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred")
           (lib "etc.ss")
           "specs.ss"
           (lib "string-constant.ss" "string-constants"))
  
  (define-syntax (provide/contract/docs stx)
    (syntax-case stx ()
      [(_ (name contract docs ...) ...)
       (syntax (provide/contract (name contract) ...))]))
  
  (provide/contract/docs
   (gui-utils:next-untitled-name any?)
   (gui-utils:cursor-delay any?)
   (gui-utils:show-busy-cursor any?)
   (gui-utils:delay-action any?)
   (gui-utils:local-busy-cursor any?)
   (gui-utils:unsaved-warning any?)
   (gui-utils:read-snips/chars-from-text any?)
   (gui-utils:get-choice any?)
   (gui-utils:open-input-buffer any?)
   (gui-utils:get-clicked-clickback-delta any?)
   (gui-utils:get-clickback-delta any?))
  
  (provide (rename alphabetic-list-box% gui-utils:alphabetic-list-box%)
           (rename text-snip<%> gui-utils:text-snip<%>))
  
  (define clickback-delta (make-object style-delta% 'change-underline #t))
  (send clickback-delta set-delta-foreground "BLUE")
  (define (get-clickback-delta) clickback-delta)
  (define clicked-clickback-delta (make-object style-delta%))
  (send clicked-clickback-delta set-delta-background "BLACK")
  (define (get-clicked-clickback-delta) clicked-clickback-delta)
  
  (define next-untitled-name
    (let ([n 1])
      (lambda ()
        (begin0
          (cond
            [(= n 1) (string-constant untitled)]
            [else (format (string-constant untitled-n) n)])
          (set! n (+ n 1))))))
  
  (define cursor-delay
    (let ([x 0.25])
      (case-lambda
        [() x]
        [(v) (set! x v) x])))
  
  (define show-busy-cursor
    (lambda (thunk)
      (local-busy-cursor #f thunk)))
  
  (define delay-action
    (lambda (delay-time open close)
      (let ([semaphore (make-semaphore 1)]
            [open? #f]
            [skip-it? #f])
        (thread 
         (lambda ()
           (sleep delay-time)
           (semaphore-wait semaphore)
           (unless skip-it?
             (set! open? #t)
             (open))
           (semaphore-post semaphore)))
        (lambda ()
          (semaphore-wait semaphore)
          (set! skip-it? #t)
          (when open?
            (close))
          (semaphore-post semaphore)))))
  
  (define local-busy-cursor
        (let ([watch (make-object cursor% 'watch)])
          (case-lambda
           [(win thunk) (local-busy-cursor win thunk (cursor-delay))]
           [(win thunk delay)
            (let* ([old-cursor #f]
                   [cursor-off void])
              (dynamic-wind
               (lambda ()
                 (set! cursor-off
                       (delay-action
                        delay
                        (lambda ()
                          (if win
                              (begin (set! old-cursor (send win get-cursor))
                                     (send win set-cursor watch))
                              (begin-busy-cursor)))
                        (lambda ()
                          (if win
                              (send win set-cursor old-cursor)
                              (end-busy-cursor))))))
               (lambda () (thunk))
               (lambda () (cursor-off))))])))
  
  (define unsaved-warning
    (case-lambda
      [(filename action-anyway) (unsaved-warning filename action-anyway #f)]
      [(filename action-anyway can-save-now?) (unsaved-warning filename action-anyway can-save-now? #f)]
      [(filename action-anyway can-save-now? parent)
       (let* ([result (void)]
              [unsaved-dialog%
               (class dialog%
                 (inherit show center)
                 
                 (define/private (on-dont-save)
                   (set! result 'continue)
                   (show #f))
                 (define/private (on-save-now)
                   (set! result 'save)
                   (show #f))
                 (define/private (on-cancel)
                   (set! result 'cancel)
                   (show #f))
                 
                 (super-make-object (string-constant warning) parent)
                 (let* ([panel (make-object vertical-panel% this)]
                        [msg
                         (make-object message%
                           (format (string-constant file-is-not-saved) filename)
                           panel)]
                        [button-panel
                         (make-object horizontal-panel% panel)])
                   (make-object button% 
                     (string-append action-anyway)
                     button-panel
                     (lambda (x y) (on-dont-save))
                     (let ([now (make-object button% 
                                  (string-constant save)
                                  button-panel
                                  (lambda (x y) (on-save-now))
                                  (if can-save-now?
                                      '(border)
                                      '()))]
                           [cancel (make-object button%
                                     (string-constant cancel)
                                     button-panel
                                     (lambda (x y) (on-cancel))
                                     (if can-save-now?
                                         '()
                                         '(border)))])
                       (if can-save-now?
                           (send now focus)
                           (begin (send cancel focus)
                                  (send now show #f)))))
                   
                   (center 'both)
                   
                   (show #t)))])
         (make-object unsaved-dialog%)
         result)]))
  
  (define get-choice
    (opt-lambda (message 
                 true-choice
                 false-choice 
                 (title (string-constant warning))
                 (default-result 'disallow-close)
                 (parent #f))
      (letrec ([result default-result]
               [dialog (make-object 
                           (class dialog%
                             (rename [super-on-close on-close]
                                     [super-can-close? can-close?])
                             (define/override (can-close?)
                               (cond
                                 [(eq? default-result 'disallow-close)
                                  (bell)
                                  (message-box title
                                               (format (string-constant please-choose-either)
                                                       true-choice false-choice))
                                  #f]
                                 [else
                                  (super-can-close?)]))
                             (define/override (on-close)
                               (set! result default-result)
                               (super-on-close))
                             (super-make-object title parent)))]
               [on-true
                (lambda args
                  (set! result #t)
                  (send dialog show #f))]
               [on-false
                (lambda rags
                  (set! result #f)
                  (send dialog show #f))]
               [vp (make-object vertical-panel% dialog)]
               [hp (make-object horizontal-panel% dialog)])
        
        (if ((string-length message) . < . 200)
            (let loop ([m message])
              (let ([match (regexp-match (format "^([^~n]*)~n(.*)")
                                         m)])
                (if match
                    (begin (make-object message% (cadr match) vp)
                           (loop (caddr match)))
                    (make-object message% m vp))))
            (let* ([t (make-object text%)]
                   [ec (make-object editor-canvas% vp t)])
              (send ec min-width 400)
              (send ec min-height 200)
              (send t insert message)
              (send t auto-wrap #t)
              (send t lock #t)))
        
        (send vp set-alignment 'left 'center)
        (send hp set-alignment 'right 'center)
        (send (make-object button% true-choice hp on-true '(border)) focus)
        (make-object button% false-choice hp on-false)
        (send hp stretchable-height #f)
        (send dialog center 'both)
        (send dialog show #t)
        result)))
  
  (define text-snip<%> (interface () get-string))
  (define read-snips/chars-from-text
    (letrec ([get-snips
              (lambda (text start end)
                (let* ([pos-box (box 0)]
                       [snip (send text find-snip start 'after-or-none pos-box)])
                  (cond
                    [(not snip) null]
                    [(> (+ (unbox pos-box) (send snip get-count)) end) null]
                    [else (cons snip (get-snips text (+ start (send snip get-count)) end))])))])
      (case-lambda
        [(text) (read-snips/chars-from-text text 0)]
        [(text start) (read-snips/chars-from-text text start (send text last-position))]
        [(text start end)
         (define pos-box (box 0))
         (define (get-next)
           (send text split-snip start)
           (send text split-snip end)
           
           ;; must get all of the snips out of the buffer before reading -- they may change.
           (let loop ([snips (get-snips text start end)])
             
             (cond
               [(null? snips)
                (set! get-next (lambda () eof))
                eof]
               [(or (is-a? (car snips) string-snip%)
                    (is-a? (car snips) text-snip<%>))
                (let ([str (if (is-a? (car snips) text-snip<%>)
                               (send (car snips) get-string)
                               (send (car snips) get-text 0 (send (car snips) get-count)))])
                     (let string-loop ([n 0])
                       (cond
                         [(< n (string-length str))
                          (set! get-next (lambda () (string-loop (+ n 1))))
                          (string-ref str n)]
                         [else
                          (loop (cdr snips))])))]
               [else
                (set! get-next (lambda () (loop (cdr snips))))
                (car snips)])))
         (let ([read-snips/chars-from-text-thunk
                (lambda ()
                  (get-next))])
           read-snips/chars-from-text-thunk)])))
  
  (define open-input-buffer
    (lambda (buffer)
      (let ([pos 0]
            [lock (make-semaphore 1)])
        (make-custom-input-port
         lock
         (lambda (s)
           (if (semaphore-try-wait? lock)
               (dynamic-wind
                void
                (lambda ()
                  (let* ([len (send buffer last-position)]
                         [count (min (string-length s)
                                     (- len pos))])
                    (if (zero? count)
                        eof
                        (let ([got (send buffer get-text pos (+ pos count))])
                          (let loop ([count count])
                            (unless (zero? count)
                              (let ([count (sub1 count)])
                                (string-set! s count (string-ref got count))
                                (loop (sub1 count)))))
                          (set! pos (+ pos count))
                          count))))
                (lambda () (semaphore-post lock)))
               0))
         #f
         void))))
  
  (define repeated-keystroke-timeout 300)
  (define alphabetic-list-box%
    (class list-box%
      (init-field choices callback)
      
      (field (chars null)
             (last-time-stamp #f))
      
      (rename [super-on-subwindow-event on-subwindow-event]
              [super-on-subwindow-char on-subwindow-char])
      (define/override (on-subwindow-event receiver evt)
        (set! chars null)
        (super-on-subwindow-event receiver evt))
      (define/override (on-subwindow-char receiver evt)
        (let ([code (send evt get-key-code)])
          (when (or (not (char? code))
                    (and last-time-stamp
                         ((- (send evt get-time-stamp) last-time-stamp)
                          . >= .
                          repeated-keystroke-timeout)))
            (set! chars null))
          (set! last-time-stamp (send evt get-time-stamp))
          (cond
            [(and (char? code) (or (char-alphabetic? code) (char-numeric? code)))
             (set! chars (cons code chars))
             (scroll-to-matching)
             (callback this (instantiate control-event% () 
                              (event-type 'list-box)
                              (time-stamp (send evt get-time-stamp))))]
            [else
             (set! chars null)
             (super-on-subwindow-char receiver evt)])))
      
      ;; scroll-to-matching : -> void
      ;; scrolls the list box to the first string
      ;; that matches the typed chars in `chars'.
      (define (scroll-to-matching)
        (let* ([typed-str (apply string (reverse chars))]
               [typed-len (string-length typed-str)])
          (let loop ([n 0])
            (when (< n (get-number))
              (let ([str (get-string n)])
                (cond
                  [(and ((string-length str) . >= . typed-len)
                        (string=? typed-str (substring str 0 typed-len)))
                   (set-selection n)
                   (make-visible n)]
                  [else (loop (+ n 1))]))))))
      (inherit get-number get-string set-selection)
      
      ;; make-visible : number -> void
      ;; scrolls the list box so that the nth item is visible,
      ;; unless the n'th item is already visible, in which case
      ;; it does nothing.
      (define (make-visible n)
        (set-first-visible-item n))
      (inherit set-first-visible-item)
      
      (super-instantiate ()
        (choices choices)
        (callback callback))))
  
  ;; manual renaming
  (define gui-utils:next-untitled-name next-untitled-name)
  (define gui-utils:show-busy-cursor show-busy-cursor)
  (define gui-utils:delay-action delay-action)
  (define gui-utils:local-busy-cursor local-busy-cursor)
  (define gui-utils:unsaved-warning unsaved-warning)
  (define gui-utils:read-snips/chars-from-text read-snips/chars-from-text)
  (define gui-utils:get-choice get-choice)
  (define gui-utils:open-input-buffer open-input-buffer)
  (define gui-utils:get-clicked-clickback-delta get-clicked-clickback-delta)
  (define gui-utils:get-clickback-delta get-clickback-delta)
  (define gui-utils:text-snip<%> text-snip<%>))