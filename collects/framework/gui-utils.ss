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
   
   (gui-utils:cancel-on-right?
    (-> boolean?)
    ()
    "Returns \\scheme{#t} if cancel should be on the right-hand side (or below)"
    "in a dialog and \\scheme{#f} otherwise."
    ""
    "See also"
    "@flink gui-utils:ok/cancel-buttons %"
    ".")
   (gui-utils:ok/cancel-buttons
    (opt->*
     ((is-a?/c area-container<%>)
      ((is-a?/c button%) (is-a?/c event%) . -> . any)
      ((is-a?/c button%) (is-a?/c event%) . -> . any))
     (string?
      string?)
     ((is-a?/c button%)
      (is-a?/c button%)))
    ((parent
      confirm-callback
      cancel-callback)
     ((confirm-label (string-constant ok))
      (cancel-label (string-constant cancel))))
    "Adds an Ok and a cancel button to a panel, changing the order"
    "to suit the platform. Under \\MacOSBoth{} and unix, the confirmation action"
    "is on the right (or bottom) and under Windows, the canceling action is on the"
    "right (or bottom)."
    "The confirmation action button has the \\scheme|'(border)| style."
    "The buttons are also sized to be the same width."
    ""
    "The first result is be the OK button and the second is"
    "the cancel button."
    ""
    "See also"
    "@flink gui-utils:cancel-on-right? %"
    ".")
   
   (gui-utils:next-untitled-name
    (-> string?)
    ()
    "Returns a name for the next opened untitled frame. The first"
    "name is ``Untitled'', the second is ``Untitled 2'',"
    "the third is ``Untitled 3'', and so forth.")
   (gui-utils:cursor-delay
    (case->
     (-> real?)
     (real? . -> . void?))
    (() (new-delay))
    "This function is {\\em not\\/} a parameter."
    "Instead, the state is just stored in the closure."
    ""
    "The first case in the case lambda"
    "returns the current delay in seconds before a watch cursor is shown,"
    "when either \\iscmprocedure{gui-utils:local-busy-cursor} or"
    "\\iscmprocedure{gui-utils:show-busy-cursor} is called."

    "The second case in the case lambda"
    "Sets the delay, in seconds, before a watch cursor is shown, when"
    "either \\iscmprocedure{gui-utils:local-busy-cursor} or"
    "\\iscmprocedure{gui-utils:show-busy-cursor} is called.")
   (gui-utils:show-busy-cursor
    (opt->
     ((-> any?))
     (integer?)
     any?)
    ((thunk)
     ((delay (gui-utils:cursor-delay))))
    "Evaluates \\rawscm{(\\var{thunk})} with a watch cursor. The argument"
    "\\var{delay} specifies the amount of time before the watch cursor is"
    "opened. Use \\iscmprocedure{gui-utils:cursor-delay} to set this value"
    "to all calls."
    ""
    "This function returns the result of \\var{thunk}.")
   (gui-utils:delay-action
    (real?
     (-> void?)
     (-> void?)
     . -> .
     void?)
    (delay-time open close)
    "Use this function to delay an action for some period of time. It also"
    "supports cancelling the action before the time period elapses. For"
    "example, if you want to display a watch cursor, but you only want it"
    "to appear after 2 seconds and the action may or may not take more than"
    "two seconds, use this pattern:"
    ""
    "\\begin{schemedisplay}"
    "(let ([close-down"
    "       (gui-utils:delay-action"
    "        2"
    "        (lambda () .. init watch cursor ...)"
    "        (lambda () .. close watch cursor ...))])"
    "  ;; .. do action ..."
    "  (close-down))"
    "\\end{schemedisplay}"
    ""
    "Creates a thread that waits \\var{delay-time}. After \\var{delay-time}"
    "has elapsed, if the result thunk has {\\em not} been called, call"
    "\\var{open}. Then, when the result thunk is called, call"
    "\\var{close}. The function \\var{close} will only be called if"
    "\\var{open} has been called.")

   (gui-utils:local-busy-cursor
    (opt->
     ((is-a?/c window<%>)
      (-> any?))
     (integer?)
     any?)
    ((window thunk)
     ((delay (gui-utils:cursor-delay))))
    "Evaluates \\rawscm{(\\var{thunk})} with a watch cursor in \\var{window}. If"
    "\\var{window} is \\rawscm{\\#f}, the watch cursor is turned on globally. The"
    "argument \\var{delay} specifies the amount of time before the watch"
    "cursor is opened. Use "
    "@flink gui-utils:cursor-delay "
    "to set this value for all uses of this function."
    ""
    "The result of this function is the result of \\var{thunk}.")

   (gui-utils:unsaved-warning
    (opt->
     (string?
      string?)
     (boolean?
      (union false?
	     (is-a?/c frame%)
	     (is-a?/c dialog%)))
     (symbols 'continue 'save 'cancel))
    ((filename action)
     ((can-save-now? #f)
      (parent #f)))

    "This displays a dialog that warns the user of a unsaved file."
    ""
    "The string, \\var{action}, indicates what action is about to"
    "take place, without saving. For example, if the application"
    "is about to close a file, a good action is \\rawscm{\"Close"
    "Anyway\"}. The result symbol indicates the user's choice. If"
    "\\var{can-save-now?} is \\rawscm{\\#f}, this function does not"
    "give the user the ``Save'' option and thus will not return"
    "\rawscm{'save}.")

   (gui-utils:get-choice
    (opt->
     (string?
      string?
      string?)
     (string?
      any?
      (union false? (is-a?/c frame%) (is-a?/c dialog%)))
     any?)
    ((message true-choice false-choice)
     ((title "Warning")
      (default-result 'disallow-close)
      (paren #f)))

    "Opens a dialog that presents a binary choice to the user. The user is forced"
    "to choose between these two options, ie cancelling or closing the dialog"
    "opens a message box asking the user to actually choose one of the two options."
    ""
    "The dialog will contain the string \\var{message} and two buttons,"
    "labeled with the \\var{true-choice} and the \\var{false-choice}.  If the"
    "user clicks on \\var{true-choice} \\rawscm{\\#t} is returned. If the user"
    "clicks on \\var{false-choice}, \\rawscm{\\#f} is returned."
    ""
    "The argument \\var{default-result} determines how closing the window is"
    "treated. If the argument is \rawscm{'disallow-close}, closing the window"
    "is not allowed. If it is anything else, that value is returned when"
    "the user closes the window."
    ""
    "If "
    "@flink gui-utils:cancel-on-right?"
    "returns \\scheme|#t|, the false choice is on the right."
    "Otherwise, the true choice is on the right.")

   (gui-utils:get-clicked-clickback-delta
    (-> (is-a?/c style-delta%))
    ()
    "This delta is designed for use with"
    "@link text set-clickback %"
    ". Use it as one of the \\iscmclass{style-delta} argument to"
    "@link text set-clickback %"
    "."
    ""
    "See also"
    "@flink gui-utils:get-clickback-delta %"
    ".")

   (gui-utils:get-clickback-delta
    (-> (is-a?/c style-delta%))
    ()
    "This delta is designed for use with"
    "@link text set-clickback %"
    ". Use the result of this function as the style"
    "for the region"
    "text where the clickback is set." 
    ""
    "See also"
    "@flink gui-utils:get-clicked-clickback-delta %"
    "."))
  
  (define (cancel-on-right?) (eq? (system-type) 'windows))
  
  (define ok/cancel-buttons
    (opt-lambda (parent 
                 confirm-callback
                 cancel-callback 
                 [confirm-str (string-constant ok)]
                 [cancel-str (string-constant cancel)])
      (let ([confirm (lambda ()
                       (instantiate button% ()
                         (parent parent)
                         (callback confirm-callback)
                         (label confirm-str)
                         (style '(border))))]
            [cancel (lambda ()
                      (instantiate button% ()
                        (parent parent)
                        (callback cancel-callback)
                        (label cancel-str)))])
        (let-values ([(b1 b2)
                      (cond
                        [(cancel-on-right?)
                         (values (confirm) (cancel))]
                        [else
                         (values (cancel) (confirm))])])
          (let ([w (max (send b1 get-width)
                        (send b2 get-width))])
            (send b1 min-width w)
            (send b2 min-width w)
	    (if (cancel-on-right?)
		(values b1 b2)
		(values b2 b1)))))))

  
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
    (opt-lambda (thunk [delay (cursor-delay)])
      (local-busy-cursor #f thunk delay)))
  
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
    (opt-lambda (filename action-anyway (can-save-now? #f) (parent #f))
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
                        (make-object horizontal-panel% panel)]
                       [anyway (make-object button% 
                                 (string-append action-anyway)
                                 button-panel
                                 (lambda (x y) (on-dont-save)))]
                       [now (make-object button% 
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
                    (send button-panel change-children
                          (lambda (l)
                            (if (cancel-on-right?)
                                (list anyway now cancel)
                                (list anyway cancel now))))
                    (if can-save-now?
                        (send now focus)
                        (begin (send cancel focus)
                               (send now show #f)))
                  
                  (center 'both)
                  
                  (show #t)))])
        (make-object unsaved-dialog%)
        result)))
  
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
	      (send t set-position 0)
              (send t auto-wrap #t)
              (send t lock #t)))
        
        (send vp set-alignment 'left 'center)
        (send hp set-alignment 'right 'center)
        (let ([make-true
               (lambda ()
                 (send (make-object button% true-choice hp on-true '(border)) focus))]
              [make-false
               (lambda ()
                 (make-object button% false-choice hp on-false))])
          (if (cancel-on-right?)
              (begin (make-true) (make-false))
              (begin (make-false) (make-true))))
        (send hp stretchable-height #f)
        (send dialog center 'both)
        (send dialog show #t)
        result)))
 
  
  ;; manual renaming
  (define gui-utils:next-untitled-name next-untitled-name)
  (define gui-utils:show-busy-cursor show-busy-cursor)
  (define gui-utils:delay-action delay-action)
  (define gui-utils:local-busy-cursor local-busy-cursor)
  (define gui-utils:unsaved-warning unsaved-warning)
  (define gui-utils:get-choice get-choice)
  (define gui-utils:get-clicked-clickback-delta get-clicked-clickback-delta)
  (define gui-utils:get-clickback-delta get-clickback-delta)
  (define gui-utils:ok/cancel-buttons ok/cancel-buttons)
  (define gui-utils:cancel-on-right? cancel-on-right?))