
(module frame mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "include.ss")
	   "sig.ss"
	   "../gui-utils.ss"
	   "../macro.ss"
           "bday.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "list.ss")
	   (lib "file.ss")
	   (lib "etc.ss"))
  
  (provide frame@)
  
  (define frame@
    (unit/sig framework:frame^
      (import mred^
              [group : framework:group^]
              [preferences : framework:preferences^]
              [icon : framework:icon^]
              [handler : framework:handler^]
              [application : framework:application^]
              [panel : framework:panel^]
              [finder : framework:finder^]
              [keymap : framework:keymap^]
              [text : framework:text^]
              [pasteboard : framework:pasteboard^]
              [editor : framework:editor^]
              [canvas : framework:canvas^]
              [menu : framework:menu^]
              [scheme : framework:scheme^]
              [exit : framework:exit^]
              [comment-box : framework:comment-box^])
      
      (rename [-editor<%> editor<%>]
              [-pasteboard% pasteboard%]
              [-text% text%])
      
      (define (reorder-menus frame)
        (let* ([items (send (send frame get-menu-bar) get-items)]
               [move-to-back
                (lambda (name items)
                  (let loop ([items items]
                             [back null])
                    (cond
                      [(null? items) back]
                      [else (let ([item (car items)])
                              (if (string=? (send item get-plain-label) name)
                                  (loop (cdr items)
                                        (cons item back))
                                  (cons item (loop (cdr items) back))))])))]
               [move-to-front
                (lambda (name items)
                  (reverse (move-to-back name (reverse items))))]
               [re-ordered
                (move-to-front
                 (string-constant file-menu)
                 (move-to-front
                  (string-constant edit-menu)
                  (move-to-back
                   (string-constant help-menu)
                   (move-to-back
                    (string-constant windows-menu)
                    items))))])
          (for-each (lambda (item) (send item delete)) items)
          (for-each (lambda (item) (send item restore)) re-ordered)))
      
      (define (add-snip-menu-items edit-menu c%)
        (let* ([get-edit-target-object
                (lambda ()
                  (let ([menu-bar
                         (let loop ([p (send edit-menu get-parent)])
                           (cond
                             [(is-a? p menu-bar%)
                              p]
                             [(is-a? p menu%)
                              (loop (send p get-parent))]
                             [else #f]))])
                    (and menu-bar
                         (let ([frame (send menu-bar get-frame)])
                           (send frame get-edit-target-object)))))]
               [edit-menu:do 
                (lambda (const)
                  (lambda (menu evt)
                    (let ([edit (get-edit-target-object)])
                      (when (and edit
                                 (is-a? edit editor<%>))
                        (send edit do-edit-operation const)))
                    #t))]
               [on-demand
                (lambda (menu-item)
                  (let ([edit (get-edit-target-object)])
                    (send menu-item enable (and edit (is-a? edit editor<%>)))))]
               [insert-comment-box
                 (lambda ()
                   (let ([text (get-edit-target-object)])
                     (when text
                       (let ([snip (make-object comment-box:snip%)])
                         (send text insert snip)
                         (send text set-caret-owner snip 'global)))))])
          
          (make-object c% (string-constant insert-comment-box-menu-item-label)
            edit-menu 
            (lambda (x y) (insert-comment-box))
            #f #f
            on-demand)
          (make-object c% (string-constant insert-image-item)
            edit-menu (edit-menu:do 'insert-image) #f #f on-demand)
          (void)))
      
      (define frame-width 600)
      (define frame-height 650)
      (let ([window-trimming-upper-bound-width 20]
            [window-trimming-upper-bound-height 50])
        (let-values ([(w h) (get-display-size)])
          (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
          (set! frame-height (min frame-height (- h window-trimming-upper-bound-height)))))
      
      (define basic<%> (interface ((class->interface frame%))
                         get-area-container%
                         get-area-container
                         get-menu-bar%
                         make-root-area-container
                         close
                         get-filename))
      (define basic-mixin
        (mixin ((class->interface frame%)) (basic<%>)
        
          (rename [super-show show])
          (define/override (show on?)
            (if on?
                (send (group:get-the-frame-group) insert-frame this)
                (send (group:get-the-frame-group) remove-frame this))
            (super-show on?))
          
          (define/override (can-exit?)
            (exit:set-exiting #t)
            (let ([res (exit:can-exit?)])
              (unless res
                (exit:set-exiting #f))
              res))
          (define/override (on-exit) 
            (exit:on-exit)
            (queue-callback
             (lambda ()
               (exit)
               (exit:set-exiting #f))))
          
          (public get-filename)
          [define get-filename
             (case-lambda
              [() (get-filename #f)]
              [(b) #f])]

	  (override on-superwindow-show)
	  (rename [super-on-superwindow-show on-superwindow-show])
	  (define (on-superwindow-show shown?)
	    (send (group:get-the-frame-group) frame-shown/hidden this)
	    (super-on-superwindow-show shown?))

          (rename [super-can-close? can-close?]
                  [super-on-close on-close]
                  [super-on-focus on-focus])
          
          (define after-init? #f)
          (override can-close? on-close on-focus on-drop-file)
          (define (can-close?)
            (let ([number-of-frames 
                   (length (send (group:get-the-frame-group)
                                 get-frames))])
              (if (preferences:get 'framework:exit-when-no-frames)
                  (and (super-can-close?)
                       (or (exit:exiting?)
                           (not (= 1 number-of-frames))
                           (exit:user-oks-exit)))
                  #t)))
          (define (on-close)
            (super-on-close)
            (send (group:get-the-frame-group)
                  remove-frame
                  this)
            (when (preferences:get 'framework:exit-when-no-frames)
              (unless (exit:exiting?)
                (when (null? (send (group:get-the-frame-group) get-frames))
                  (exit:exit)))))
          
          (define (on-focus on?)
            (super-on-focus on?)
            (when on?
              (send (group:get-the-frame-group) set-active-frame this)))
          
          [define on-drop-file
            (lambda (filename)
              (handler:edit-file filename))]
          
          ;; added call to set label here to hopefully work around a problem in mac mred
          (inherit set-label change-children)
          (override after-new-child)
          [define after-new-child
            (lambda (child)
              (when after-init?
                (change-children (lambda (l) (remq child l)))
                (error 'frame:basic-mixin
                       "do not add children directly to a frame:basic (unless using make-root-area-container); use the get-area-container method instead"
                       )))]
          
          (public get-area-container% get-menu-bar% make-root-area-container close)
          [define get-area-container% (lambda () vertical-panel%)]
          [define get-menu-bar% (lambda () menu-bar%)]
          [define make-root-area-container
            (lambda (% parent)
              (make-object % parent))]
          [define close
           (lambda ()
             (when (can-close?)
               (on-close)
               (show #f)))]
          
          (inherit accept-drop-files)

	  (super-instantiate ())

          (accept-drop-files #t)
            
	  (let ([mb (make-object (get-menu-bar%) this)])
	    (when (or (eq? (system-type) 'macos)
		      (eq? (system-type) 'macosx))
	      (make-object menu:can-restore-underscore-menu% (string-constant windows-menu-label)
			   mb)))
          
          (reorder-menus this)
	  (send (group:get-the-frame-group) insert-frame this)
	  
          [define panel (make-root-area-container (get-area-container%) this)]
          (public get-area-container)
          [define get-area-container (lambda () panel)]
          (set! after-init? #t)))
      
      (define locked-message (string-constant read-only))
      (define unlocked-message (string-constant read/write))
      
      (define lock-canvas%
        (class canvas%
          (field [locked? #f])
          (define/public (set-locked l)
            (set! locked? l)
            (on-paint))
          (inherit get-client-size get-dc)
          (define/override (on-paint)
            (let* ([dc (get-dc)]
                   [draw
                    (lambda (str bg-color bg-style line-color line-style)
                      (send dc set-font (send (get-parent) get-label-font))
                      (let-values ([(w h) (get-client-size)]
                                   [(tw th ta td) (send dc get-text-extent str)])
                        (send dc set-pen (send the-pen-list find-or-create-pen line-color 1 line-style))
                        (send dc set-brush (send the-brush-list find-or-create-brush bg-color bg-style))
                        (send dc draw-rectangle 0 0 w h)
                        (send dc draw-text str
                              (- (/ w 2) (/ tw 2))
                              (- (/ h 2) (/ th 2)))))])
              (if locked?
                  (draw locked-message "yellow" 'solid "black" 'solid)
                  (draw unlocked-message (get-panel-background) 'panel (get-panel-background) 'transparent))))
          (inherit get-parent min-width min-height stretchable-width stretchable-height)

	  (super-instantiate ())

          (let ([dc (get-dc)])
            (send dc set-font (send (get-parent) get-label-font))
            (let-values ([(w1 h1 _1 _2) (send dc get-text-extent locked-message)]
                         [(w2 h2 _3 _4) (send dc get-text-extent unlocked-message)])
              (stretchable-width #f)
              (stretchable-height #t)
              (min-width (inexact->exact (floor (max w1 w2))))
              (min-height (inexact->exact (floor (+ 4 (max h1 h2)))))))))

      (define status-line<%>
        (interface (basic<%>)
          open-status-line
          close-status-line
          update-status-line))
      
      ;; status-line : (make-status-line symbol number)
      (define-struct status-line (id count))
      
      ;; status-line-msg : (make-status-line-msg (is-a?/c message%) (union symbol #f))
      (define-struct status-line-msg (message id))
      
      (define status-line-mixin
        (mixin (basic<%>) (status-line<%>)
          (field [status-line-container-panel #f]
                 
                 ;; status-lines : (listof status-line)
                 [status-lines null]

                 ;; status-line-msgs : (listof status-line-msg)
                 [status-line-msgs null])
          (rename [super-make-root-area-container make-root-area-container])
          (define/override (make-root-area-container % parent)
            (let* ([s-root (super-make-root-area-container vertical-panel% parent)]
                   [r-root (make-object % s-root)])
              (set! status-line-container-panel
                    (instantiate vertical-panel% ()
                      (parent s-root)
                      (stretchable-height #f)))
              r-root))
          (define/public (open-status-line id)
            (do-main-thread
             (lambda ()
               (when status-line-container-panel
                 (set! status-lines
                       (let loop ([status-lines status-lines])
                         (cond
                           [(null? status-lines)
                            (list (make-status-line id 1))]
                           [else (let ([status-line (car status-lines)])
                                   (if (eq? id (status-line-id status-line))
                                       (cons (make-status-line id (+ (status-line-count status-line) 1))
                                             (cdr status-lines))
                                       (cons status-line (loop (cdr status-lines)))))])))))))
          
          (define/public (close-status-line id)
            (do-main-thread
             (lambda ()
               (when status-line-container-panel
                 
                 ;; decrement counter in for status line, or remove it if
                 ;; counter goes to zero.
                 (set! status-lines
                       (let loop ([status-lines status-lines])
                         (cond
                           [(null? status-lines) (error 'close-status-line "status line not open ~e" id)]
                           [else (let* ([status-line (car status-lines)]
                                        [this-line? (eq? (status-line-id status-line) id)])
                                   (cond
                                     [(and this-line? (= 1 (status-line-count status-line)))
                                      (cdr status-lines)]
                                     [this-line?
                                      (cons (make-status-line id (- (status-line-count status-line) 1))
                                            (cdr status-lines))]
                                     [else (cons status-line (loop (cdr status-lines)))]))])))
                 
                 ;; make sure that there are only as many messages as different status lines, in total
                 (let ([status-line-msg (find-status-line-msg id)])
                   (when status-line-msg
                     (send (status-line-msg-message status-line-msg) set-label "")
                     (set-status-line-msg-id! status-line-msg #f)))
                 (let* ([msgs-that-can-be-removed (filter (lambda (x) (not (status-line-msg-id x))) status-line-msgs)]
                        [max-to-include (length status-lines)]
                        [msgs-to-remove
                         (let loop ([n max-to-include]
                                    [l msgs-that-can-be-removed])
                           (cond
                             [(null? l) l]
                             [(zero? n) l]
                             [else (loop (- n 1) (cdr l))]))])
                   (send status-line-container-panel
                         change-children
                         (lambda (old-children)
                           (foldl (lambda (status-line-msg l)
                                    (remq (status-line-msg-message status-line-msg) l))
                                  old-children
                                  msgs-to-remove)))
                   (set! status-line-msgs
                         (let loop ([l msgs-to-remove]
                                    [status-line-msgs status-line-msgs])
                           (cond
                             [(null? l) status-line-msgs]
                             [else (loop (cdr l)
                                         (remq (car l) status-line-msgs))]))))))))

          ;; update-status-line : symbol (union #f string)
          (define/public (update-status-line id msg-txt)
            (do-main-thread
             (lambda ()
               (unless (open-status-line? id)
                 (error 'update-status-line "unknown id ~e, other arg ~e" id msg-txt))
               (if msg-txt
                   (cond
                     [(find-status-line-msg id)
                      =>
                      (lambda (existing-status-line-msg)
                        (let ([msg (status-line-msg-message existing-status-line-msg)])
                          (unless (equal? (send msg get-label) msg-txt)
                            (send msg set-label msg-txt))))]
                     [(find-available-status-line-msg)
                      =>
                      (lambda (available-status-line-msg)
                        (send (status-line-msg-message available-status-line-msg) set-label msg-txt)
                        (set-status-line-msg-id! available-status-line-msg id))]
                     [else
                      (set! status-line-msgs
                            (cons (make-new-status-line-msg id msg-txt)
                                  status-line-msgs))])
                   (let ([status-line-msg (find-status-line-msg id)])
                     (when status-line-msg
                       (send (status-line-msg-message status-line-msg) set-label "")
                       (set-status-line-msg-id! status-line-msg #f)))))))
          
          ;; open-status-line? : symbol -> boolean
          (define (open-status-line? id)
            (let loop ([status-lines status-lines])
              (cond
                [(null? status-lines) #f]
                [else
                 (let ([status-line (car status-lines)])
                   (or (eq? (status-line-id status-line) id)
                       (loop (cdr status-lines))))])))

          ;; find-available-status-line-msg : -> (union #f status-line-msg)
          (define/private (find-available-status-line-msg)
            (let loop ([status-line-msgs status-line-msgs])
              (cond
                [(null? status-line-msgs) #f]
                [else (let ([status-line-msg (car status-line-msgs)])
                        (if (status-line-msg-id status-line-msg)
                            (loop (cdr status-line-msgs))
                            status-line-msg))])))

          ;; find-status-line-msg : symbol -> (union #f status-line-msg)
          (define/private (find-status-line-msg id)
            (let loop ([status-line-msgs status-line-msgs])
              (cond
                [(null? status-line-msgs) #f]
                [else (let ([status-line-msg (car status-line-msgs)])
                        (if (eq? id (status-line-msg-id status-line-msg))
                            status-line-msg
                            (loop (cdr status-line-msgs))))])))
          
          ;; make-new-status-line-msg : symbol string -> status-line-msg
          (define/private (make-new-status-line-msg id msg-txt)
            (make-status-line-msg
             (instantiate message% ()
               (parent status-line-container-panel)
               (stretchable-width #t)
               (label msg-txt))
             id))
                    
          (field [eventspace-main-thread (current-thread)]) ;; replace by using new primitive in 203.5 called eventspace-main-thread
          (inherit get-eventspace)
          (define/private (do-main-thread t)
            (if (eq? (current-thread) eventspace-main-thread)
                (t)
                (parameterize ([current-eventspace (get-eventspace)])
                  ;; need high priority callbacks to ensure ordering wrt other callbacks
                  (queue-callback t #t))))
          
	  (super-instantiate ())))
      
      (define info<%> (interface (basic<%>)
                        determine-width
                        lock-status-changed
                        update-info
                        set-info-canvas
                        get-info-canvas
                        get-info-editor
                        get-info-panel
                        show-info
                        hide-info))
      
      (define magic-space 25)

      (define info-mixin
        (mixin (basic<%>) (info<%>)
          (rename [super-make-root-area-container make-root-area-container])
          [define rest-panel 'uninitialized-root]
          [define super-root 'uninitialized-super-root]
          (override make-root-area-container)
          [define make-root-area-container
            (lambda (% parent)
              (let* ([s-root (super-make-root-area-container
                              vertical-panel%
                              parent)]
                     [r-root (make-object % s-root)])
                (set! super-root s-root)
                (set! rest-panel r-root)
                r-root))]
          
          [define info-canvas #f]
          (public get-info-canvas set-info-canvas get-info-editor)
          [define get-info-canvas
           (lambda ()
             info-canvas)]
          [define set-info-canvas
           (lambda (c)
             (set! info-canvas c))]
          [define get-info-editor
           (lambda ()
             (and info-canvas
                  (send info-canvas get-editor)))]
          
          (public determine-width)
          [define determine-width
            (lambda (string canvas edit)
              (send edit set-autowrap-bitmap #f)
              (send canvas call-as-primary-owner
                    (lambda ()
                      (let ([lb (box 0)]
                            [rb (box 0)])
                        (send edit erase)
                        (send edit insert string)
                        (send edit position-location 
                              (send edit last-position)
                              rb)
                        (send edit position-location 0 lb)
                        (send canvas min-width 
                              (+ magic-space (- (inexact->exact (floor (unbox rb)))
                                                (inexact->exact (floor (unbox lb))))))))))]
          
          (rename [super-on-close on-close])
          [define outer-info-panel 'top-info-panel-uninitialized]
          (define/public (hide-info)
            (send super-root change-children
                  (lambda (l)
                    (list rest-panel))))
          (define/public (show-info)
            (send super-root change-children
                  (lambda (l)
                    (list rest-panel outer-info-panel))))
          [define close-panel-callback
            (preferences:add-callback
             'framework:show-status-line
             (lambda (p v)
               (if v 
                   (register-gc-blit)
                   (unregister-collecting-blit gc-canvas))
               (if v
                   (show-info)
                   (hide-info))))]
          [define memory-cleanup void] ;; only for CVSers; used with memory-text
          (override on-close)
          [define on-close
            (lambda ()
              (super-on-close)
              (unregister-collecting-blit gc-canvas)
              (close-panel-callback)
              (memory-cleanup))]
          
          [define icon-currently-locked? 'uninit]
          (public lock-status-changed)
          [define lock-status-changed
            (lambda ()
              (let ([info-edit (get-info-editor)])
                (cond
                  [(not (object? lock-canvas))
                   (void)]
                  [info-edit
                   (unless (send lock-canvas is-shown?)
                     (send lock-canvas show #t))
                   (let ([locked-now? (send info-edit is-locked?)])
                     (unless (eq? locked-now? icon-currently-locked?)
                       (set! icon-currently-locked? locked-now?)
                       (when (object? lock-canvas)
                         (send lock-canvas set-locked locked-now?))))]
                  [else
                   (when (send lock-canvas is-shown?)
                     (send lock-canvas show #f))])))]
          
          (public update-info)
          [define update-info
            (lambda ()
              (lock-status-changed))]
          
	  (super-instantiate ())
          (set! outer-info-panel (make-object horizontal-panel% super-root))
          (send outer-info-panel stretchable-height #f)
          
          [define info-panel (make-object horizontal-panel% outer-info-panel)]
          (make-object grow-box-spacer-pane% outer-info-panel)
          (public get-info-panel)
          [define get-info-panel
            (lambda ()
              info-panel)]
          (public update-memory-text)
          [define update-memory-text
            (lambda ()
              (when show-memory-text?
                (send memory-text begin-edit-sequence)
                (send memory-text lock #f)
                (send memory-text erase)
                (send memory-text insert (format-number (current-memory-use)))
                (send memory-text lock #t)
                (send memory-text end-edit-sequence)))]
          
          (define (format-number n)
            (let loop ([n n])
              (cond
                [(<= n 1000) (number->string n)]
                [else
                 (string-append 
                  (loop (quotient n 1000))
                  ","
                  (pad-to-3 (modulo n 1000)))])))

          (define (pad-to-3 n)
            (cond
              [(<= n 9) (format "00~a" n)]
              [(<= n 99) (format "0~a" n)]
              [else (number->string n)]))
          
            ; only for CVSers
          (when show-memory-text?
            (let* ([panel (make-object horizontal-panel% (get-info-panel) '(border))]
                   [button (make-object button% (string-constant collect-button-label) panel 
                             (lambda x
                               (collect-garbage)
                               (update-memory-text)))]
                   [ec (make-object editor-canvas% panel memory-text '(no-hscroll no-vscroll))])
              (determine-width "0,000,000,000" ec memory-text)
              (update-memory-text)
              (set! memory-cleanup
                    (lambda ()
                      (send ec set-editor #f)))
              (send panel stretchable-width #f)))

          [define lock-canvas (make-object lock-canvas% (get-info-panel))]
          [define gc-canvas (make-object bday-click-canvas% (get-info-panel) '(border))]
          [define register-gc-blit
            (lambda ()
              (let ([onb (icon:get-gc-on-bitmap)]
                    [offb (icon:get-gc-off-bitmap)])
                (when (and (send onb ok?)
                           (send offb ok?))
                  (register-collecting-blit gc-canvas 
                                            0 0
                                            (send onb get-width)
                                            (send onb get-height)
                                            onb offb))))]
          
          (unless (preferences:get 'framework:show-status-line)
            (send super-root change-children
                  (lambda (l)
                    (list rest-panel))))
          (register-gc-blit)
            
          (let* ([gcb (icon:get-gc-on-bitmap)]
                 [gc-width (if (send gcb ok?)
                               (send gcb get-width)
                               10)]
                 [gc-height (if (send gcb ok?)
                                (send gcb get-height)
                                10)])
            (send* gc-canvas
              (min-client-width (max (send gc-canvas min-width) gc-width))
              (min-client-height (max (send gc-canvas min-height) gc-height))
              (stretchable-width #f)
              (stretchable-height #f)))
          (send* (get-info-panel) 
            (set-alignment 'right 'center)
            (stretchable-height #f)
            (spacing 3)
            (border 3))))
      
      (define text-info<%> (interface (info<%>)
                             set-macro-recording
                             overwrite-status-changed
                             anchor-status-changed
                             editor-position-changed))
      (define text-info-mixin
        (mixin (info<%>) (text-info<%>)
          (inherit get-info-editor)
          (rename [super-on-close on-close])
          [define remove-first
            (preferences:add-callback
             'framework:col-offsets
             (lambda (p v)
               (editor-position-changed-offset/numbers
                v
                (preferences:get 'framework:display-line-numbers))
               #t))]
          [define remove-second
            (preferences:add-callback
             'framework:display-line-numbers
             (lambda (p v)
               (editor-position-changed-offset/numbers
                (preferences:get 'framework:col-offsets)
                v)
               #t))]          
          (override on-close)
          [define on-close
            (lambda ()
              (super-on-close)
              (remove-first)
              (remove-second))]
          [define last-start #f]
          [define last-end #f]
          [define last-params #f]
          [define editor-position-changed-offset/numbers
            (lambda (offset? line-numbers?)
               (let* ([edit (get-info-editor)]
                      [make-one
                       (lambda (pos)
                         (let* ([line (send edit position-paragraph pos)]
                                [col (find-col edit line pos)])
                           (if line-numbers?
                               (format "~a:~a"
                                       (add1 line)
                                       (if offset?
                                           (add1 col)
                                           col))
                               (format "~a" pos))))])
                 (cond
                   [(not (object? position-canvas))
                    (void)]
                   [edit
                    (unless (send position-canvas is-shown?)
                      (send position-canvas show #t))
                    (let ([start (send edit get-start-position)]
                          [end (send edit get-end-position)])
                      (unless (and last-start
                                   (equal? last-params (list offset? line-numbers?))
                                   (= last-start start)
                                   (= last-end end))
                        (set! last-params (list offset? line-numbers?))
                        (set! last-start start)
                        (set! last-end end)
                        (when (object? position-edit)
                          (send* position-edit
                            (lock #f)
                            (erase)
                            (insert 
                             (if (= start end)
                                 (make-one start)
                                 (string-append (make-one start)
                                                "-"
                                                (make-one end))))
                            (lock #t)))))]
                   [else
                    (when (send position-canvas is-shown?)
                      (send position-canvas show #f))])))]
          
          ;; find-col : text number number -> number
          ;; given a line number and a position, finds the
          ;; column number for that position
          (define/private (find-col text line pos)
            (let ([line-start (send text line-start-position line)])
              (if (= line-start pos)
                  0
                  (let loop ([col 0]
                             [snip (send text find-snip line-start 'after-or-none)])
                    (cond
                      [(and snip (is-a? snip tab-snip%))
                       ;; assume cursor isn't in the middle of the tab snip
                       ;; and that there is no tab array
                       (let ([twb (box 0)])
                         (send text get-tabs #f twb #f)
                         (let ([tw (floor (inexact->exact (unbox twb)))])
                           (loop (+ col (- tw (modulo col tw)))
                                 (send snip next))))]
                      [snip 
                       (let ([snip-position (send text get-snip-position snip)]
                             [snip-length (send snip get-count)])
                         (if (<= snip-position pos (+ snip-position snip-length))
                             (+ col (- pos snip-position))
                             (loop (+ col snip-length)
                                   (send snip next))))]
                      [else
                       col])))))
                  

          [define anchor-last-state? #f]
          [define overwrite-last-state? #f]
          (public anchor-status-changed editor-position-changed overwrite-status-changed set-macro-recording)
          
          (field (macro-recording? #f))
          (define (update-macro-recording-icon)
            (unless (eq? (send macro-recording-message is-shown?)
                         macro-recording?)
              (send macro-recording-message show macro-recording?)))
          (define (set-macro-recording on?)
            (set! macro-recording? on?)
            (update-macro-recording-icon))

          [define anchor-status-changed
           (lambda ()
             (let ([info-edit (get-info-editor)]
                   [failed
                    (lambda ()
                      (unless (eq? anchor-last-state? #f)
                        (set! anchor-last-state? #f)
                        (send anchor-message show #f)))])
               (cond
                 [info-edit
                  (let ([anchor-now? (send info-edit get-anchor)])
                    (unless (eq? anchor-now? anchor-last-state?)
                      (cond
                        [(object? anchor-message)
                         (send anchor-message
                               show
                               anchor-now?)
                         (set! anchor-last-state? anchor-now?)]
                        [else (failed)])))]
                 [else
                  (failed)])))]
            [define editor-position-changed
             (lambda ()
               (editor-position-changed-offset/numbers
                (preferences:get 'framework:col-offsets)
                (preferences:get 'framework:display-line-numbers)))]
            [define overwrite-status-changed
             (lambda ()
               (let ([info-edit (get-info-editor)]
                     [failed
                      (lambda ()
                        (set! overwrite-last-state? #f)
                        (send overwrite-message show #f))])
                 (cond
                   [info-edit
                    (let ([overwrite-now? (send info-edit get-overwrite-mode)])
                      (unless (eq? overwrite-now? overwrite-last-state?)
                        (cond
                          [(object? overwrite-message)
                           (send overwrite-message
                                 show
                                 overwrite-now?)
                           (set! overwrite-last-state? overwrite-now?)]
                          [else
                           (failed)])))]
                   [else
                    (failed)])))]
          (rename [super-update-info update-info])
          (override update-info)
          [define update-info
            (lambda ()
              (super-update-info)
              (update-macro-recording-icon)
              (overwrite-status-changed)
              (anchor-status-changed)
              (editor-position-changed))]
	  (super-instantiate ())

          (inherit get-info-panel)
          
          [define anchor-message
            (make-object message%
              (let ([b (icon:get-anchor-bitmap)])
                (if (and #f (send b ok?))
                    b
                    (string-constant auto-extend-selection)))
              (get-info-panel))]
          [define overwrite-message 
           (make-object message%
             (string-constant overwrite)
             (get-info-panel))]
          [define position-canvas (make-object editor-canvas% (get-info-panel) #f '(no-hscroll no-vscroll))]
          [define position-edit (make-object text%)]
          
          
          (define macro-recording-message
            (instantiate message% ()
              (label "c-x;(")
              (parent (get-info-panel))))
          
          (inherit determine-width)
          (let ([move-front
                 (lambda (x l)
                   (cons x (remq x l)))])
            (send (get-info-panel) change-children
                  (lambda (l)
                    (move-front
                     macro-recording-message
                     (move-front
                      anchor-message
                      (move-front
                       overwrite-message
                       (move-front
                        position-canvas
                        l)))))))
          (send macro-recording-message show #f)
          (send anchor-message show #f)
          (send overwrite-message show #f)
          (send* position-canvas
            (set-line-count 1)
            (set-editor position-edit)
            (stretchable-width #f)
            (stretchable-height #f))
          (determine-width "0000:000-0000:000"
                           position-canvas
                           position-edit)
          (editor-position-changed)
          (send position-edit hide-caret #t)
          (send position-edit lock #t)))
      
      (define pasteboard-info<%> (interface (info<%>)))
      (define pasteboard-info-mixin
        (mixin (basic<%>) (pasteboard-info<%>)
          (super-instantiate ())))
      
      (include "standard-menus.ss")
      
      (define -editor<%> (interface (standard-menus<%>)
                           get-entire-label
                           get-label-prefix
                           set-label-prefix
                           
                           get-canvas%
                           get-canvas<%>
                           get-editor%
                           get-editor<%>
                           
                           make-editor
                           revert
                           save
                           save-as
                           get-canvas
                           get-editor))
      
      (define editor-mixin
        (mixin (standard-menus<%>) (-editor<%>)
          (init (filename #f))

          (inherit get-area-container get-client-size 
                   show get-edit-target-window get-edit-target-object)
          (rename [super-on-close on-close]
                  [super-set-label set-label])
          
          (override get-filename on-close)
          [define get-filename
            (case-lambda
             [() (get-filename #f)]
             [(b)
              (let ([e (get-editor)])
                (and e (send e get-filename b)))])]
          [define on-close
           (lambda ()
             (super-on-close)
             (send (get-editor) on-close))]
          [define label ""]
          [define label-prefix (application:current-app-name)]
          (define (do-label)
            (super-set-label (gui-utils:trim-string (get-entire-label) 200))
            (send (group:get-the-frame-group) frame-label-changed this))
          
          (public get-entire-label get-label-prefix set-label-prefix)
          [define get-entire-label
            (lambda ()
              (cond
                [(string=? "" label)
                 label-prefix]
                [(string=? "" label-prefix)
                 label]
                [else 
                 (string-append label " - " label-prefix)]))]
          [define get-label-prefix (lambda () label-prefix)]
          [define set-label-prefix
           (lambda (s)
             (when (and (string? s)
                        (not (string=? s label-prefix)))
               (set! label-prefix s)
               (do-label)))]
          (override get-label set-label)
          [define get-label (lambda () label)]
          [define set-label
            (lambda (t)
              (when (and (string? t)
                         (not (string=? t label)))
                (set! label t)
                (do-label)))]

          (public get-canvas% get-canvas<%> make-canvas get-editor% get-editor<%> make-editor)
          [define get-canvas% (lambda () editor-canvas%)]
          [define get-canvas<%> (lambda () (class->interface editor-canvas%))]
          [define make-canvas (lambda ()
                                (let ([% (get-canvas%)]
                                      [<%> (get-canvas<%>)])
                                  (unless (implementation? % <%>)
                                    (error 'frame:editor%
                                           "result of get-canvas% method must match ~e interface; got: ~e"
                                           <%> %))
                                  (instantiate % () (parent (get-area-container)))))]
          (define (get-editor%)
	    (error 'editor-frame% "abstract method: no editor% class specified"))
          (define (get-editor<%>)
	    editor<%>)
          (define (make-editor)
	    (let ([% (get-editor%)]
		  [<%> (get-editor<%>)])
	      (unless (implementation? % <%>)
		(error 'frame:editor%
		       "result of get-editor% method must match ~e interface; got: ~e"
		       <%> %))
	      (make-object %)))
          
          (define/public save
            (opt-lambda ([format 'same])
              (let* ([ed (get-editor)]
                     [filename (send ed get-filename)])
                (if filename
                    (send ed save-file/gui-error filename format)
                    (save-as format)))))
          
          (define/public save-as
            (opt-lambda ([format 'same])
              (let* ([editor (get-editor)]
                     [name (send editor get-filename)])
                (let-values ([(base name)
                              (if name 
                                  (let-values ([(base name dir?) (split-path name)])
                                    (values base name))
                                  (values #f #f))])
                  (let ([file (send editor put-file name base)])
                    (if file
                        (send editor save-file/gui-error file format)
                        #f))))))
          
          (define/private (basename str)
            (let-values ([(base name dir?) (split-path str)])
              base))
          
          (inherit get-checkable-menu-item% get-menu-item%)
          (override file-menu:save-callback
                    file-menu:create-save? file-menu:save-as-callback file-menu:create-save-as? 
                    file-menu:print-callback file-menu:create-print?)

          (define/override (file-menu:revert-on-demand item)
            (send item enable (not (send (get-editor) is-locked?))))

          (define/override file-menu:revert-callback 
            (lambda (item control)
              (let* ([edit (get-editor)]
                     [b (box #f)]
                     [filename (send edit get-filename b)])
                (if (or (not filename) 
                        (unbox b))
                    (bell)
                    (when (gui-utils:get-choice
                           (string-constant are-you-sure-revert)
                           (string-constant yes)
                           (string-constant no)
                           (string-constant are-you-sure-revert-title)
                           #f
                           this)
                      (revert))))
              #t))
          
          (define/public (revert)
            (let* ([edit (get-editor)]
                   [b (box #f)]
                   [filename (send edit get-filename b)])
              (when (and filename
                         (not (unbox b)))
                (let ([start
                       (if (is-a? edit text%)
                           (send edit get-start-position)
                           #f)])
                  (send edit begin-edit-sequence)
                  (let ([status (send edit load-file/gui-error
                                      filename
                                      'same
                                      #f)])
                    (if status
                        (begin
                          (when (is-a? edit text%)
                            (send edit set-position start start))
                          (send edit end-edit-sequence))
                        (send edit end-edit-sequence)))))))
          
          (define/override file-menu:create-revert? (lambda () #t))
          (define file-menu:save-callback (lambda (item control)
                                            (save)
                                            #t))
          
          (define file-menu:create-save? (lambda () #t))
          (define file-menu:save-as-callback (lambda (item control) (save-as) #t))
          (define file-menu:create-save-as? (lambda () #t))
          (define file-menu:print-callback (lambda (item control)
                                             (send (get-editor) print
                                                   #t
                                                   #t
                                                   (preferences:get 'framework:print-output-mode))
                                             #t))
          (define file-menu:create-print? (lambda () #t))
          
          (override edit-menu:between-select-all-and-find)
          (define edit-menu:between-select-all-and-find
           (lambda (edit-menu)
             (let* ([c% (get-checkable-menu-item%)]
                    [on-demand
                     (lambda (menu-item)
                       (let ([edit (get-edit-target-object)])
                         (if (and edit (is-a? edit editor<%>))
                             (begin
                               (send menu-item enable #t)
                               (send menu-item check (send edit auto-wrap)))
                             (begin 
                               (send menu-item check #f)
                               (send menu-item enable #f)))))]
                    [callback
                     (lambda (item event)
                       (let ([edit (get-edit-target-object)])
                         (when (and edit
                                    (is-a? edit editor<%>))
                           (let ([new-pref (not (send edit auto-wrap))])
                             (preferences:set 'framework:auto-set-wrap? new-pref)
                             (send edit auto-wrap new-pref)))))])
               (make-object c% (string-constant wrap-text-item)
                 edit-menu callback #f #f on-demand))
             
             (make-object separator-menu-item% edit-menu)))
          
          (override help-menu:about-callback help-menu:about-string help-menu:create-about?)
          (define help-menu:about-callback 
            (lambda (menu evt) 
              (message-box (application:current-app-name)
                           (format (string-constant welcome-to-something)
                                   (application:current-app-name))
                           #f
                           '(ok app))))
          (define help-menu:about-string (lambda () (application:current-app-name)))
          (define help-menu:create-about? (lambda () #t))
          
	  (super-instantiate () (label (get-entire-label)))
          
          (define canvas #f)
          (define editor #f)
          (public get-canvas get-editor)
          (define get-canvas 
            (lambda () 
              (unless canvas
                (set! canvas (make-canvas))
                (send canvas set-editor (get-editor)))
              canvas))
          (define get-editor 
            (lambda () 
              (unless editor
                (set! editor (make-editor))
                (send (get-canvas) set-editor editor))
              editor))

	  (cond
	    [(and filename (file-exists? filename))
	     (let ([ed (get-editor)])
	       (send ed begin-edit-sequence)
	       (send ed load-file/gui-error filename 'guess)
	       (send ed end-edit-sequence))]
	    [filename
	     (send (get-editor) set-filename filename)]
	    [else (void)])

          (let ([ed-fn (send (get-editor) get-filename)])
            (set! label (if ed-fn
                            (or (file-name-from-path ed-fn)
                                (gui-utils:next-untitled-name))
                            (gui-utils:next-untitled-name))))
          (do-label)
          (let ([canvas (get-canvas)])
            (when (is-a? canvas editor-canvas%)
              ;; when get-canvas is overridden,
              ;; it might not yet be implemented
              (send canvas focus)))))
      
      (define open-here<%>
        (interface (-editor<%>)
          get-open-here-editor
	  open-here))
      
      (define open-here-mixin
        (mixin (-editor<%>) (open-here<%>)

          (rename [super-file-menu:new-on-demand file-menu:new-on-demand])
          (define/override (file-menu:new-on-demand item)
            (super-file-menu:new-on-demand item)
            (send item set-label (if (preferences:get 'framework:open-here?)
                                     (string-constant new-...-menu-item)
                                     (string-constant new-menu-item))))
          
          (define/override (file-menu:new-callback item event)
            (cond
              [(preferences:get 'framework:open-here?)
               (let ([clear-current (ask-about-new-here)])
                 (cond
                   [(eq? clear-current 'cancel) (void)]
                   [clear-current
                    (let* ([editor (get-editor)]
                           [canceled? (cancel-due-to-unsaved-changes editor)])
                      (unless canceled?
                        (send editor begin-edit-sequence)
                        (send editor lock #f)
                        (send editor set-filename #f)
                        (send editor erase)
                        (send editor set-modified #f)
                        (send editor clear-undos)
                        (send editor end-edit-sequence)))]
                   [else ((handler:current-create-new-window) #f)]))]
              [else ((handler:current-create-new-window) #f)]))

          ;; cancel-due-to-unsaved-changes : -> boolean
          ;; returns #t if the action should be cancelled
          (define (cancel-due-to-unsaved-changes editor)
            (and (send editor is-modified?)
                 (let ([save (gui-utils:unsaved-warning
                              (or (send editor get-filename) (get-label))
                              (string-constant clear-anyway)
                              #t
                              this)])
                   (case save
                     [(continue) #f]
                     [(save) (not (send editor save-file/gui-error))]
                     [(cancel) #t]))))
          
          ;; ask-about-new-here : -> (union 'cancel boolean?)
          ;; prompts the user about creating a new window
          ;; or "reusing" the current one.
          (define/private (ask-about-new-here)
            (gui-utils:get-choice
             (string-constant create-new-window-or-clear-current)
             (string-constant clear-current)
             (string-constant new-window)
             (string-constant warning)
             'cancel
             this))
 
          (rename [super-file-menu:open-on-demand file-menu:open-on-demand])
          (define/override (file-menu:open-on-demand item)
            (super-file-menu:open-on-demand item)
            (send item set-label (if (preferences:get 'framework:open-here?)
                                     (string-constant open-here-menu-item)
                                     (string-constant open-menu-item))))
          
	  (rename [super-on-close on-close])
	  (define/override (on-close)
	    (super-on-close)
	    (let ([group (group:get-the-frame-group)])
	      (when (eq? this (send group get-open-here-frame))
		(send group set-open-here-frame #f))))

          (rename [super-on-activate on-activate])
          (define/override (on-activate on?)
            (super-on-activate on?)
            (when on?
              (send (group:get-the-frame-group) set-open-here-frame this)))
          
          (inherit get-editor)
          (define/public (get-open-here-editor) (get-editor))
          (define/public (open-here filename)
            (let* ([editor (get-open-here-editor)]
                   [okay-to-switch? (user-okays-switch? editor)])
              (when okay-to-switch?
                (when (is-a? editor text%)
                  (let* ([b (box #f)]
                         [filename (send editor get-filename b)])
                    (unless (unbox b)
                      (when filename
                        (handler:set-recent-position 
                         filename 
                         (send editor get-start-position)
                         (send editor get-end-position))))))
                (send editor begin-edit-sequence)
                (send editor lock #f)
                (send editor load-file/gui-error filename)
                (send editor end-edit-sequence)
		(void))))
          
          (inherit get-label)
          (define/private (user-okays-switch? ed)
            (or (not (send ed is-modified?))
                (let ([answer
                       (gui-utils:unsaved-warning 
                        (or (send ed get-filename) (get-label))
                        (string-constant switch-anyway)
                        #t)])
                  (case answer
                    [(continue)
                     #t]
                    [(save) 
                     (send ed save-file/gui-error)]
                    [(cancel)
                     #f]))))
            
	  (super-instantiate ())))

      (define text<%> (interface (-editor<%>)))
      (define text-mixin
        (mixin (-editor<%>) (text<%>)
          (override get-editor<%> get-editor%)
          [define get-editor<%> (lambda () (class->interface text%))]
          [define get-editor% (lambda () text:keymap%)]
          (super-instantiate ())))
      
      (define pasteboard<%> (interface (-editor<%>)))
      (define pasteboard-mixin
        (mixin (-editor<%>) (pasteboard<%>)
          (override get-editor<%> get-editor%)
          [define get-editor<%> (lambda () (class->interface pasteboard%))]
          [define get-editor% (lambda () pasteboard:keymap%)]
          (super-instantiate ())))
      
      (define delegate<%>
        (interface (status-line<%> text<%>)
          get-delegated-text
          delegated-text-shown?
          hide-delegated-text
          show-delegated-text
          delegate-moved))
      
      (define delegatee-editor-canvas%
        (class editor-canvas%
          (rename [super-on-event on-event])
          (init-field delegate-frame)
          (inherit get-editor get-dc)
          
          (define/override (on-event evt)
            (super-on-event evt)
            (when delegate-frame
              (let ([text (get-editor)])
                (when (is-a? text text%)
                  (cond
                    [(send evt button-down?)
                     (let-values ([(editor-x editor-y)
                                   (send text dc-location-to-editor-location 
                                         (send evt get-x)
                                         (send evt get-y))])
                       (send delegate-frame click-in-overview 
                             (send text find-position editor-x editor-y)))]
                    [(or (send evt entering?)
                         (send evt moving?))
                     (let-values ([(editor-x editor-y)
                                   (send text dc-location-to-editor-location 
                                         (send evt get-x)
                                         (send evt get-y))])
                       (let* ([b (box #f)]
                              [pos (send text find-position editor-x editor-y #f b)])
                         (cond
                           [(unbox b)
                            (let* ([para (send text position-paragraph pos)]
                                   [start-pos (send text paragraph-start-position para)]
                                   [end-pos (send text paragraph-end-position para)])
                              (send delegate-frame update-status-line 'plt:delegate
                                    (send text get-text start-pos end-pos)))]
                           [else
                            (send delegate-frame update-status-line 'plt:delegate #f)])))]
                    [(send evt leaving?)
                     (send delegate-frame update-status-line 'plt:delegate #f)])))))
          (super-instantiate ())))
      
      (define delegatee-text%
        (class text:basic%
          (rename [super-on-paint on-paint])
          (inherit get-admin)
          (define start-para #f)
          (define end-para #f)
          (define view-x-b (box 0))
          (define view-width-b (box 0))
          (inherit paragraph-start-position paragraph-end-position 
                   position-location invalidate-bitmap-cache scroll-to-position
                   get-visible-position-range position-paragraph
                   last-position)

          (define/override (on-new-string-snip)
            (instantiate text:1-pixel-string-snip% ()))
          
          (define/override (on-new-tab-snip)
            (instantiate text:1-pixel-tab-snip% ()))
          
          ;; set-start/end-para : (union (#f #f -> void) (number number -> void))
          (define/public (set-start/end-para _start-para _end-para)
            (unless (and (equal? _start-para start-para)
                         (equal? _end-para end-para))
              (let ([old-start-para start-para]
                    [old-end-para end-para])
                (cond
                  [else
                   (set! start-para _start-para)
                   (set! end-para _end-para)])
                
                (when (and start-para end-para)
                  
                  (let-values ([(v-start v-end) (let ([bs (box 0)]
                                                      [bf (box 0)])
                                                  (get-visible-position-range bs bf)
                                                  (values (unbox bs)
                                                          (unbox bf)))])
                    (let ([v-start-para (position-paragraph v-start)]
                          [v-end-para (position-paragraph v-end)])
                      (cond
                        [(v-start-para . >= . start-para)
                         (scroll-to-position (paragraph-start-position start-para))]
                        [(v-end-para . <= . end-para)
                         (scroll-to-position (paragraph-end-position end-para))]
                        [else (void)]))))
                
                (when (and old-start-para old-end-para)
                  (let-values ([(x y w h) (get-rectangle old-start-para old-end-para)])
                    (when x
                      (invalidate-bitmap-cache x y w h))))
                (when (and start-para end-para)
                  (let-values ([(x y w h) (get-rectangle start-para end-para)])
                    (when x
                      (invalidate-bitmap-cache x y w h)))))))

          (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
            (super-on-paint before? dc left top right bottom dx dy draw-caret)
            (when (and before?
                       start-para
                       end-para)
              (let ([old-pen (send dc get-pen)]
                    [old-brush (send dc get-brush)])
                (send dc set-pen
                      (send the-pen-list find-or-create-pen "light blue" 1 'solid))
                (send dc set-brush
                      (send the-brush-list find-or-create-brush "light blue" 'solid))
                (let-values ([(x y w h) (get-rectangle start-para end-para)])
                  (when x
                    (send dc draw-rectangle 
                          (+ dx x)
                          (+ dy y)
                          w
                          h)))
                (send dc set-pen old-pen)
                (send dc set-brush old-brush))))
          
          
          ;; get-rectangle : number number -> 
          ;;                (values (union #f number) (union #f number) (union #f number) (union #f number))
          ;; computes the rectangle corresponding the input paragraphs
          (define/private (get-rectangle start-para end-para)
            (let ([start (get-line-y start-para #t)]
                  [end (get-line-y end-para #f)]
                  [admin (get-admin)])
              (cond
                [(not admin)
                 (values #f #f #f #f)]
                [(= 0 (last-position))
                 (values #f #f #f #f)]
                [else
                 (send admin get-view view-x-b #f view-width-b #f)
                 (send admin get-view view-x-b #f view-width-b #f)
                 (values (unbox view-x-b)
                         start
                         (unbox view-width-b)
                         (- end start))])))

          (define/private (get-line-y para top?)
            (let ([pos (paragraph-start-position para)]
                  [b (box 0)])
              (position-location pos #f b top? #f #t)
              (unbox b)))
	  (super-instantiate ())

	  (inherit set-cursor)
	  (set-cursor (make-object cursor% 'arrow))

          (inherit set-line-spacing)
          (set-line-spacing 0)))
      
      (define delegate-mixin
        (mixin (status-line<%> text<%>) (delegate<%>)

          (define/public (get-delegated-text) (get-editor))

          (rename [super-make-root-area-container make-root-area-container])
          [define rest-panel 'uninitialized-root]
          [define super-root 'uninitialized-super-root]
          (override make-root-area-container)
          [define make-root-area-container
            (lambda (% parent)
              (let* ([s-root (super-make-root-area-container
                              horizontal-panel%
                              parent)]
                     [r-root (make-object % s-root)])
                (set! super-root s-root)
                (set! rest-panel r-root)
                r-root))]

	  (rename [super-get-editor<%> get-editor<%>])
	  (define/override (get-editor<%>)
            text:delegate<%>)

          (rename [super-get-editor% get-editor%])
          (define/override (get-editor%)
            (text:delegate-mixin (super-get-editor%)))

          (field (shown? (preferences:get 'framework:show-delegate?)))
          (define/public (delegated-text-shown?) 
            shown?)

          (inherit close-status-line open-status-line)
          (define/public (hide-delegated-text)
            (close-status-line 'plt:delegate)
            (set! shown? #f)
            (send (get-delegated-text) set-delegate #f)
            (send super-root change-children
                  (lambda (l) (list rest-panel))))
          (define/public (show-delegated-text)
            (open-status-line 'plt:delegate)
            (set! shown? #t)
            (send (get-delegated-text) set-delegate delegatee)
            (send super-root change-children
                  (lambda (l) (list rest-panel delegate-ec))))

          (define/public (click-in-overview pos)
            (when shown?
              (let* ([d-text (get-delegated-text)]
                     [d-canvas (send d-text get-canvas)]
                     [bx (box 0)]
                     [by (box 0)])
                (let-values ([(cw ch) (send d-canvas get-client-size)])
                  (send d-text position-location pos bx by)
                  (send d-canvas focus)
                  (send d-canvas scroll-to 
                        (- (unbox bx) (/ cw 2))
                        (- (unbox by) (/ ch 2))
                        cw
                        ch
                        #t)))))
          
          (define/public (delegate-moved)
            (let ([startb (box 0)]
                  [endb (box 0)]
                  [delegate-text (get-delegated-text)])
              (send delegate-text get-visible-position-range startb endb #f)
              (send delegatee set-start/end-para
                    (send delegate-text position-paragraph (unbox startb)) 
                    (send delegate-text position-paragraph (unbox endb)))))

          (define/public (get-delegatee) delegatee)

	  (super-instantiate ())
          
          (define delegatee (instantiate delegatee-text% ()))
          (define delegate-ec (instantiate delegatee-editor-canvas% ()
                                (editor delegatee)
                                (parent super-root)
                                (delegate-frame this)
                                (min-width 150)
                                (stretchable-width #f)))
          (inherit get-editor)
          (if (preferences:get 'framework:show-delegate?)
              (begin
                (open-status-line 'plt:delegate)
                (send (get-delegated-text) set-delegate delegatee)
                (send super-root change-children
                      (lambda (l) (list rest-panel delegate-ec))))
              (begin
                (send (get-delegated-text) set-delegate #f)
                (send super-root change-children (lambda (l) (list rest-panel)))))))
              
      
      (define (search-dialog frame)
        (init-find/replace-edits)
        (keymap:call/text-keymap-initializer
         (lambda ()
           (let* ([to-be-searched-text (send frame get-text-to-search)]
                  [to-be-searched-canvas (send to-be-searched-text get-canvas)]
                  
                  [allow-replace? (not (send to-be-searched-text is-locked?))]
                  
                  [dialog (make-object dialog% 
                            (if allow-replace?
                                (string-constant find-and-replace)
                                (string-constant find))
                            frame)]
                  
                  [copy-text
                   (lambda (from to)
                     (send to erase)
                     (let loop ([snip (send from find-first-snip)])
                       (when snip
                         (send to insert (send snip copy))
                         (loop (send snip next)))))]
                  
                  [text-keymap/editor%
                   (class100 text:keymap% args
                     (rename [super-get-keymaps get-keymaps])
                     (override
                       [get-keymaps
                        (lambda ()
                          (if (preferences:get 'framework:menu-bindings)
                              (append (list (keymap:get-editor))
                                      (super-get-keymaps))
                              (append (super-get-keymaps)
                                      (list (keymap:get-editor)))))])
                     (sequence
                       (apply super-init args)))]
                  
                  
                  [find-panel (make-object horizontal-panel% dialog)]
                  [find-message (make-object message% (string-constant find) find-panel)]
                  [f-text (make-object text-keymap/editor%)]
                  [find-canvas (make-object editor-canvas% find-panel f-text
                                 '(hide-hscroll hide-vscroll))]
                  
                  [replace-panel (make-object horizontal-panel% dialog)]
                  [replace-message (make-object message% (string-constant replace) replace-panel)]
                  [r-text (make-object text-keymap/editor%)]
                  [replace-canvas (make-object editor-canvas% replace-panel r-text
                                    '(hide-hscroll hide-vscroll))]
                  
                  [button-panel (make-object horizontal-panel% dialog)]
                  
                  [update-texts
                   (lambda ()
                     (send find-edit stop-searching)
                     (copy-text f-text find-edit)
                     (send find-edit start-searching)
                     (copy-text r-text replace-edit))]
                  
                  [find-button (make-object button% (string-constant find) button-panel 
                                 (lambda x
                                   (update-texts)
                                   (send frame search-again))
                                 '(border))]
                  [replace-button (make-object button% (string-constant replace) button-panel
                                    (lambda x
                                      (update-texts)
                                      (send frame replace)))]
                  [replace-and-find-button (make-object button% (string-constant replace&find-again)
                                             button-panel
                                             (lambda x
                                               (update-texts)
                                               (send frame replace&search)))]
                  [replace-to-end-button
                   (make-object button% (string-constant replace-to-end) button-panel
                     (lambda x
                       (update-texts)
                       (send frame replace-all)))]
                  
                  [dock-button (make-object button%
                                 (string-constant dock)
                                 button-panel
                                 (lambda (btn evt)
                                   (update-texts)
                                   (preferences:set 'framework:search-using-dialog? #f)
                                   (send frame unhide-search)))]
                  
                  [close
                   (lambda ()
                     (when to-be-searched-canvas
                       (send to-be-searched-canvas force-display-focus #f))
                     (send dialog show #f))]
                  
                  [close-button (make-object button% (string-constant close) button-panel
                                  (lambda (x y)
                                    (close)))]

                  [remove-pref-callback
                   (preferences:add-callback 
                    'framework:search-using-dialog?
                    (lambda (p v)
                      (unless v
                        (close))))])
             
             (unless allow-replace?
               (send button-panel change-children
                     (lambda (l)
                       (remq
                        replace-button
                        (remq
                         replace-and-find-button
                         (remq 
                          replace-to-end-button
                          l)))))
               (send dialog change-children
                     (lambda (l)
                       (remq replace-panel l))))

             (copy-text find-edit f-text)
             (copy-text replace-edit r-text)
             (send find-canvas min-width 400)
             (send find-canvas set-line-count 2)
             (send find-canvas stretchable-height #f)
             (send find-canvas allow-tab-exit #t)
             (send replace-canvas min-width 400)
             (send replace-canvas set-line-count 2)
             (send replace-canvas stretchable-height #f)
             (send replace-canvas allow-tab-exit #t)
             (let ([msg-width (max (send find-message get-width)
                                   (send replace-message get-width))])
               (send find-message min-width msg-width)
               (send replace-message min-width msg-width))
             (send find-canvas focus)
             (send f-text set-position 0 (send f-text last-position))
             (send button-panel set-alignment 'right 'center)
             (send dialog center 'both)
             (when to-be-searched-canvas
               (send to-be-searched-canvas force-display-focus #t))
             (send dialog show #t)
             (remove-pref-callback)))))
      
      (define searchable<%> (interface (basic<%>)
                              get-text-to-search
                              hide-search
                              unhide-search
                              set-search-direction
                              replace&search
                              replace-all
                              replace
                              can-replace?
                              toggle-search-focus
                              move-to-search-or-search
                              move-to-search-or-reverse-search
                              search-again))
      (define search-anchor 0)
      (define searching-direction 'forward)
      (define (set-searching-direction x) 
        (unless (or (eq? x 'forward)
                    (eq? x 'backward))
          (error 'set-searching-direction "expected ~e or ~e, got ~e" 'forward 'backward x))
        (set! searching-direction x))
      
      (define old-search-highlight void)
      (define clear-search-highlight
        (lambda ()
          (begin (old-search-highlight)
                 (set! old-search-highlight void))))
      (define reset-search-anchor
        (let ([color (make-object color% "BLUE")])
          (lambda (edit)
            (old-search-highlight)
            (let ([position 
                   (if (eq? 'forward searching-direction)
                       (send edit get-end-position)
                       (send edit get-start-position))])
              (set! search-anchor position)
              
              ;; don't draw the anchor
              '(set! old-search-highlight
                     (send edit highlight-range position position color #f))))))
      
      (define find-string-embedded
        (opt-lambda (edit
                     str
                     [direction 'forward]
                     [start 'start]
                     [end 'eof]
                     [get-start #t]
                     [case-sensitive? #t]
                     [pop-out? #f])
          (unless (member direction '(forward backward))
            (error 'find-string-embedded
                   "expected ~e or ~e as first argument, got: ~e" 'forward 'backward direction))
          (let/ec k
            (let* ([start (if (eq? start 'start) 
                              (send edit get-start-position)
                              start)]
                   [end (if (eq? 'eof end)
                            (if (eq? direction 'forward)
                                (send edit last-position)
                                0)
                            end)]
                   [flat (send edit find-string str direction
                               start end get-start
                               case-sensitive?)]
                   [pop-out
                    (lambda ()
                      (let ([admin (send edit get-admin)])
                        (if (is-a? admin editor-snip-editor-admin<%>)
                            (let* ([snip (send admin get-snip)]
                                   [edit-above (send (send snip get-admin) get-editor)]
                                   [pos (send edit-above get-snip-position snip)]
                                   [pop-out-pos (if (eq? direction 'forward) (add1 pos) pos)])
                              (find-string-embedded
                               edit-above
                               str
                               direction 
                               pop-out-pos
                               (if (eq? direction 'forward) 'eof 0)
                               get-start
                               case-sensitive?
                               pop-out?))
                            (values edit #f))))])
              (let loop ([current-snip (send edit find-snip start
                                             (if (eq? direction 'forward)
                                                 'after-or-none
                                                 'before-or-none))])
                (let ([next-loop
                       (lambda ()
                         (if (eq? direction 'forward)
                             (loop (send current-snip next))
                             (loop (send current-snip previous))))])
                  (cond
                    [(or (not current-snip)
                         (and flat
                              (let* ([start (send edit get-snip-position current-snip)]
                                     [end (+ start (send current-snip get-count))])
                                (if (eq? direction 'forward)
                                    (and (<= start flat)
                                         (< flat end))
                                    (and (< start flat)
                                         (<= flat end))))))
                     (if (and (not flat) pop-out?)
                         (pop-out)
                         (values edit flat))]
                    [(is-a? current-snip editor-snip%)
                     (let-values ([(embedded embedded-pos)
                                   (let ([media (send current-snip get-editor)])
                                     (if (and media
                                              (is-a? media text%))
                                         (begin
                                           (find-string-embedded 
                                            media 
                                            str
                                            direction
                                            (if (eq? 'forward direction)
                                                0
                                                (send media last-position))
                                            'eof
                                            get-start case-sensitive?))
                                         (values #f #f)))])
                       (if (not embedded-pos)
                           (next-loop)
                           (values embedded embedded-pos)))]
                    [else (next-loop)])))))))
      
      (define searching-frame #f)
      (define (set-searching-frame frame)
        (set! searching-frame frame))
      
      (define find-text%
        (class100 text:keymap% args
          (inherit get-text)
          (rename [super-after-insert after-insert]
                  [super-after-delete after-delete]
                  [super-on-focus on-focus])
          (private
            [get-searching-edit
             (lambda ()
               (and searching-frame
                    (send searching-frame get-text-to-search)))])
          (public
            [search
             (opt-lambda ([reset-search-anchor? #t] [beep? #t] [wrap? #t])
               (when searching-frame
                 (let* ([string (get-text)]
                        [top-searching-edit (get-searching-edit)]
                        
                        [searching-edit (let ([focus-snip (send top-searching-edit get-focus-snip)])
                                          (if focus-snip
                                              (send focus-snip get-editor)
                                              top-searching-edit))]
                        
                        [not-found
                         (lambda (found-edit skip-beep?)
                           (send found-edit set-position search-anchor)
                           (when (and beep?
                                      (not skip-beep?))
                             (bell))
                           #f)]
                        [found
                         (lambda (edit first-pos)
                           (let ([last-pos ((if (eq? searching-direction 'forward) + -)
                                            first-pos (string-length string))])
                             (send* edit 
                               (set-caret-owner #f 'display)
                               (set-position
                                (min first-pos last-pos)
                                (max first-pos last-pos)
                                #f #t 'local))
                             #t))])
                   (if (string=? string "")
                       (not-found top-searching-edit #t)
                       (begin
                         (when reset-search-anchor?
                           (reset-search-anchor searching-edit))
                         (let-values ([(found-edit first-pos)
                                       (find-string-embedded
                                        searching-edit
                                        string
                                        searching-direction
                                        search-anchor
                                        'eof #t #t #t)])
                           (cond
                             [(not first-pos)
                              (if wrap?
                                  (let-values ([(found-edit pos)
                                                (find-string-embedded
                                                 top-searching-edit
                                                 string 
                                                 searching-direction
                                                 (if (eq? 'forward searching-direction)
                                                     0
                                                     (send searching-edit last-position)))])
                                    (if (not pos)
                                        (not-found found-edit #f)
                                        (found found-edit pos)))
                                  (not-found found-edit #f))]
                             [else
                              (found found-edit first-pos)])))))))])
          (private-field
            [dont-search #f])
          (public
            [stop-searching
             (lambda ()
               (set! dont-search #t))]
            [start-searching
             (lambda ()
               (set! dont-search #f))])
          
          (override
            [on-focus
             (lambda (on?)
               (when on?
                 (let ([edit (get-searching-edit)])
                   (when edit
                     (reset-search-anchor (get-searching-edit)))))
               (super-on-focus on?))]
            [after-insert
             (lambda (x y)
               (super-after-insert x y)
               (unless dont-search
                 (search #f)))]
            [after-delete
             (lambda (x y)
               (super-after-delete x y)
               (unless dont-search
                 (search #f)))])
          (sequence (apply super-init args))))
      
      ; this is here for when editors are printed, during debugging
      (define replace-text%
        (class text:keymap%
          (super-instantiate ())))
      
      (define find-edit #f)
      (define replace-edit #f)
      
      (define searchable-canvas% 
        (class100 editor-canvas% (parent)
          (inherit get-top-level-window set-line-count)
          (rename [super-on-focus on-focus])
          (override
            [on-focus
             (lambda (x)
               (when x
                 (set-searching-frame (get-top-level-window)))
               (super-on-focus x))])
          (sequence
            (super-init parent #f '(hide-hscroll hide-vscroll))
            (set-line-count 2))))
      
      (define (init-find/replace-edits)
        (unless find-edit
          (set! find-edit (make-object find-text%))
          (set! replace-edit (make-object replace-text%))
          (for-each (lambda (keymap)
                      (send keymap chain-to-keymap
                            (keymap:get-search)
                            #t))
                    (list (send find-edit get-keymap)
                          (send replace-edit get-keymap)))))
      
      (define searchable-mixin
        (mixin (standard-menus<%>) (searchable<%>)
          (init-find/replace-edits)
          (rename [super-make-root-area-container make-root-area-container]
                  [super-on-activate on-activate]
                  [super-on-close on-close])
          (define super-root 'unitiaialized-super-root)
          (override edit-menu:find-callback edit-menu:create-find? 
                    edit-menu:find-again-callback edit-menu:create-find-again? 
                    edit-menu:replace-and-find-again-callback edit-menu:replace-and-find-again-on-demand
                    edit-menu:create-replace-and-find-again?)
          (define edit-menu:find-callback (lambda (menu evt) (move-to-search-or-search) #t))
          (define edit-menu:create-find? (lambda () #t))
          (define edit-menu:find-again-callback (lambda (menu evt) (search-again) #t))
          (define edit-menu:create-find-again? (lambda () #t))
          (define edit-menu:replace-and-find-again-callback (lambda (menu evt) (replace&search) #t))
          (define edit-menu:replace-and-find-again-on-demand
            (lambda (item) (send item enable (can-replace?))))
          (define edit-menu:create-replace-and-find-again? (lambda () #t))
          (override make-root-area-container)
          (define make-root-area-container
            (lambda (% parent)
              (let* ([s-root (super-make-root-area-container
                              vertical-panel%
                              parent)]
                     [root (make-object % s-root)])
                (set! super-root s-root)
                root)))

          (define/override on-activate
            (lambda (on?)
              (unless hidden?
                (if on?
                    (reset-search-anchor (get-text-to-search))
                    (clear-search-highlight)))
              (super-on-activate on?)))
          
          (define/public (get-text-to-search)
	    (error 'get-text-to-search "abstract method in searchable-mixin"))
          (define/public hide-search
            (opt-lambda ([startup? #f])
	      (when search-gui-built?
		(send super-root change-children
		      (lambda (l)
			(remove search-panel l))))
              (clear-search-highlight)
              (unless startup?
                (let ([canvas (send (get-text-to-search) get-canvas)])
                  (when canvas
                    (send canvas force-display-focus #f)
                    (send canvas focus))))
              (set! hidden? #t)))
          
          (define/public (unhide-search)
            (when (and hidden?
                       (not (preferences:get 'framework:search-using-dialog?)))
              (set! hidden? #f)

	      (build-search-gui-in-frame)

              (let ([canvas (send (get-text-to-search) get-canvas)])
                (when canvas
                  (send canvas force-display-focus #t)))
              (show/hide-replace (send (get-text-to-search) is-locked?))
              (send search-panel focus)
              (send find-edit set-position 0 (send find-edit last-position))
              (unless (memq search-panel (send super-root get-children))
		(send super-root add-child search-panel))
              (reset-search-anchor (get-text-to-search))))

          (define (undock)
            (preferences:set 'framework:search-using-dialog? #t)
            (hide-search)
            (search-dialog this))

	  ;; pre-condition : search-gui-built? is #t
          (define/private (show/hide-replace hide?)
            (cond
              [hide?
               (send replace-canvas-panel change-children
                     (lambda (l) null))
               (send replace-button-panel change-children (lambda (l) null))
               (send middle-middle-panel change-children (lambda (l) null))]
              [else
               (send replace-canvas-panel change-children
                     (lambda (l) (list replace-canvas)))
               (send replace-button-panel change-children
                     (lambda (l) (list replace-button)))
               (send middle-middle-panel change-children
                     (lambda (l) (list replace&search-button
                                       replace-all-button)))]))

          (define remove-callback
            (preferences:add-callback
             'framework:search-using-dialog?
             (lambda (p v)
               (when p
                 (hide-search)))))
          (override on-close)
          (define on-close
            (lambda ()
              (super-on-close)
              (remove-callback)
              (let ([close-canvas
                     (lambda (canvas edit)
                       (send canvas set-editor #f))])
		(when search-gui-built?
		  (close-canvas find-canvas find-edit)
		  (close-canvas replace-canvas replace-edit)))
              (when (eq? this searching-frame)
                (set-searching-frame #f))))
          (public set-search-direction can-replace? replace&search replace-all replace
                  toggle-search-focus move-to-search-or-search move-to-search-or-reverse-search
                  search-again)
          (define set-search-direction 
            (lambda (x) 
              (set-searching-direction x)
	      (when dir-radio
		(send dir-radio set-selection (if (eq? x 'forward) 0 1)))))
          (define can-replace?
            (lambda ()
              (let ([tx (get-text-to-search)])
                (and
                 tx
                 (not (= 0 (send replace-edit last-position)))
                 (string=?
                  (send tx get-text
                        (send tx get-start-position)
                        (send tx get-end-position))
                  (send find-edit get-text 0 (send find-edit last-position)))))))
          (define replace&search
            (lambda ()
              (let ([text (get-text-to-search)])
                (send text begin-edit-sequence)
                (when (replace)
                  (search-again))
                (send text end-edit-sequence))))
          (define (replace-all)
            (let* ([replacee-edit (get-text-to-search)]
                   [embeded-replacee-edit (find-embedded-focus-editor replacee-edit)]
                   [pos (if (eq? searching-direction 'forward)
                            (send embeded-replacee-edit get-start-position)
                            (send embeded-replacee-edit get-end-position))]
                   [done? (if (eq? 'forward searching-direction)
                              (lambda (x) (>= x (send replacee-edit last-position)))
                              (lambda (x) (<= x 0)))])
              (send replacee-edit begin-edit-sequence)
              (when (search-again)
                (send embeded-replacee-edit set-position pos)
                (let loop ()
                  (when (send find-edit search #t #f #f)
                    (replace)
                    (loop))))
              (send replacee-edit end-edit-sequence)))
          (define (replace)
            (let* ([search-text (send find-edit get-text)]
                   [replacee-edit (find-embedded-focus-editor (get-text-to-search))]
                   [replacee-start (send replacee-edit get-start-position)]
                   [new-text (send replace-edit get-text)]
                   [replacee (send replacee-edit get-text
                                   replacee-start
                                   (send replacee-edit get-end-position))])
              (if (string=? replacee search-text)
                  (begin (send replacee-edit insert new-text)
                         (send replacee-edit set-position
                               replacee-start
                               (+ replacee-start (string-length new-text)))
                         #t)
                  #f)))

          (define/private (find-embedded-focus-editor editor)
            (let loop ([editor editor])
              (let ([s (send editor get-focus-snip)])
                (cond
                  [(and s (is-a? s editor-snip%))
                   (let ([next-ed (send s get-editor)])
                     (if next-ed 
                         (loop next-ed)
                         editor))]
                  [else editor]))))
          
          (define (toggle-search-focus)
            (set-searching-frame this)
            (unhide-search)
	    (send (cond
		    [(send find-canvas has-focus?)
		     replace-canvas]
		    [(send replace-canvas has-focus?)
		     (send (get-text-to-search) get-canvas)]
		    [else
		     find-canvas])
		  focus))
          (define move-to-search-or-search
            (lambda ()
              (set-searching-frame this)
              (unhide-search)
              (cond
                [(preferences:get 'framework:search-using-dialog?)
                 (search-dialog this)]
                [else
		 (if (or (send find-canvas has-focus?)
			 (send replace-canvas has-focus?))
		     (search-again 'forward)
		     (send find-canvas focus))])))
          (define move-to-search-or-reverse-search
            (lambda ()
              (set-searching-frame this)
              (unhide-search)
              (if (or (send find-canvas has-focus?)
                      (send replace-canvas has-focus?))
                  (search-again 'backward)
                  (send find-canvas focus))))
          (define search-again
            (opt-lambda ([direction searching-direction] [beep? #t])
              (set-searching-frame this)
              (unhide-search)
              (set-search-direction direction)
              (send find-edit search #t beep?)))
          
	  (define search-panel #f)
	  (define search-gui-built? #f)
          (define dir-radio #f)
	  (define replace-canvas-panel #f)
          (define find-canvas #f)
	  (define replace-canvas #f)
	  (define hidden? #t)
	  (define replace-button-panel #f)
	  (define middle-middle-panel #f)
	  (define replace-button #f)
	  (define replace&search-button #f)
	  (define replace-all-button #f)
	  
	  (inherit begin-container-sequence end-container-sequence)
	  (define/private (build-search-gui-in-frame)
	    (unless search-gui-built?
	      (set! search-gui-built? #t)
	      (begin-container-sequence)
	      (let ()
		(define _0 (set! search-panel (make-object horizontal-panel% super-root '(border))))
		(define left-panel (make-object vertical-panel% search-panel))
		(define _1 (set! find-canvas (make-object searchable-canvas% left-panel)))
		(define _2
		  (set! replace-canvas-panel (instantiate vertical-panel% ()
					       (parent left-panel)
					       (stretchable-width #t)
					       (stretchable-height #f))))
		(define _3
		  (set! replace-canvas (make-object searchable-canvas% replace-canvas-panel)))
		
		(define middle-left-panel (make-object vertical-pane% search-panel))
		(define _4
		  (set! middle-middle-panel (make-object vertical-pane% search-panel)))
		(define middle-right-panel (make-object vertical-pane% search-panel))
		
		(define search-button (make-object button% 
					(string-constant find)
					middle-left-panel
					(lambda args (search-again))))
		
		(define _5
		  (set! replace-button-panel
			(instantiate vertical-panel% ()
			  (parent middle-left-panel)
			  (stretchable-width #f)
			  (stretchable-height #f))))
		
		(define _6
		  (set! replace-button (make-object button% (string-constant replace)
						    replace-button-panel
						    (lambda x (replace)))))
		
		(define _7
		  (set! replace&search-button (make-object button% 
						(string-constant replace&find-again)
						middle-middle-panel
						(lambda x (replace&search)))))
		
		(define _8
		  (set! replace-all-button (make-object button% 
					     (string-constant replace-to-end)
					     middle-middle-panel
					     (lambda x (replace-all)))))
		(define _9
		  (set! dir-radio (make-object radio-box%
				    #f
				    (list (string-constant forward)
					  (string-constant backward))
				    middle-right-panel
				    (lambda (dir-radio evt)
				      (let ([forward (if (= (send dir-radio get-selection) 0)
							 'forward
							 'backward)])
					(set-search-direction forward)
					(reset-search-anchor (get-text-to-search)))))))

		(define hide/undock-pane (make-object horizontal-panel% middle-right-panel))
		(define hide-button (make-object button% (string-constant hide)
						 hide/undock-pane
						 (lambda args (hide-search))))
		(define undock-button (make-object button% (string-constant undock)
						   hide/undock-pane
						   (lambda args (undock))))
		(let ([align
		       (lambda (x y)
			 (let ([m (max (send x get-width)
				       (send y get-width))])
			   (send x min-width m)
			   (send y min-width m)))])
		  (align search-button replace-button)
		  (align replace&search-button replace-all-button))
		(for-each (lambda (x) (send x set-alignment 'center 'center))
			  (list middle-left-panel middle-middle-panel))
		(for-each (lambda (x) (send x stretchable-height #f))
			  (list search-panel middle-left-panel middle-middle-panel middle-right-panel))
		(for-each (lambda (x) (send x stretchable-width #f))
			  (list middle-left-panel middle-middle-panel middle-right-panel))
		(send find-canvas set-editor find-edit)
		(send find-canvas stretchable-height #t)
		(send replace-canvas set-editor replace-edit))
	      (end-container-sequence)))

	  (super-instantiate ())

	  (hide-search #t)))
      
      (define searchable-text<%> (interface (searchable<%> text<%>)))
      
      (define searchable-text-mixin
        (mixin (text<%> searchable<%>) (searchable-text<%>)
          (inherit get-editor)
          (override get-text-to-search)
          (define (get-text-to-search)
	    (get-editor))
          (override get-editor<%> get-editor%)
          (define (get-editor<%>) text:searching<%>)
          (define (get-editor%) text:searching%)
          (super-instantiate ())))
      
      ; to see printouts in memory debugging better.
      (define memory-text% (class100 text% args (sequence (apply super-init args))))
      (define memory-text (make-object memory-text%))
      (send memory-text hide-caret #t)
      (define show-memory-text?
	(with-handlers ([not-break-exn?
			 (lambda (x) #f)])
	  (directory-exists? (build-path (collection-path "framework") "CVS"))))
      
      (define file<%> (interface (-editor<%>)))
      (define file-mixin
        (mixin (-editor<%>) (file<%>)
          (inherit get-editor get-filename get-label save)
          (rename [super-can-close? can-close?])
          (override can-close?)
          [define can-close?
            (lambda ()
              (let* ([edit (get-editor)]
                     [user-allowed-or-not-modified
                      (or (not (send edit is-modified?))
                          (case (gui-utils:unsaved-warning
                                 (let ([fn (get-filename)])
                                   (if (string? fn)
                                       fn
                                       (get-label)))
                                 (string-constant close-anyway)
                                 #t
                                 this)
                            [(continue) #t]
                            [(save) (save)]
                            [else #f]))])
                (and user-allowed-or-not-modified
                     (super-can-close?))))]
	  (super-instantiate ())))
      
      (define bday-click-canvas%
        (class canvas%
          (rename [super-on-event on-event])
          (define/override (on-event evt)
            (cond
              [(and (mrf-bday?)
                    (send evt button-up?))
               (message-box (string-constant drscheme)
                            (string-constant happy-birthday-matthew))]
              [else (super-on-event evt)]))
          (super-instantiate ())))
      
      (define basic% (basic-mixin frame%))
      (define info% (info-mixin basic%))
      (define text-info% (text-info-mixin info%))
      (define pasteboard-info% (pasteboard-info-mixin text-info%))
      (define status-line% (status-line-mixin text-info%))
      (define standard-menus% (standard-menus-mixin status-line%))
      (define editor% (editor-mixin standard-menus%))
      (define open-here% (open-here-mixin editor%))
      
      (define -text% (text-mixin open-here%))
      (define text-info-file% (file-mixin -text%))
      (define searchable% (searchable-text-mixin (searchable-mixin text-info-file%)))
      (define delegate% (delegate-mixin searchable%))
      
      (define -pasteboard% (pasteboard-mixin open-here%))
      (define pasteboard-info-file% (file-mixin -pasteboard%)))))
