
(module frame mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "include.ss")
	   "sig.ss"
	   "../gui-utils-sig.ss"
	   "../macro.ss"
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
              [gui-utils : framework:gui-utils^]
              [exit : framework:exit^]
              [finder : framework:finder^]
              [keymap : framework:keymap^]
              [text : framework:text^]
              [pasteboard : framework:pasteboard^]
              [editor : framework:editor^]
              [canvas : framework:canvas^]
              [menu : framework:menu^])
      
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
          (rename [super-can-close? can-close?]
                  [super-on-close on-close]
                  [super-on-focus on-focus])
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


          (define after-init? #f)
          (override can-close? on-close on-focus on-drop-file)
          [define can-close?
            (lambda ()
              (let ([super (super-can-close?)]
                    [group
                     (send (group:get-the-frame-group)
                           can-remove-frame?
                           this)])
                (and super group)))]
          [define on-close
            (lambda ()
              (super-on-close)
              (send (group:get-the-frame-group)
                    remove-frame
                    this))]
          [define on-focus
            (lambda (on?)
              (super-on-focus on?)
              (when on?
                (send (group:get-the-frame-group) set-active-frame this)))]
          
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
          
          (inherit show)
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
            
          (make-object menu:can-restore-underscore-menu% (string-constant windows-menu-label)
            (make-object (get-menu-bar%) this))
          (reorder-menus this)
          (send (group:get-the-frame-group) insert-frame this)
          [define panel (make-root-area-container (get-area-container%) this)]
          (public get-area-container)
          [define get-area-container (lambda () panel)]
          (set! after-init? #t)))
      
      (define locked-message (string-constant read-only))
      (define unlocked-message (string-constant read/write))
      
      (define lock-canvas%
        (class100 canvas% (parent . args)
          (private-field
           [locked? #f])
          (public
            [set-locked
             (lambda (l)
               (set! locked? l)
               (on-paint))])
          (inherit get-client-size get-dc)
          (override
            [on-paint
             (lambda ()
               (let* ([dc (get-dc)]
                      [draw
                       (lambda (str bg-color line-color)
                         (let-values ([(w h) (get-client-size)]
                                      [(tw th ta td) (send dc get-text-extent str)])
                           (send dc set-pen (send the-pen-list find-or-create-pen line-color 1 'solid))
                           (send dc set-brush (send the-brush-list find-or-create-brush bg-color 'solid))
                           (send dc draw-rectangle 0 0 w h)
                           (send dc draw-text str
                                 (- (/ w 2) (/ tw 2))
                                 (- (/ h 2) (/ th 2)))))])
                 (if locked?
                     (draw locked-message "yellow" "black")
                     (draw unlocked-message (get-panel-background) (get-panel-background)))))])
          (inherit min-width min-height stretchable-width stretchable-height)
          (sequence
            (apply super-init parent args)
            (let ([dc (get-dc)])
              (send dc set-font (send parent get-label-font))
              (let-values ([(w1 h1 _1 _2) (send dc get-text-extent locked-message)]
                           [(w2 h2 _3 _4) (send dc get-text-extent unlocked-message)])
                (stretchable-width #f)
                (stretchable-height #t)
                (min-width (inexact->exact (floor (max w1 w2))))
                (min-height (inexact->exact (floor (+ 4 (max h1 h2))))))))))

      (define info<%> (interface (basic<%>)
                        determine-width
                        lock-status-changed
                        update-info
                        set-info-canvas
                        get-info-canvas
                        get-info-editor
                        get-info-panel))
      
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
          [define close-panel-callback
            (preferences:add-callback
             'framework:show-status-line
             (lambda (p v)
               (if v 
                   (register-gc-blit)
                   (unregister-collecting-blit gc-canvas))
               (send super-root change-children
                     (lambda (l)
                       (if v
                           (list rest-panel outer-info-panel)
                           (list rest-panel))))))]
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
                (send memory-text insert (number->string (current-memory-use)))
                (send memory-text lock #t)
                (send memory-text end-edit-sequence)))]
          
            ; only for CVSers
          (when show-memory-text?
            (let* ([panel (make-object horizontal-panel% (get-info-panel) '(border))]
                   [button (make-object button% (string-constant collect-button-label) panel 
                             (lambda x
                               (collect-garbage)
                               (update-memory-text)))]
                   [ec (make-object editor-canvas% panel memory-text '(no-hscroll no-vscroll))])
              (determine-width "000000000" ec memory-text)
              (update-memory-text)
              (set! memory-cleanup
                    (lambda ()
                      (send ec set-editor #f)))
              (send panel stretchable-width #f)))

          [define lock-canvas (make-object lock-canvas% (get-info-panel))]
          [define gc-canvas (make-object canvas% (get-info-panel) '(border))]
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
             'framework:line-offsets
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
                (preferences:get 'framework:line-offsets)
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
                                [line-start (send edit paragraph-start-position line)]
                                [char (- pos line-start)])
                           (if line-numbers?
                               (format "~a:~a"
                                       (if offset?
                                           (add1 line)
                                           line)
                                       (if offset?
                                           (add1 char)
                                           char))
                               (format "~a"
                                       (if offset?
                                           (+ pos 1)
                                           pos)))))])
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
                (preferences:get 'framework:line-offsets)
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
                           save
                           save-as
                           get-canvas
                           get-editor
                           
                           add-edit-menu-snip-items))
      
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
          [define label (if filename
                            (file-name-from-path filename)
                            (gui-utils:next-untitled-name))]
          [define label-prefix (application:current-app-name)]
          [define do-label
           (lambda ()
             (super-set-label (get-entire-label))
             (send (group:get-the-frame-group) frame-label-changed this))]
          
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
                                  (make-object % (get-area-container))))]
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
              (let* ([name (send (get-editor) get-filename)]
                     [file (parameterize ([finder:dialog-parent-parameter this])
                             (finder:put-file name))])
                (if file
                    (send (get-editor) save-file/gui-error file format)
                    #f))))
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
                    (let ([start
                           (if (is-a? edit text%)
                               (send edit get-start-position)
                               #f)])
                      (when (gui-utils:get-choice
                             (string-constant are-you-sure-revert)
                             (string-constant yes)
                             (string-constant no)
                             (string-constant are-you-sure-revert-title)
                             #f
                             this)
                        (send edit begin-edit-sequence)
                        (let ([status (send edit load-file
                                            filename
                                            'same
                                            #f)])
                          (if status
                              (begin
                                (when (is-a? edit text%)
                                  (send edit set-position start start))
                                (send edit end-edit-sequence))
                              (begin
                                (send edit end-edit-sequence)
                                (message-box
                                 (string-constant error-reverting)
                                 (format (string-constant could-not-read) filename)
                                 this))))))))
              #t))
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
          
          (define edit-menu:do (lambda (const)
                                 (lambda (menu evt)
                                   (let ([edit (get-edit-target-object)])
                                     (when (and edit
                                                (is-a? edit editor<%>))
                                       (send edit do-edit-operation const)))
                                   #t)))
          
          (public add-edit-menu-snip-items)
          (define add-edit-menu-snip-items
            (lambda (edit-menu)
              (let ([c% (get-menu-item%)]
                    [on-demand
                     (lambda (menu-item)
                       (let ([edit (get-edit-target-object)])
                         (send menu-item enable (and edit (is-a? edit editor<%>)))))])
                
                (make-object c% (string-constant insert-text-box-item)
                  edit-menu (edit-menu:do 'insert-text-box) #f #f on-demand)
                (make-object c% (string-constant insert-pb-box-item)
                  edit-menu (edit-menu:do 'insert-pasteboard-box) #f #f on-demand)
                (make-object c% (string-constant insert-image-item)
                  edit-menu (edit-menu:do 'insert-image) #f #f on-demand))))
          
          
          (override edit-menu:between-select-all-and-find)
          (define edit-menu:between-select-all-and-find
           (lambda (edit-menu)
             (make-object separator-menu-item% edit-menu)
             
             (add-edit-menu-snip-items edit-menu)
             
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
                                   (application:current-app-name)))))
          (define help-menu:about-string (lambda () (application:current-app-name)))
          (define help-menu:create-about? (lambda () #t))
          
          (super-instantiate 
           () 
           (label (get-entire-label)))
          
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

          (do-label)
          (cond
            [(and filename (file-exists? filename))
             (send (get-editor) load-file filename 'guess #f)]
            [filename
             (send (get-editor) set-filename filename)]
            [else (void)])
          (let ([canvas (get-canvas)])
            (when (is-a? canvas editor-canvas%)
	    ;; when get-canvas is overridden,
	    ;; it might not yet be implemented
              (send canvas focus)))))
      
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
        (interface (text<%>)
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
            (when (and delegate-frame
                       (send evt button-down?))
              (let ([text (get-editor)])
                (when (is-a? text text%)
                  (let-values ([(editor-x editor-y)
                                (send text dc-location-to-editor-location 
                                      (send evt get-x)
                                      (send evt get-y))])
                    (send delegate-frame click-in-overview 
                          (send text find-position editor-x editor-y)))))))
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
        (mixin (text<%>) (delegate<%>)

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

          (define/public (hide-delegated-text)
            (set! shown? #f)
            (send (get-delegated-text) set-delegate #f)
            (send super-root change-children
                  (lambda (l) (list rest-panel))))
          (define/public (show-delegated-text)
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
                     (send to-be-searched-canvas force-display-focus #f)
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
             (send to-be-searched-canvas force-display-focus #t)
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
          
          (public get-text-to-search hide-search unhide-search)
          (define get-text-to-search
            (lambda ()
              (error 'get-text-to-search "abstract method in searchable-mixin")))
          (define hide-search
            (opt-lambda ([startup? #f])
              (send super-root change-children
                    (lambda (l)
                      (remove search-panel l)))
              (clear-search-highlight)
              (unless startup?
                (let ([canvas (send (get-text-to-search) get-canvas)])
                  (send canvas force-display-focus #f)
                  (send canvas focus)))
              (set! hidden? #t)))
          
          (define (unhide-search)
            (when (and hidden?
                       (not (preferences:get 'framework:search-using-dialog?)))
              (set! hidden? #f)
              (let ([canvas (send (get-text-to-search) get-canvas)])
                (when canvas
                  (send canvas force-display-focus #t)))
              (show/hide-replace (send (get-text-to-search) is-locked?))
              (send search-panel focus)
              (send find-edit set-position 0 (send find-edit last-position))
              (send super-root add-child search-panel)
              (reset-search-anchor (get-text-to-search))))

          (define (undock)
            (preferences:set 'framework:search-using-dialog? #t)
            (hide-search)
            (search-dialog this))

          (define (show/hide-replace hide?)
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
                (close-canvas find-canvas find-edit)
                (close-canvas replace-canvas replace-edit))
              (when (eq? this searching-frame)
                (set-searching-frame #f))))
          (public set-search-direction can-replace? replace&search replace-all replace
                  toggle-search-focus move-to-search-or-search move-to-search-or-reverse-search
                  search-again)
          (define set-search-direction 
            (lambda (x) 
              (set-searching-direction x)
              (send dir-radio set-selection (if (eq? x 'forward) 0 1))))
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
               (when (replace)
                 (search-again))))
            (define replace-all
             (lambda ()
               (let* ([replacee-edit (get-text-to-search)]
                      [pos (if (eq? searching-direction 'forward)
                               (send replacee-edit get-start-position)
                               (send replacee-edit get-end-position))]
                      [done? (if (eq? 'forward searching-direction)
                                 (lambda (x) (>= x (send replacee-edit last-position)))
                                 (lambda (x) (<= x 0)))])
                 (send* replacee-edit 
                   (begin-edit-sequence)
                   (set-position pos))
                 (when (search-again)
                   (send replacee-edit set-position pos)
                   (let loop ()
                     (when (send find-edit search #t #f #f)
                       (replace)
                       (loop))))
                 (send replacee-edit end-edit-sequence))))
            (define replace
             (lambda ()
               (let* ([search-text (send find-edit get-text)]
                      [replacee-edit (get-text-to-search)]
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
                     #f))))
            (define toggle-search-focus
             (lambda ()
               (set-searching-frame this)
               (unhide-search)
               (send (cond
                       [(send find-canvas has-focus?)
                        replace-canvas]
                       [(send replace-canvas has-focus?)
                        (send (get-text-to-search) get-canvas)]
                       [else
                        find-canvas])
                     focus)))
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
          
          (super-instantiate ())

          (define search-panel (make-object horizontal-panel% super-root '(border)))
          
          (define left-panel (make-object vertical-panel% search-panel))
          (define find-canvas (make-object searchable-canvas% left-panel))
          (define replace-canvas-panel (instantiate vertical-panel% ()
                                         (parent left-panel)
                                         (stretchable-width #t)
                                         (stretchable-height #f)))
          (define replace-canvas (make-object searchable-canvas% replace-canvas-panel))
          
          (define middle-left-panel (make-object vertical-pane% search-panel))
          (define middle-middle-panel (make-object vertical-pane% search-panel))
          (define middle-right-panel (make-object vertical-pane% search-panel))
          
          (define search-button (make-object button% 
                                  (string-constant find)
                                  middle-left-panel
                                  (lambda args (search-again))))

          (define replace-button-panel
            (instantiate vertical-panel% ()
              (parent middle-left-panel)
              (stretchable-width #f)
              (stretchable-height #f)))
          
          (define replace-button (make-object button% (string-constant replace)
                                   replace-button-panel
                                   (lambda x (replace))))
          
          (define replace&search-button (make-object button% 
                                          (string-constant replace&find-again)
                                          middle-middle-panel
                                          (lambda x (replace&search))))

          (define replace-all-button (make-object button% 
                                       (string-constant replace-to-end)
                                       middle-middle-panel
                                       (lambda x (replace-all))))
          
          (define dir-radio (make-object radio-box%
                              #f
                              (list (string-constant forward)
                                    (string-constant backward))
                              middle-right-panel
                              (lambda (dir-radio evt)
                                (let ([forward (if (= (send dir-radio get-selection) 0)
                                                   'forward
                                                   'backward)])
                                  (set-search-direction forward)
                                  (reset-search-anchor (get-text-to-search))))))
          (define hide/undock-pane (make-object horizontal-panel% middle-right-panel))
          (define hide-button (make-object button% (string-constant hide)
                                 hide/undock-pane
                                 (lambda args (hide-search))))
          (define undock-button (make-object button% (string-constant undock)
                                  hide/undock-pane
                                  (lambda args (undock))))
          (define hidden? #f)

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
          (send replace-canvas set-editor replace-edit) 
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
      (define show-memory-text? (directory-exists? (build-path (collection-path "framework") "CVS")))
      
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
      
      (define basic% (basic-mixin frame%))
      (define info% (info-mixin basic%))
      (define text-info% (text-info-mixin info%))
      (define pasteboard-info% (pasteboard-info-mixin text-info%))
      (define standard-menus% (standard-menus-mixin pasteboard-info%))
      (define editor% (editor-mixin standard-menus%))
      
      (define -text% (text-mixin editor%))
      (define text-info-file% (file-mixin -text%))
      (define searchable% (searchable-text-mixin (searchable-mixin text-info-file%)))
      (define delegate% (delegate-mixin searchable%))
      
      (define -pasteboard% (pasteboard-mixin editor%))
      (define pasteboard-info-file% (file-mixin -pasteboard%)))))
