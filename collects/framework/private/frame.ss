
(module frame mzscheme
  (require (lib "unitsig.ss")
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
                 "File"
                 (move-to-front
                  "Edit"
                  (move-to-back
                   "Help"
                   items)))])
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
            
          (make-object menu% "&Windows" (make-object (get-menu-bar%) this))
          (reorder-menus this)
          (send (group:get-the-frame-group) insert-frame this)
          [define panel (make-root-area-container (get-area-container%) this)]
          (public get-area-container)
          [define get-area-container (lambda () panel)]
          (set! after-init? #t)))
      
      (define locked-message "Read only")
      (define unlocked-message "Read/Write")
      
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
                   [button (make-object button% "Collect" panel 
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
          (public anchor-status-changed editor-position-changed overwrite-status-changed)
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
                    "Auto-extend Selection"))
              (get-info-panel))]
          [define overwrite-message 
           (make-object message%
             "Overwrite"
             (get-info-panel))]
          [define position-canvas (make-object editor-canvas% (get-info-panel) #f '(no-hscroll no-vscroll))]
          [define position-edit (make-object text%)]
          
          (inherit determine-width)
          (let ([move-front
                 (lambda (x l)
                   (cons x (remq x l)))])
            (send (get-info-panel) change-children
                  (lambda (l)
                    (move-front
                     anchor-message
                     (move-front
                      overwrite-message
                      (move-front
                       position-canvas
                       l))))))
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
                           save-as
                           get-canvas
                           get-editor
                           
                           add-edit-menu-snip-items))
      
      (define editor-mixin
        (mixin (standard-menus<%>) (-editor<%>)
          (init (file-name #f))

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
          [define label (if file-name
                            (file-name-from-path file-name)
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
          [define get-editor% (lambda () (error 'editor-frame% "no editor% class specified"))]
          [define get-editor<%> (lambda () editor<%>)]
          [define make-editor (lambda ()
                                (let ([% (get-editor%)]
                                      [<%> (get-editor<%>)])
                                  (unless (implementation? % <%>)
                                    (error 'frame:editor%
                                           "result of get-editor% method must match ~e interface; got: ~e"
                                           <%> %))
                                  (make-object %)))]
          
          
          (public save-as)
          [define save-as
            (opt-lambda ([format 'same])
              (let* ([name (send (get-editor) get-filename)]
                     [file (parameterize ([finder:dialog-parent-parameter this])
                             (finder:put-file name))])
                (when file
                  (send (get-editor) save-file file format))))]
          (inherit get-checkable-menu-item% get-menu-item%)
          (override file-menu:revert-callback file-menu:create-revert? file-menu:save-callback
                    file-menu:create-save? file-menu:save-as-callback file-menu:create-save-as? 
                    file-menu:print-callback file-menu:create-print?)
          [define file-menu:revert-callback 
            (lambda (item control)
              (let* ([b (box #f)]
                     [edit (get-editor)]
                     [filename (send edit get-filename b)])
                (if (or (not filename) (unbox b))
                    (bell)
                    (let ([start
                           (if (is-a? edit text%)
                               (send edit get-start-position)
                               #f)])
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
                               "Error Reverting"
                               (format "could not read ~a" filename)))))))
                #t))]
          [define file-menu:create-revert? (lambda () #t)]
          [define file-menu:save-callback (lambda (item control)
                                            (send (get-editor) save-file)
                                            #t)]
          
          [define file-menu:create-save? (lambda () #t)]
          [define file-menu:save-as-callback (lambda (item control) (save-as) #t)]
          [define file-menu:create-save-as? (lambda () #t)]
          [define file-menu:print-callback (lambda (item control)
                                             (send (get-editor) print
                                                   #t
                                                   #t
                                                   (preferences:get 'framework:print-output-mode))
                                             #t)]
          [define file-menu:create-print? (lambda () #t)]
          
          [define edit-menu:do (lambda (const)
                                 (lambda (menu evt)
                                   (let ([edit (get-edit-target-object)])
                                     (when (and edit
                                                (is-a? edit editor<%>))
                                       (send edit do-edit-operation const)))
                                   #t))]
          
          (public add-edit-menu-snip-items)
          [define add-edit-menu-snip-items
            (lambda (edit-menu)
              (let ([c% (get-menu-item%)]
                    [on-demand
                     (lambda (menu-item)
                       (let ([edit (get-edit-target-object)])
                         (send menu-item enable (and edit (is-a? edit editor<%>)))))])
                
                (make-object c% "Insert Text Box" edit-menu (edit-menu:do 'insert-text-box) #f #f on-demand)
                (make-object c% "Insert Pasteboard Box" edit-menu (edit-menu:do 'insert-pasteboard-box) #f #f on-demand)
                (make-object c% "Insert Image..." edit-menu (edit-menu:do 'insert-image) #f #f on-demand)))]
          
          
          (override edit-menu:between-select-all-and-find)
          [define edit-menu:between-select-all-and-find
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
                           (send edit auto-wrap (not (send edit auto-wrap))))))])
               (make-object c% "Wrap Text" edit-menu callback #f #f on-demand))
             
             (make-object separator-menu-item% edit-menu))]
          
          (override help-menu:about-callback help-menu:about-string help-menu:create-about?)
          [define help-menu:about-callback 
            (lambda (menu evt) 
              (message-box (application:current-app-name)
                           (format "Welcome to ~a" (application:current-app-name))))]
          [define help-menu:about-string (lambda () (application:current-app-name))]
          [define help-menu:create-about? (lambda () #t)]
          
          (super-instantiate 
           () 
           (label (get-entire-label)))
          
          [define canvas #f]
          [define editor #f]
          (public get-canvas get-editor)
          [define get-canvas 
            (lambda () 
              (unless canvas
                (set! canvas (make-canvas))
                (send canvas set-editor (get-editor)))
              canvas)]
          [define get-editor 
            (lambda () 
              (unless editor
                (set! editor (make-editor))
                (send (get-canvas) set-editor editor))
              editor)]

          (do-label)
          (cond
            [(and file-name (file-exists? file-name))
             (send (get-editor) load-file file-name 'guess #f)]
            [file-name
             (send (get-editor) set-filename file-name)]
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
      
      (define (search-dialog frame)
        (init-find/replace-edits)
        (keymap:call/text-keymap-initializer
         (lambda ()
           (let* ([to-be-searched-text (send frame get-text-to-search)]
                  [to-be-searched-canvas (send to-be-searched-text get-canvas)]
                  
                  [dialog (make-object dialog% "Find and Replace" frame)]
                  
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
                  [find-message (make-object message% "Find" find-panel)]
                  [f-text (make-object text-keymap/editor%)]
                  [find-canvas (make-object editor-canvas% find-panel f-text
                                 '(hide-hscroll hide-vscroll))]
                  
                  [replace-panel (make-object horizontal-panel% dialog)]
                  [replace-message (make-object message% "Replace" replace-panel)]
                  [r-text (make-object text-keymap/editor%)]
                  [replace-canvas (make-object editor-canvas% replace-panel r-text
                                    '(hide-hscroll hide-vscroll))]
                  
                  [button-panel (make-object horizontal-panel% dialog)]
                  [pref-check (make-object check-box%
                                "Use separate dialog for searching"
                                dialog
                                (lambda (pref-check evt)
                                  (preferences:set
                                   'framework:search-using-dialog?
                                   (send pref-check get-value))))]
                  
                  [update-texts
                   (lambda ()
                     (send find-edit stop-searching)
                     (copy-text f-text find-edit)
                     (send find-edit start-searching)
                     (copy-text r-text replace-edit))]
                  
                  [find-button (make-object button% "Find" button-panel 
                                 (lambda x
                                   (update-texts)
                                   (send frame search-again))
                                 '(border))]
                  [replace-button (make-object button% "Replace" button-panel
                                    (lambda x
                                      (update-texts)
                                      (send frame replace)))]
                  [replace-button (make-object button% "Replace && Find Again" button-panel
                                    (lambda x
                                      (update-texts)
                                      (send frame replace&search)))]
                  [replace-button (make-object button% "Replace to End" button-panel
                                    (lambda x
                                      (update-texts)
                                      (send frame replace-all)))]
                  [close-button (make-object button% "Close" button-panel
                                  (lambda x
                                    (send to-be-searched-canvas force-display-focus #f)
                                    (send dialog show #f)))])
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
             (send pref-check set-value (preferences:get 'framework:search-using-dialog?))
             (send button-panel set-alignment 'right 'center)
             (send dialog center 'both)
             (send to-be-searched-canvas force-display-focus #t)
             (send dialog show #t)))))
      
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
        (let ([default-direction 'forward]
              [default-start 'start]
              [default-end 'eof]
              [default-get-start #t]
              [default-case-sensitive? #t]
              [default-pop-out? #f])
          (case-lambda
           [(edit str)
            (find-string-embedded edit str default-direction default-start default-end default-get-start default-case-sensitive? default-pop-out?)]
           [(edit str direction) 
            (find-string-embedded edit str direction default-start default-end default-get-start default-case-sensitive? default-pop-out?)]
           [(edit str direction start) 
            (find-string-embedded edit str direction start default-end default-get-start default-case-sensitive? default-pop-out?)]
           [(edit str direction start end) 
            (find-string-embedded edit str direction start end default-get-start default-case-sensitive? default-pop-out?)]
           [(edit str direction start end get-start) 
            (find-string-embedded edit str direction start end get-start default-case-sensitive? default-pop-out?)]
           [(edit str direction start end get-start case-sensitive?) 
            (find-string-embedded edit str direction start end get-start case-sensitive? default-pop-out?)]
           [(edit str direction start end get-start case-sensitive? pop-out?)
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
                      [else (next-loop)])))))])))
      
      
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
                         (lambda (found-edit)
                           (send found-edit set-position search-anchor)
                           (when beep?
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
                       (not-found top-searching-edit)
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
                                        (not-found found-edit)
                                        (found found-edit pos)))
                                  (not-found found-edit))]
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
             (lambda ()
               (super-after-insert)
               (unless dont-search
                 (search #f)))]
            [after-delete
             (lambda ()
               (super-after-delete)
               (unless dont-search
                 (search #f)))])
          (sequence (apply super-init args))))
      
      ; this is here for when editors are printed.
      (define replace-text%
        (class100 text:keymap% args
          (sequence (apply super-init args))))
      
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
          [define super-root 'unitiaialized-super-root]
          (override edit-menu:find-callback edit-menu:create-find? 
                    edit-menu:find-again-callback edit-menu:create-find-again? 
                    edit-menu:replace-and-find-again-callback edit-menu:replace-and-find-again-on-demand
                    edit-menu:create-replace-and-find-again?)
          [define edit-menu:find-callback (lambda (menu evt) (move-to-search-or-search) #t)]
          [define edit-menu:create-find? (lambda () #t)]
          [define edit-menu:find-again-callback (lambda (menu evt) (search-again) #t)]
          [define edit-menu:create-find-again? (lambda () #t)]
          [define edit-menu:replace-and-find-again-callback (lambda (menu evt) (replace&search) #t)]
          [define edit-menu:replace-and-find-again-on-demand
            (lambda (item) (send item enable (can-replace?)))]
          [define edit-menu:create-replace-and-find-again? (lambda () #t)]
          (override make-root-area-container)
          [define make-root-area-container
            (lambda (% parent)
              (let* ([s-root (super-make-root-area-container
                              vertical-panel%
                              parent)]
                     [root (make-object % s-root)])
                (set! super-root s-root)
                root))]
          (override on-activate)
          [define on-activate
            (lambda (on?)
              (unless hidden?
                (if on?
                    (reset-search-anchor (get-text-to-search))
                    (clear-search-highlight)))
              (super-on-activate on?))]
          
          (public get-text-to-search hide-search unhide-search)
          [define get-text-to-search
            (lambda ()
              (error 'get-text-to-search "abstract method in searchable-mixin"))]
          [define hide-search
            (opt-lambda ([startup? #f])
              (send super-root change-children
                    (lambda (l)
                      (remove search-panel l)))
              (clear-search-highlight)
              (unless startup?
                (send 
                 (send (get-text-to-search) get-canvas) 
                 focus))
              (set! hidden? #t))]
          [define unhide-search
            (lambda ()
              (when (and hidden?
                         (not (preferences:get 'framework:search-using-dialog?)))
                (set! hidden? #f)
                (send search-panel focus)
                (send super-root add-child search-panel)
                (reset-search-anchor (get-text-to-search))))]
          [define remove-callback
            (preferences:add-callback
             'framework:search-using-dialog?
             (lambda (p v)
               (when p
                 (hide-search))))]
          (override on-close)
          [define on-close
            (lambda ()
              (super-on-close)
              (remove-callback)
              (let ([close-canvas
                     (lambda (canvas edit)
                       (send canvas set-editor #f))])
                (close-canvas find-canvas find-edit)
                (close-canvas replace-canvas replace-edit))
              (when (eq? this searching-frame)
                (set-searching-frame #f)))]
          (public set-search-direction can-replace? replace&search replace-all replace
                  toggle-search-focus move-to-search-or-search move-to-search-or-reverse-search
                  search-again)
          [define set-search-direction 
            (lambda (x) 
              (set-searching-direction x)
              (send dir-radio set-selection (if (eq? x 'forward) 0 1)))]
          [define can-replace?
            (lambda ()
              (let ([tx (get-text-to-search)])
                (and
                 tx
                 (not (= 0 (send replace-edit last-position)))
                 (string=?
                  (send tx get-text
                        (send tx get-start-position)
                        (send tx get-end-position))
                  (send find-edit get-text 0 (send find-edit last-position))))))]
            [define replace&search
             (lambda ()
               (when (replace)
                 (search-again)))]
            [define replace-all
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
                 (send replacee-edit end-edit-sequence)))]
            [define replace
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
                     #f)))]
            [define toggle-search-focus
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
                     focus))]
            [define move-to-search-or-search
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
                      (send find-canvas focus))]))]
            [define move-to-search-or-reverse-search
             (lambda ()
               (set-searching-frame this)
               (unhide-search)
               (if (or (send find-canvas has-focus?)
                       (send replace-canvas has-focus?))
                   (search-again 'backward)
                   (send find-canvas focus)))]
            [define search-again
             (opt-lambda ([direction searching-direction] [beep? #t])
               (set-searching-frame this)
               (unhide-search)
               (set-search-direction direction)
               (send find-edit search #t beep?))]
          
          (super-instantiate ())

          [define search-panel (make-object horizontal-panel% super-root '(border))]
          
          [define left-panel (make-object vertical-panel% search-panel)]
          [define find-canvas (make-object searchable-canvas% left-panel)]
          [define replace-canvas (make-object searchable-canvas% left-panel)]
          
          [define middle-left-panel (make-object vertical-pane% search-panel)]
          [define middle-middle-panel (make-object vertical-pane% search-panel)]
          [define middle-right-panel (make-object vertical-pane% search-panel)]
          
          [define search-button (make-object button% 
                                  "Search"
                                  middle-left-panel
                                  (lambda args (search-again)))]
          
          [define replace&search-button (make-object button% 
                                          "Replace && Search"
                                          middle-middle-panel 
                                          (lambda x (replace&search)))]
          [define replace-button (make-object button% "Replace" middle-left-panel (lambda x (replace)))]
          [define replace-all-button (make-object button% 
                                       "Replace To End"
                                       middle-middle-panel
                                       (lambda x (replace-all)))]
          
          [define dir-radio (make-object radio-box%
                              #f
                              (list "Forward" "Backward")
                              middle-right-panel
                              (lambda (dir-radio evt)
                                (let ([forward (if (= (send dir-radio get-selection) 0)
                                                   'forward
                                                   'backward)])
                                  (set-search-direction forward)
                                  (reset-search-anchor (get-text-to-search)))))]
          [define close-button (make-object button% "Hide"
                                 middle-right-panel
                                 (lambda args (hide-search)))]
          [define hidden? #f]

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
                    (list search-panel left-panel middle-left-panel middle-middle-panel middle-right-panel))
          (for-each (lambda (x) (send x stretchable-width #f))
                    (list middle-left-panel middle-middle-panel middle-right-panel))
          (send find-canvas set-editor find-edit)
          (send replace-canvas set-editor replace-edit) 
          (hide-search #t)))
      
      (define searchable-text<%> (interface (searchable<%> text<%>)))
      
      (define searchable-text-mixin
        (mixin (text<%> searchable<%>) (searchable-text<%>)
          (inherit get-editor)
          (override get-text-to-search)
          [define get-text-to-search
            (lambda ()
              (get-editor))]
          (override get-editor<%> get-editor%)
          [define get-editor<%> (lambda () text:searching<%>)]
          [define get-editor% (lambda () text:searching%)]
          (super-instantiate ())))
      
      ; to see printouts in memory debugging better.
      (define memory-text% (class100 text% args (sequence (apply super-init args))))
      (define memory-text (make-object memory-text%))
      (send memory-text hide-caret #t)
      (define show-memory-text? (directory-exists? (build-path (collection-path "framework") "CVS")))
      
      (define file<%> (interface (-editor<%>)))
      (define file-mixin
        (mixin (-editor<%>) (file<%>)
          (inherit get-editor get-filename get-label)
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
                                 "Close"
                                 #t
                                 this)
                            [(continue) #t]
                            [(save) (send edit save-file)]
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
      
      (define -pasteboard% (pasteboard-mixin editor%))
      (define pasteboard-info-file% (file-mixin -pasteboard%)))))
