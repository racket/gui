#lang racket/unit

  (require mzlib/class
           string-constants
           "sig.rkt"
           "../preferences.rkt"
           "../gui-utils.rkt"
           "interfaces.rkt"
           mzlib/etc
           mred/mred-sig
           racket/path
           racket/contract
           racket/format
           racket/match
           mrlib/panel-wob)
  
  (import mred^
          [prefix autosave: framework:autosave^]
          [prefix finder: framework:finder^]
          [prefix path-utils: framework:path-utils^]
          [prefix keymap: framework:keymap^]
          [prefix icon: framework:icon^]
          [prefix text: framework:text^]
          [prefix pasteboard: framework:pasteboard^]
          [prefix frame: framework:frame^]
          [prefix handler: framework:handler^])
  (export (rename framework:editor^
                  [-keymap<%> keymap<%>]))
  (init-depend mred^ framework:autosave^)
  
  ;; renaming, for editor-mixin where get-file is shadowed by a method.
  (define mred:get-file get-file) 
  
  (define basic<%> editor:basic<%>)
  
  (define basic-mixin
    (mixin (editor<%>) (basic<%>)
      (inherit begin-edit-sequence end-edit-sequence)
      
      (define/pubment (can-close?) (inner #t can-close?))
      (define/pubment (on-close) (inner (void) on-close))
      (define/public (close) (if (can-close?)
                                 (begin (on-close) #t)
                                 #f))
      
      (define/public (get-pos/text event)
        (get-pos/text-dc-location (send event get-x) (send event get-y)))
      
      (define/public (get-pos/text-dc-location event-x event-y)
        (let ([on-it? (box #f)])
          (let loop ([editor this])
            (let-values ([(x y) (send editor dc-location-to-editor-location event-x event-y)])
              (cond
                [(is-a? editor text%)
                 (let ([pos (send editor find-position x y #f on-it?)])
                   (cond
                     [(not (unbox on-it?)) (values #f #f)]
                     [else
                      (let ([snip (send editor find-snip pos 'after-or-none)])
                        (if (and snip
                                 (is-a? snip editor-snip%))
                            (loop (send snip get-editor))
                            (values pos editor)))]))]
                [(is-a? editor pasteboard%)
                 (let ([snip (send editor find-snip x y)])
                   (if (and snip
                            (is-a? snip editor-snip%))
                       (loop (send snip get-editor))
                       (values #f editor)))]
                [else (values #f #f)])))))
      
      ;; get-filename/untitled-name : -> string
      ;; returns a string representing the visible name for this file,
      ;; or "Untitled <n>" for some n.
      (define untitled-name #f)
      (define/public (get-filename/untitled-name)
        (let ([filename (get-filename)])
          (if filename
              (path->string filename)
              (begin
                (unless untitled-name
                  (set! untitled-name (gui-utils:next-untitled-name)))
                untitled-name))))
      
      (inherit get-filename save-file)
      (define/public save-file/gui-error
        (opt-lambda ([input-filename #f]
                     [fmt 'same]
                     [show-errors? #t])
          (let ([filename (if (or (not input-filename)
                                  (equal? input-filename ""))
                              (let ([internal-filename (get-filename)])
                                (if (or (not internal-filename)
                                        (equal? internal-filename ""))
                                    (put-file #f #f)
                                    internal-filename))
                              input-filename)])
            (with-handlers ([exn:fail?
                             (λ (exn)
                               (message-box
                                (string-constant error-saving)
                                (string-append
                                 (format (string-constant error-saving-file/name) 
                                         filename)
                                 "\n\n"
                                 (format-error-message exn))
                                (find-parent/editor this)
                                '(stop ok))
                               #f)])
              (when filename
                (save-file filename fmt #f))
              (and filename #t)))))
      
      (inherit load-file)
      (define/public load-file/gui-error
        (opt-lambda ([input-filename #f]
                     [fmt 'guess]
                     [show-errors? #t])
          (let ([filename (if (or (not input-filename)
                                  (equal? input-filename ""))
                              (let ([internal-filename (get-filename)])
                                (if (or (not internal-filename)
                                        (equal? internal-filename ""))
                                    (get-file #f)
                                    internal-filename))
                              input-filename)])
            (with-handlers ([exn:fail?
                             (λ (exn)
                               (message-box 
                                (string-constant error-loading)
                                (string-append
                                 (format (string-constant error-loading-file/name)
                                         filename)
                                 "\n\n"
                                 (format-error-message exn))
                                (find-parent/editor this)
                                '(stop ok))
                               #f)])
              (load-file input-filename fmt show-errors?)
              #t))))

      (define/public (revert/gui-error)
        (define b (box #f))
        (define filename (get-filename b))
        (cond
          [(and filename
                (not (unbox b)))
           (define start
             (if (is-a? this text%)
                 (send this get-start-position)
                 #f))
           (begin-edit-sequence)
           (define status (load-file/gui-error filename 'guess #f))
           (when status
             (when (is-a? this text%)
               (send this set-position start start)))
           (end-edit-sequence)
           status]
          [else #t]))
      
      (define/private (format-error-message exn)
        (if (exn? exn)
            (format "~a" (exn-message exn))
            (format "uncaught exn: ~s" exn)))
      
      (inherit refresh-delayed? 
               get-canvas
               get-admin)
      
      (define/augment (can-save-file? filename format)
        (define (ask-users-opinion)
          (cond
            [(doing-autosave?)
             ;; opt for the effect of
             ;; clicking on the `cancel`
             ;; button
             #f]
            [else
             (gui-utils:get-choice
              (string-constant file-has-been-modified)
              (string-constant overwrite-file-button-label)
              (string-constant cancel)
              (string-constant warning)
              #f
              (get-top-level-window)
              #:dialog-mixin frame:focus-table-mixin)]))
        (define result
          (and (if (equal? filename (get-filename))
                   (if (save-file-out-of-date?)
                       (ask-users-opinion)
                       #t)
                   #t)
               (inner #t can-save-file? filename format)))
        (when result (set! can-save-file-filename filename))
        result)

      (define can-save-file-filename #f)
      (define last-saved-file-time #f)
      
      (define/augment (after-save-file success?)
        (define temp-b (box #f))
        (define filename (get-filename temp-b))

        ;; update recently opened file names
        (unless (unbox temp-b)
          (when filename
            (handler:add-to-recent filename)))

        ;; if the filenames are different, then the save
        ;; was an auto-save to a temporary file so we
        ;; don't want to update the last-saved-file-time
        (when (equal? filename can-save-file-filename)
          (unless (unbox temp-b)
            (when success?
              (set! last-saved-file-time
                    (and filename
                         (file-exists? filename)
                         (file-or-directory-modify-seconds filename))))))

        (set! can-save-file-filename #f)
        (inner (void) after-save-file success?))
      
      (define/augment (after-load-file success?)
        (when success?
          (define temp-b (box #f))
          (define filename (get-filename temp-b))
          (unless (unbox temp-b)
            (set! last-saved-file-time
                  (and filename
                       (file-exists? filename)
                       (file-or-directory-modify-seconds filename)))))
        (inner (void) after-load-file success?))
      
      (define/public (save-file-out-of-date?)
        (and last-saved-file-time
             (let ([fn (get-filename)])
               (and fn
                    (file-exists? fn)
                    (let ([ms (file-or-directory-modify-seconds fn)])
                      (< last-saved-file-time ms))))))
      
      (define has-focus #f)
      (define/override (on-focus x)
        (set! has-focus x)
        (super on-focus x))
      (define/public (has-focus?) has-focus)
      
      (define/public (get-top-level-window)
        (let loop ([text this])
          (let ([editor-admin (send text get-admin)])
            (cond
              [(is-a? editor-admin editor-snip-editor-admin<%>)
               (let* ([snip (send editor-admin get-snip)]
                      [snip-admin (send snip get-admin)])
                 (loop (send snip-admin get-editor)))]
              [(send text get-canvas) 
               => 
               (λ (canvas)
                 (send canvas get-top-level-window))]
              [else #f]))))
      
      [define edit-sequence-queue null]
      [define edit-sequence-ht (make-hasheq)]
      [define in-local-edit-sequence? #f]
      [define/public local-edit-sequence? (λ () in-local-edit-sequence?)]
      [define/public run-after-edit-sequence
        (case-lambda 
          [(t) (run-after-edit-sequence t #f)]
          [(t sym)
           (unless (and (procedure? t)
                        (= 0 (procedure-arity t)))
             (error 'editor:basic::run-after-edit-sequence
                    "expected procedure of arity zero, got: ~s\n" t))
           (unless (or (symbol? sym) (not sym))
             (error 'editor:basic::run-after-edit-sequence
                    "expected second argument to be a symbol or #f, got: ~s\n"
                    sym))
           (if (refresh-delayed?)
               (if in-local-edit-sequence?
                   (cond
                     [(symbol? sym)
                      (hash-set! edit-sequence-ht sym t)]
                     [else (set! edit-sequence-queue
                                 (cons t edit-sequence-queue))])
                   (let ([snip-admin (get-admin)])
                     (cond
                       [(not snip-admin)
                        (t)] ;; refresh-delayed? is always #t when there is no admin.
                       [(is-a? snip-admin editor-snip-editor-admin<%>)
                        (let loop ([ed this])
                          (let ([snip-admin (send ed get-admin)])
                            (if (is-a? snip-admin editor-snip-editor-admin<%>)
                                (let ([up-one
                                       (send (send (send snip-admin get-snip) get-admin) get-editor)])
                                  (if (is-a? up-one basic<%>)
                                      (send up-one run-after-edit-sequence t sym)
                                      (loop up-one)))
                                
                                ;; here we are in an embdedded editor that is not
                                ;; in an edit sequence and the "parents" of the embdedded editor
                                ;; are all non-basic<%> objects, so we just run the thunk now.
                                (t))))]
                       [else
                        '(message-box "run-after-edit-sequence error"
                                      (format "refresh-delayed? is #t but snip admin, ~s, is not an editor-snip-editor-admin<%>"
                                              snip-admin))
                        '(t)
                        (void)])))
               (t))
           (void)])]
      [define/public extend-edit-sequence-queue
        (λ (l ht)
          (hash-for-each ht (λ (k t) (hash-set! edit-sequence-ht k t)))
          (set! edit-sequence-queue (append l edit-sequence-queue)))]
      (define/augment (on-edit-sequence)
        (set! in-local-edit-sequence? #t)
        (inner (void) on-edit-sequence))
      (define/augment (after-edit-sequence)
        (set! in-local-edit-sequence? #f)
        (let ([queue edit-sequence-queue]
              [ht edit-sequence-ht]
              [find-enclosing-editor
               (λ (editor)
                 (let ([admin (send editor get-admin)])
                   (cond
                     [(is-a? admin editor-snip-editor-admin<%>)
                      (send (send (send admin get-snip) get-admin) get-editor)]
                     [else #f])))])
          (set! edit-sequence-queue null)
          (set! edit-sequence-ht (make-hash))
          (let loop ([editor (find-enclosing-editor this)])
            (cond
              [(and editor 
                    (is-a? editor basic<%>)
                    (not (send editor local-edit-sequence?)))
               (loop (find-enclosing-editor editor))]
              [(and editor
                    (is-a? editor basic<%>))
               (send editor extend-edit-sequence-queue queue ht)]
              [else
               (hash-for-each ht (λ (k t) (t)))
               (for-each (λ (t) (t)) queue)])))
        (inner (void) after-edit-sequence))
      
      (define/override (on-new-box type)
        (cond
          [(eq? type 'text) (make-object editor-snip% (make-object text:basic%))]
          [else (make-object editor-snip% (make-object pasteboard:basic%))]))
      
      (define/override (on-new-image-snip filename kind relative-path? inline?)
        (super on-new-image-snip 
               filename
               (if (eq? kind 'unknown) 'unknown/mask kind) 
               relative-path? 
               inline?))
      
      (define/override (get-file d)
        (parameterize ([finder:dialog-parent-parameter
                        (get-top-level-window)])
          (finder:get-file d)))
      (define/override (put-file d f)
        (parameterize ([finder:dialog-parent-parameter
                        (get-top-level-window)])
          (finder:put-file f d)))
      
      (super-new)))
  
  (define standard-style-list (new style-list%))
  (define (get-standard-style-list) standard-style-list)

;; this name can never change as the name is used directly in mrlib
;; and we cannot add a dependency from mrlib to the framework
  (define default-color-style-name "framework:default-color")
  (define (get-default-color-style-name) default-color-style-name)
  
  (let ([delta (make-object style-delta% 'change-normal)])
    (send delta set-delta 'change-family 'modern)
    (let ([style (send standard-style-list find-named-style "Standard")])
      (if style
          (send style set-delta delta)
          (send standard-style-list new-named-style "Standard"
                (send standard-style-list find-or-create-style
                      (send standard-style-list basic-style)
                      delta)))))
  
  (let ([delta (make-object style-delta%)]
        [style (send standard-style-list find-named-style default-color-style-name)])
    (if style
        (send style set-delta delta)
        (send standard-style-list new-named-style default-color-style-name
              (send standard-style-list find-or-create-style
                    (send standard-style-list find-named-style "Standard")
                    delta))))
  
(define (set-default-font-color color [bg-color #f])
  (define the-standard (send standard-style-list find-named-style default-color-style-name))
  (define the-delta (make-object style-delta%))
  (send the-standard get-delta the-delta)
  (send the-delta set-delta-foreground color)
  (when bg-color (send the-delta set-delta-background bg-color))
  (send the-standard set-delta the-delta))
  
  (define (set-font-size size)
    (update-standard-style
     (λ (the-delta)
       (send the-delta set-size-mult 0)
       (send the-delta set-size-add size))))
  
  (define (set-font-name name)
    (update-standard-style
     (λ (the-delta)
       (send the-delta set-delta-face name)
       (send the-delta set-family 'modern))))
  
  (define (set-font-smoothing sym)
    (update-standard-style
     (λ (the-delta)
       (send the-delta set-smoothing-on sym))))

  (define (set-font-weight sym)
    (update-standard-style
     (λ (the-delta)
       (send the-delta set-weight-on sym))))
  
  (define (update-standard-style cng-delta)
    (let* ([the-standard (send standard-style-list find-named-style "Standard")]
           [the-delta (make-object style-delta%)])
      (send the-standard get-delta the-delta)
      (cng-delta the-delta)
      (send the-standard set-delta the-delta)))
  
  (define standard-style-list<%>
    (interface (editor<%>)
      ))
  
  (define standard-style-list-mixin
    (mixin (editor<%>) (standard-style-list<%>)
      (super-new)
      (inherit set-style-list set-load-overwrites-styles)
      (set-style-list standard-style-list)
      (set-load-overwrites-styles #f)))
  
    
  ;; the 'set-font-size' function can be slow,
  ;; as it involves redrawing every frame
  ;; so we do the change on a low-priority
  ;; callback so we don't get too many of these
  ;; piling up.
  (define (set-font-size/callback size)
    (set! set-font-size-callback-size size)
    (unless set-font-size-callback-running?
      (set! set-font-size-callback-running? #t)
      (queue-callback
       (λ ()
         (set-font-size set-font-size-callback-size)
         (set! set-font-size-callback-running? #f))
       #f)
      (set! set-font-size-callback-running? #t)))  
  (define set-font-size-callback-running? #f)
  (define set-font-size-callback-size #f)

  (define (set-standard-style-list-pref-callbacks)
    (set-font-size (get-current-preferred-font-size))
    (set-font-name (preferences:get 'framework:standard-style-list:font-name))
    (set-font-smoothing (preferences:get 'framework:standard-style-list:smoothing))
    (set-font-weight (preferences:get 'framework:standard-style-list:weight))
    (preferences:add-callback 'framework:standard-style-list:font-size 
                              (λ (p v)
                                (set-font-size/callback (font-size-pref->current-font-size v))))
    (preferences:add-callback 'framework:standard-style-list:font-name (λ (p v) (set-font-name v)))
    (preferences:add-callback 'framework:standard-style-list:smoothing
                              (λ (p v) (set-font-smoothing v)))
    (preferences:add-callback 'framework:standard-style-list:weight (λ (p v) (set-font-weight v)))
    (define fl (get-face-list))
    (unless (member (preferences:get 'framework:standard-style-list:font-name) fl)
      (define preferred-font 
        (cond
          [(equal? (system-type) 'macosx)
           (define preferred-font "Menlo")
           (if (member preferred-font fl)
               preferred-font
               (get-family-builtin-face 'modern))]
          [else (get-family-builtin-face 'modern)]))
      (preferences:set 'framework:standard-style-list:font-name preferred-font)))
  
  (define (get-current-preferred-font-size)
    (font-size-pref->current-font-size (preferences:get 'framework:standard-style-list:font-size)))
  
  (define (font-size-pref->current-font-size v)
    (define default-size (vector-ref v 1))
    (cond
      [change-font-size-when-monitors-change?
       (define monitor-sizes (get-current-monitor-sizes))
       (hash-ref (vector-ref v 0) monitor-sizes default-size)]
      [else
       default-size]))
  
  (define change-font-size-when-monitors-change? #f)
  (define (get-change-font-size-when-monitors-change?)
    change-font-size-when-monitors-change?)
  (define (set-change-font-size-when-monitors-change? b?)
    (unless (equal? change-font-size-when-monitors-change? b?)
      (set! change-font-size-when-monitors-change? b?)
      (set-current-preferred-font-size
       (get-current-preferred-font-size))))
  
  
  (define (set-current-preferred-font-size new-size)
    (unless (exact-nonnegative-integer? new-size)
      (raise-argument-error 'set-current-preferred-font-size
                            "exact-nonnegative-integer?"
                            new-size))
    (define old-pref (preferences:get 'framework:standard-style-list:font-size))
    (define current-mons (get-current-monitor-sizes))
    (define new-monitor-sizes
      (hash-set (vector-ref old-pref 0)
                current-mons
                new-size))
    (preferences:set 'framework:standard-style-list:font-size
                     (vector new-monitor-sizes new-size)))
  
  (define (get-current-monitor-sizes)
    (let loop ([m (get-display-count)]
               [sizes '()])
      (cond
        [(zero? m) sizes]
        [else
         (define-values (w h) (get-display-size #:monitor (- m 1)))
         (loop (- m 1)
               (if (and w h)
                   (cons (list w h) sizes)
                   sizes))])))
  
  ;; set-standard-style-list-delta : string (is-a?/c style-delta<%>) -> void
  (define (set-standard-style-list-delta name delta)
    (let* ([style-list (get-standard-style-list)]
           [style (send style-list find-named-style name)])
      (if style
          (send style set-delta delta)
          (send style-list new-named-style name
                (send style-list find-or-create-style
                      (send style-list find-named-style "Standard")
                      delta)))
      (void)))
  
  (define -keymap<%> editor:keymap<%>)
  (define keymap-mixin
    (mixin (basic<%>) (-keymap<%>)
      (define/public (get-keymaps)
        (list (keymap:get-user) (keymap:get-global)))
      (inherit set-keymap)
      
      (super-new)
      (let ([keymap (make-object keymap:aug-keymap%)])
        (set-keymap keymap)
        (for-each (λ (k) (send keymap chain-to-keymap k #f))
                  (get-keymaps)))))
  
  (define (add-after-user-keymap km kms)
    (let loop ([kms kms])
      (cond
        [(null? kms) (list km)]
        [else
         (let ([f (car kms)])
           (if (eq? f (keymap:get-user))
               (list* f km (cdr kms))
               (cons f (loop (cdr kms)))))])))
  
  (define autowrap<%> (interface (basic<%>)))
  (define autowrap-mixin
    (mixin (basic<%>) (autowrap<%>)
      (inherit auto-wrap)
      (super-instantiate ())
      (auto-wrap 
       (preferences:get
        'framework:auto-set-wrap?))))
  
  (define file<%> 
    (interface (-keymap<%>)
      get-can-close-parent
      update-frame-filename
      allow-close-with-no-filename?))
  
  (define file-mixin
    (mixin (-keymap<%>) (file<%>)
      (inherit get-filename lock get-style-list 
               is-modified? set-modified 
               get-top-level-window)
      
      (inherit get-canvases get-filename/untitled-name)
      (define/public (update-frame-filename)
        (let* ([filename (get-filename)]
               [name (if filename
                         (path->string 
                          (file-name-from-path 
                           filename))
                         (get-filename/untitled-name))])
          (for-each (λ (canvas)
                      (let ([tlw (send canvas get-top-level-window)])
                        (when (and (is-a? tlw frame:editor<%>)
                                   (eq? this (send tlw get-editor)))
                          (send tlw set-label name))))
                    (get-canvases))))
      
      (define/override set-filename
        (case-lambda
          [(name) (set-filename name #f)]
          [(name temp?)
           (super set-filename name temp?)
           (unless temp?
             (update-frame-filename))]))
      
      (inherit save-file)
      (define/public (allow-close-with-no-filename?) #f)
      (define/augment (can-close?)
        (and (user-saves-or-not-modified?)
             (inner #t can-close?)))
      
      (define/public (user-saves-or-not-modified? [allow-cancel? #t])
        (or (not (is-modified?))
            (and (not (get-filename))
                 (allow-close-with-no-filename?))
            (case (gui-utils:unsaved-warning
                   (get-filename/untitled-name)
                   (string-constant dont-save)
                   #t
                   (or (get-top-level-window)
                       (get-can-close-parent))
                   allow-cancel?
                   #:dialog-mixin frame:focus-table-mixin)
              [(continue) #t]
              [(save) (save-file)]
              [else #f])))
      
      (define/public (get-can-close-parent) #f)
      
      (define/override (get-keymaps)
        (add-after-user-keymap (keymap:get-file) (super get-keymaps)))
      (super-new)))
  
  (define backup-autosave<%>
    (interface (basic<%>)
      backup?
      autosave?
      do-autosave
      remove-autosave))

  (define doing-autosave? (make-parameter #f))

  (define backup-autosave-mixin
    (mixin (basic<%>) (backup-autosave<%> autosave:autosavable<%>)
      (inherit is-modified? get-filename save-file)
      [define auto-saved-name #f]
      [define auto-save-out-of-date? #t]
      [define auto-save-error? #f]
      (define/private (file-old? filename)
        (if (and filename
                 (file-exists? filename))
            (let ([modified-seconds (file-or-directory-modify-seconds filename)]
                  [old-seconds (- (current-seconds) (* 7 24 60 60))])
              (< modified-seconds old-seconds))
            #t))
      (define/public (backup?) (preferences:get 'framework:backup-files?))
      (define/augment (on-save-file name format)
        (when (and (backup?)
                   (not (eq? format 'copy))
                   (file-exists? name))
          (let ([back-name (path-utils:generate-backup-name name)])
            (when (or (not (file-exists? back-name))
                      (file-old? back-name))
              (with-handlers ([exn:fail? 
                               (λ (exn)
                                 (log-debug "failed to clean up autosave file.1: ~a" back-name))])
                (when (file-exists? back-name)
                  (delete-file back-name))
                (copy-file name back-name)))))
        (inner (void) on-save-file name format))
      (define/augment (after-save-file success?)
        (when success?
          (set! auto-save-error? #f))
        (inner (void) after-save-file success?))

      (define/augment (on-close)
        (remove-autosave)
        (set! do-autosave? #f)
        (inner (void) on-close))
      (define/augment (on-change)
        (set! auto-save-out-of-date? #t)
        (inner (void) on-change))
      (define/override (set-modified modified?)
        (when auto-saved-name
          (if modified?
              (set! auto-save-out-of-date? #t)
              (remove-autosave)))
        (super set-modified modified?))
      
      [define do-autosave? #t]
      (define/public (autosave?) do-autosave?)
      
      (define/public (do-autosave)
        (cond
          [(and (autosave?)
                (not auto-save-error?)
                (is-modified?)
                (or (not auto-saved-name)
                    auto-save-out-of-date?))
           (let* ([orig-name (get-filename)]
                  [old-auto-name auto-saved-name]
                  [auto-name (path-utils:generate-autosave-name orig-name)]
                  [orig-format (and (is-a? this text%)
                                    (send this get-file-format))])
             (when (is-a? this text%)
               (send this set-file-format 'standard))
             (with-handlers ([exn:fail?
                              (λ (exn)
                                (show-autosave-error exn orig-name)
                                (set! auto-save-error? #t)
                                (when (is-a? this text%)
                                  (send this set-file-format orig-format))
                                #f)])
               (parameterize ([doing-autosave? #t])
                 (save-file auto-name 'copy #f))
               (when (is-a? this text%)
                 (send this set-file-format orig-format))
               (when old-auto-name
                 (when (file-exists? old-auto-name)
                   (delete-file old-auto-name)))
               (set! auto-saved-name auto-name)
               (set! auto-save-out-of-date? #f)
               auto-name))]
          [else auto-saved-name]))
      
      ;; show-autosave-error : any (union #f string) -> void
      ;; opens a message box displaying the exn and the filename
      ;; to the user.
      (define/private (show-autosave-error exn orig-name)
        (message-box 
         (string-constant warning)
         (apply
          string-append
          (format (string-constant error-autosaving)
                  (or orig-name (string-constant untitled)))
          "\n"
          (string-constant autosaving-turned-off)
          "\n\n"
          (if (exn? exn)
              (format "~a" (exn-message exn))
              (format "~s" exn))
          "\n\n"
          (if (and (exn? exn)
                   (continuation-mark-set? (exn-continuation-marks exn)))
              (for/list ([fr (in-list (continuation-mark-set->context 
                                       (exn-continuation-marks exn)))])
                (format "   ~s\n" fr))
              '()))
         (find-parent/editor this)
         '(caution ok)))
      
      (define/public (remove-autosave)
        (when auto-saved-name
          (when (file-exists? auto-saved-name)
            (with-handlers ([exn:fail? 
                             (λ (exn)
                               (log-debug "failed to clean up autosave file.2: ~a" 
                                          auto-saved-name))])            
              (delete-file auto-saved-name)
              (set! auto-saved-name #f)))))
      (super-new)
      (autosave:register this)))

(define-local-member-name autoload-file-changed)

(define autoload<%>
  (interface (basic<%>)))

(define autoload-mixin
  (mixin (basic<%>) (autoload<%>)
    (inherit get-filename load-file
             begin-edit-sequence end-edit-sequence
             is-modified?)

    (define/override (set-filename filename [temporary? #f])
      (unless (equal? #f (preferences:get 'framework:autoload))
        (unless temporary?
          (start-the-monitor filename)))
      (super set-filename filename temporary?))

    (define/augment (on-close)
      (stop-the-monitor)
      (inner (void) on-close))

    (define/private (start-the-monitor _filename)
      (define filename
        (or _filename
            (let ([b (box #f)])
              (define f (get-filename b))
              (and (not (unbox b)) f))))
      (when filename
        (monitor-a-file this filename (current-eventspace))))
    (define/private (stop-the-monitor)
      (un-monitor-a-file this))

    ;; not supposed to be a method to ensure
    ;; the callback stays registered with the
    ;; preferences system as as long as `this`
    ;; is held onto
    (define pref-callback
      (λ (p v)
        (case v
          [(#f) (stop-the-monitor)]
          [(#t) (start-the-monitor #f)]
          [(ask) (start-the-monitor #f)])))
    (preferences:add-callback 'framework:autoload pref-callback #t)

    (define/public (autoload-file-changed)
      (when (ask-can-revert-and-maybe-restart-monitor)
        (define b (box #f))
        (define filename (get-filename b))
        (when (and filename
                   (not (unbox b)))
          (define start
            (if (is-a? this text%)
                (send this get-start-position)
                #f))
          (begin-edit-sequence)
          (define failed?
            (with-handlers ([exn:fail? (λ (x) #t)])
              (load-file filename 'guess #f)))
          (unless failed?
            (when (is-a? this text%)
              (send this set-position start start)))
          (end-edit-sequence))))

    (define/private (ask-can-revert-and-maybe-restart-monitor)
      (cond
        [(is-modified?)
         (define button
           (message-box/custom
            (string-constant warning)
            (string-constant autoload-file-changed-on-disk-editor-dirty)
            (string-constant revert)
            (string-constant ignore)
            #f
            (find-parent/editor this)
            '(caution no-default)
            2
            #:dialog-mixin frame:focus-table-mixin))

         ;; restart the monitor as long as there is a possibility
         ;; we'll revert the buffer
         (unless (equal? (preferences:get 'framework:autoload) #f)
           (start-the-monitor #f))
         (case button
           [(1) #t]
           [(2) #f])]
        [(equal? (preferences:get 'framework:autoload) 'ask)
         (define-values (button checked?)
           (message+check-box/custom
            (string-constant warning)
            (string-constant autoload-file-changed-on-disk)
            (string-constant dont-ask-again-always-current)
            (string-constant revert)
            (string-constant ignore)
            #f
            (find-parent/editor this)
            '(caution no-default)
            2
            #:dialog-mixin frame:focus-table-mixin))
         (define answer (case button
                          [(1) #t]
                          [(2) #f]))
         (cond
           [checked?
            ;; setting the preference will start the monitor
            ;; if `answer` is #t
            (preferences:set 'framework:autoload answer)]
           [else
            ;; and thus we need to start it otherwise
            (start-the-monitor #f)])
         answer]
        [(equal? (preferences:get 'framework:autoload) #t)
         (start-the-monitor #f) #t]
        [(equal? (preferences:get 'framework:autoload) #f)
         ;; here we don't want to restart the monitor
         ;; as setting the preference to #t will do that
         ;; (it is surprising it is on in this case actually)
         #f]))

    (super-new)))

(define (find-parent/editor editor)
  (let loop ([editor editor])
    (define ed-admin (send editor get-admin))
    (cond
      [(not ed-admin) #f]
      [(is-a? ed-admin editor-snip-editor-admin<%>)
       (define snip (send ed-admin get-snip))
       (define snip-admin (send snip get-admin))
       (loop (send snip-admin get-editor))]
      [else
       (define canvas (send editor get-canvas))
       (and canvas (find-parent/window canvas))])))

(define (find-parent/window win)
  (let loop ([win win])
    (cond
      [(or (is-a? win frame%)
           (is-a? win dialog%))
       win]
      [else
       (define p (send win get-parent))
       (and p (loop p))])))

(define (monitor-a-file txt path eventspace)
  ;; grab the monitoring event on the same event
  ;; to facilitate testing
  (define mod-time (file-or-directory-modify-seconds path))
  (define evt (filesystem-change-evt path #f))
  (define size (file-size path))
  (channel-put filename-changed-chan (vector txt path eventspace mod-time size evt)))
(define filename-changed-chan (make-channel))
(define (un-monitor-a-file txt)
  (channel-put unmonitor-chan txt))
(define unmonitor-chan (make-channel))
(void
 (thread
  (λ ()
    (struct monitored (path evt mod-time size eventspace) #:transparent)
    ;; state : hash[txt -o> monitored?]
    (let loop ([state (hash)])
      (apply
       sync
       (handle-evt
        unmonitor-chan
        (λ (txt)
          (define old (hash-ref state txt #f))
          (cond
            [old
             (filesystem-change-evt-cancel (monitored-evt old))
             (loop (hash-remove state txt))]
            [else (loop state)])))
       (handle-evt
        filename-changed-chan
        (λ (txt+path+eventspace)
          (match-define (vector txt path eventspace mod-time size evt)
            txt+path+eventspace)
          (cond
            [evt
             (define old (hash-ref state txt #f))
             (when old (filesystem-change-evt-cancel (monitored-evt old)))
             (loop (hash-set state
                             txt
                             (monitored path evt
                                        mod-time size
                                        eventspace)))]
            [else
             ;; failed to create an evt, so give up
             ;; trying to monitor this file
             (loop state)])))
       (for/list ([(txt a-monitored) (in-hash state)])
         (match-define (monitored path evt mod-time size eventspace)
           a-monitored)
         (handle-evt
          evt
          (λ (_)
            (match-define (vector _1 _2 _3 can-track-file-level-changes?)
              (system-type 'fs-change))
            (cond
              [(or can-track-file-level-changes?
                   (< mod-time (file-or-directory-modify-seconds path))
                   (not (= (file-size path) size)))
               ;; the `or` above ensures that the file actually is changed,
               ;; as it might not be on some platforms
               (parameterize ([current-eventspace eventspace])
                 (queue-callback
                  (λ ()
                    (send txt autoload-file-changed))))
               (loop (hash-remove state txt))]
              [else
               ;; the file appears to not actually be modified.
               ;; try to create a new evt to wait again
               (define new-evt (filesystem-change-evt path #f))
               (cond
                 [evt
                  (loop (hash-set state
                                  txt
                                  (monitored
                                   path
                                   new-evt
                                   mod-time
                                   size
                                   eventspace)))]
                 [else
                  ;; we failed to create an evt; give up on
                  ;; monitoring this file (can we do better?)
                  (loop (hash-remove state txt))])])))))))))

  (define info<%> (interface (basic<%>)))
  (define info-mixin
    (mixin (basic<%>) (info<%>)
      (inherit get-top-level-window run-after-edit-sequence)
      (define callback-running? #f)
      (define/override (lock x)
        (super lock x)
        (run-after-edit-sequence
         (rec send-frame-update-lock-icon
           (λ ()
             (unless callback-running?
               (set! callback-running? #t)
               (queue-callback
                (λ ()
                  (let ([frame (get-top-level-window)])
                    (when (is-a? frame frame:info<%>)
                      (send frame lock-status-changed)))
                  (set! callback-running? #f))
                #f))))
         'framework:update-lock-icon))
      (super-new)))

(define font-size-message%
  (class canvas%
    (init message
          [stretchable-height #f])
    (init-field [text-alignment 'center])
    (define msgs
      (cond
        [(string? message) (regexp-split #rx"\n" message)]
        [((listof string?) message) message]
        [else
         (raise-argument-error 'editor:font-size-message%
                               (~s '(or/c string? (listof string?)))
                               message)]))
    (unless (member text-alignment '(left center right))
      (raise-argument-error 'editor:font-size-message%
                            (~s '(or/c 'left 'center 'right))
                            text-alignment))
    (inherit refresh get-dc get-client-size popup-menu)
    (define/public (set-message message)
      (set! msgs
            (cond
              [(string? message) (regexp-split #rx"\n" message)]
              [((listof string?) message) message]
              [else
               (raise-argument-error 'editor:font-size-message%::set-label
                                     (~s '(or/c string? (listof string?)))
                                     message)]))
      (refresh))
    (define/override (on-paint)
      (define dc (get-dc))
      (define text-foreground (send dc get-text-foreground))
      (when (white-on-black-panel-scheme?)
        (send dc set-text-foreground "white"))
      (define-values (cw ch) (get-client-size))
      (define-values (tot-th tot-tw)
        (for/fold ([tot-th 0] [tot-tw 0])
                  ([msg (in-list msgs)])
          (define-values (tw th td ta) (send dc get-text-extent msg))
          (values (+ tot-th th) (max tot-tw tw))))
      (for/fold ([y (- (/ ch 2) (/ tot-th 2))]) ([msg (in-list msgs)])
        (define-values (tw th td ta) (send dc get-text-extent msg))
        (define x
          (case text-alignment
            [(center) (- (/ cw 2) (/ tw 2))]
            [(left) 2]
            [(right) (- cw 2)]))
        (send dc draw-text msg x y)
        (+ y th))
      (send dc set-text-foreground text-foreground))
      (super-new [style '(transparent)][stretchable-height stretchable-height])
      
      ;; need object to hold onto this function, so this is
      ;; intentionally a private field, not a method
      (define (font-size-changed-callback _ new-prefs)
        (define new-size (font-size-pref->current-font-size new-prefs))
        (set-the-height/dc-font new-size)
        (refresh))
      (preferences:add-callback
       'framework:standard-style-list:font-size
       font-size-changed-callback
       #t)
      
      (define/private (set-the-height/dc-font font-size)
        (define dc (get-dc))
        (send dc set-font 
              (send the-font-list find-or-create-font
                    font-size
                    (send normal-control-font get-family)
                    (send normal-control-font get-style)
                    (send normal-control-font get-weight)
                    (send normal-control-font get-underlined)
                    (send normal-control-font get-smoothing)))
        (define tot-th
          (for/sum ([msg (in-list msgs)])
            (define-values (tw th td ta) (send dc get-text-extent msg))
            th))
        (min-height (inexact->exact (ceiling tot-th))))
      
      (inherit min-height)
      (set-the-height/dc-font
       (get-current-preferred-font-size))))
