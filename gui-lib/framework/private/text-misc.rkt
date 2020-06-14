#lang racket/base

(require string-constants
         racket/class
         "sig.rkt"
         "../gui-utils.rkt"
         "../preferences.rkt"
         mred/mred-sig
         racket/unit
         "text-sig.rkt")

(provide text-misc@)

(define-unit text-misc@
  (import mred^
          text-basic^
          [prefix editor: framework:editor^]
          [prefix frame: framework:frame^])
  (export text-misc^)

  (define original-output-port (current-output-port))
  (define (oprintf . args) (apply fprintf original-output-port args))


  (define line-spacing<%> (interface ()))

  (define line-spacing-mixin
    (mixin (basic<%>) (line-spacing<%>)
      (super-new)
      (inherit set-line-spacing)
      ;; this is a field so that the weakly
      ;; held callback works out properly
      (define (pref-cb-func sym val)
        (set-line-spacing (if val 1 0)))
      (preferences:add-callback 'framework:line-spacing-add-gap?
                                pref-cb-func
                                #t)
      (set-line-spacing (if (preferences:get 'framework:line-spacing-add-gap?)
                            1
                            0))))

  (define foreground-color<%>
    (interface (basic<%> editor:standard-style-list<%>)
      ))

  (define foreground-color-mixin
    (mixin (basic<%> editor:standard-style-list<%>) (foreground-color<%>)
      (inherit begin-edit-sequence end-edit-sequence change-style get-style-list)
    
      (define/override (default-style-name)
        (editor:get-default-color-style-name))
    
      (define/override (get-fixed-style)
        (send (editor:get-standard-style-list)
              find-named-style
              (editor:get-default-color-style-name)))
      (super-new)))

  (define hide-caret/selection<%> (interface (basic<%>)))
  (define hide-caret/selection-mixin
    (mixin (basic<%>) (hide-caret/selection<%>)
      (inherit get-start-position get-end-position hide-caret)
      (define/augment (after-set-position)
        (hide-caret (= (get-start-position) (get-end-position)))
        (inner (void) after-set-position))
      (super-new)))

  (define nbsp->space<%> (interface ((class->interface text%))))
  (define nbsp->space-mixin
    (mixin ((class->interface text%)) (nbsp->space<%>)
      (field [rewriting #f])
      (inherit begin-edit-sequence end-edit-sequence delete insert get-character)
      (define/augment (on-insert start len)
        (inner (void) on-insert start len)
        (begin-edit-sequence #t #f))
      (inherit find-string)
      (define/augment (after-insert start len)
        (unless rewriting
          (set! rewriting #t)
          (let ([str (string (integer->char 160))]
                [last-pos (+ start len)])
            (let loop ([pos start])
              (when (< pos last-pos)
                (let ([next-pos (find-string str 'forward pos last-pos)])
                  (when next-pos
                    (delete next-pos (+ next-pos 1) #f)
                    (insert " " next-pos next-pos #f)
                    (loop (+ next-pos 1)))))))
          (set! rewriting #f))
        (end-edit-sequence)
        (inner (void) after-insert start len))
      (super-new)))

  (define return<%> (interface ((class->interface text%))))
  (define return-mixin
    (mixin ((class->interface text%)) (return<%>) 
      (init-field return)
      (define/override (on-local-char key)
        (let ([cr-code #\return]
              [lf-code #\newline]
              [code (send key get-key-code)])
          (or (and (char? code)
                   (or (char=? lf-code code)
                       (char=? cr-code code))
                   (return))
              (super on-local-char key))))
      (super-new)))

  (define info<%> (interface (basic<%>)))

  (define info-mixin
    (mixin (editor:keymap<%> basic<%>) (info<%>)
      (inherit get-start-position get-end-position get-canvas
               run-after-edit-sequence)
      (define/private (enqueue-for-frame call-method tag)
        (run-after-edit-sequence
         (let ([from-enqueue-for-frame
                (λ ()
                  (call-with-frame call-method))])
           from-enqueue-for-frame)
         tag))
    
      ;; call-with-frame : ((is-a?/c frame:text-info<%>) -> void) -> void
      ;; calls the argument thunk with the frame showing this editor.
      (define/private (call-with-frame call-method)
        (let ([canvas (get-canvas)])
          (when canvas
            (let ([frame (send canvas get-top-level-window)])
              (when (is-a? frame frame:text-info<%>)
                (call-method frame))))))
    
      (define/override (set-anchor x)
        (super set-anchor x)
        (enqueue-for-frame 
         (λ (x) (send x anchor-status-changed))
         'framework:anchor-status-changed))
      (define/override (set-overwrite-mode x)
        (super set-overwrite-mode x)
        (enqueue-for-frame
         (λ (x) (send x overwrite-status-changed))
         'framework:overwrite-status-changed))
      (define/augment (after-set-position)
        (maybe-queue-editor-position-update)
        (inner (void) after-set-position))
      (define/override use-file-text-mode
        (case-lambda
          [() (super use-file-text-mode)]
          [(x) (super use-file-text-mode x)
               (enqueue-for-frame
                (λ (x) (send x use-file-text-mode-changed))
                'framework:file-text-mode-changed)]))
    
      ;; maybe-queue-editor-position-update : -> void
      ;; updates the editor-position in the frame,
      ;; but delays it until the next low-priority event occurs.
      (define callback-running? #f)
      (define/private (maybe-queue-editor-position-update)
        (enqueue-for-frame 
         (λ (frame) 
           (unless callback-running?
             (set! callback-running? #t)
             (queue-callback
              (λ ()
                (send frame editor-position-changed)
                (set! callback-running? #f))
              #f)))
         'framework:info-frame:update-editor-position))
    
      (define/augment (after-insert start len)
        (maybe-queue-editor-position-update)
        (inner (void) after-insert start len))
      (define/augment (after-delete start len)
        (maybe-queue-editor-position-update)
        (inner (void) after-delete start len))
      (super-new)))

  (define clever-file-format<%> (interface ((class->interface text%))))

  (define clever-file-format-mixin
    (mixin ((class->interface text%)) (clever-file-format<%>)
      (inherit get-file-format set-file-format find-first-snip)
      
      ;; all-string-snips : -> boolean
      ;; returns #t when it is safe to save this file in regular (non-WXME) mode.
      (define/private (all-string-snips)
        (let loop ([s (find-first-snip)])
          (cond
            [(not s) #t]
            [(is-a? s string-snip%)
             (loop (send s next))]
            [else #f])))

      ;; Saving in text when wxme is needed looses data,
      ;; therefore if the user refuses to change file formats
      ;; abort save.
      (define/augment (can-save-file? name format)
        (define needs-wxme?
          (and (not (all-string-snips))
               (eq? format 'same)
               (eq? 'text (get-file-format))))
        (define (get-users-opinion)
          (cond
            [(editor:doing-autosave?)
             ;; we don't ask for the user's opinion here;
             ;; we opt for clicking the `dont-save` button
             ;; when the file is explicitly saved, we'll
             ;; do the actual asking
             3]
            [else
             (message-box/custom
              (string-constant warning)
              (string-constant save-as-binary-format)
              (string-constant convert-format)
              (string-constant keep-format)
              (string-constant dont-save)
              #f
              '(disallow-close default=3)
              3
              #:dialog-mixin frame:focus-table-mixin)]))
        (define format-converted
          (and needs-wxme?
               (or (not (preferences:get 'framework:verify-change-format))
                   (get-users-opinion))))
        (define continue-saving?
          (case format-converted
            [(1 #t)
             (set-file-format 'standard)
             #t]
            [(2) #t]
            [(3) #f]))
        (and continue-saving? (inner #t can-save-file? name format)))
    
      (define/augment (on-save-file name format)
        (when (and (all-string-snips)
                   (eq? format 'same)
                   (eq? 'standard (get-file-format))
                   (or (not (preferences:get 'framework:verify-change-format))
                       (gui-utils:get-choice
                        (string-constant save-as-plain-text) 
                        (string-constant yes)
                        (string-constant no)
                        #:dialog-mixin frame:focus-table-mixin)))
          (set-file-format 'text))
        (inner (void) on-save-file name format))
    
      (super-new)))

  (define unix-line-endings-regexp #rx"(^$)|((^|[^\r])\n)")
  (unless (and (regexp-match? unix-line-endings-regexp "")
               (regexp-match? unix-line-endings-regexp "\n")
               (regexp-match? unix-line-endings-regexp "a\n")
               (not (regexp-match? unix-line-endings-regexp "\r\n"))
               (regexp-match? unix-line-endings-regexp "x\ny\r\nz\n")
               (regexp-match? unix-line-endings-regexp "\n\r\n")
               (not (regexp-match? unix-line-endings-regexp "a\r\nb\r\nc\r\n"))
               (regexp-match? unix-line-endings-regexp "a\r\nb\r\nc\n")
               (regexp-match? unix-line-endings-regexp "a\nb\r\nc\r\n"))
    (error 'framework/private/text.rkt "unix-line-endings-regexp test failure"))

  (define crlf-line-endings<%> (interface ((class->interface text%))))

  (define crlf-line-endings-mixin
    (mixin ((class->interface text%)) (crlf-line-endings<%>)
      (inherit get-filename use-file-text-mode)
      (define/augment (after-load-file success?)
        (when success?
          (cond
            [(preferences:get 'framework:always-use-platform-specific-linefeed-convention)
             (use-file-text-mode #t)]
            [else
             (define unix-endings?
               (with-handlers ((exn:fail:filesystem? (λ (x) #t)))
                 (call-with-input-file (get-filename)
                   (λ (port)
                     (regexp-match? unix-line-endings-regexp port)))))
             (use-file-text-mode
              (and (eq? (system-type) 'windows) 
                   (not unix-endings?)))]))
        (inner (void) after-load-file success?))

      (super-new)
    
      ;; for empty files we want to use LF mode so
      ;; set it this way until a file is loaded in the editor
      (when (eq? (system-type) 'windows)
        (unless (preferences:get 'framework:always-use-platform-specific-linefeed-convention)
          (use-file-text-mode #f)))))

  (define file<%>
    (interface (editor:file<%> basic<%>)
      get-read-write?
      while-unlocked))

  (define file-mixin
    (mixin (editor:file<%> basic<%>) (file<%>)
      (inherit get-filename)
      (define read-write? #t)
      (define/public (get-read-write?) read-write?)
      (define/private (check-lock)
        (define filename (get-filename))
        (define can-edit?
          (if (and filename
                   (file-exists? filename))
              (and (member 'write 
                           (with-handlers ([exn:fail:filesystem? (λ (x) '())])
                             (file-or-directory-permissions filename)))
                   #t)
              #t))
        (set! read-write? can-edit?))
    
      (define/public (while-unlocked t)
        (define unlocked? 'unint)
        (dynamic-wind
         (λ () 
           (set! unlocked? read-write?)
           (set! read-write? #t))
         (λ () (t))
         (λ () (set! read-write? unlocked?))))
    
      (define/augment (can-insert? x y)
        (and read-write? (inner #t can-insert? x y)))
      (define/augment (can-delete? x y) 
        (and read-write? (inner #t can-delete? x y)))
    
      (define/augment (after-save-file success)
        (when success
          (check-lock))
        (inner (void) after-save-file success))
    
      (define/augment (after-load-file sucessful?)
        (when sucessful?
          (check-lock))
        (inner (void) after-load-file sucessful?))
      (super-new)))



  (define all-string-snips<%>
    (interface ()
      all-string-snips?))
  
  (define all-string-snips-mixin
    (mixin ((class->interface text%)) (all-string-snips<%>)
      (inherit find-first-snip find-snip)
    
      (define/private (all-string-snips?/slow)
        (let loop ([s (find-first-snip)])
          (cond
            [(not s) #t]
            [(is-a? s string-snip%) (loop (send s next))]
            [else #f])))

      (define/augment (after-insert start len)
        (inner (void) after-insert start len)
        (define end (+ start len))
        (when (equal? all-string-snips-state #t)
          (define init-i (box 0))
          (define init-s (find-snip start 'after-or-none init-i))
          (let loop ([s init-s]
                     [i (unbox init-i)])
            (cond
              [(not s) (void)]
              [(not (< i end)) (void)]
              [(is-a? s string-snip%)
               (define size (send s get-count))
               (loop (send s next) (+ i size))]
              [else
               (set! all-string-snips-state #f)]))))
    
      (define/augment (on-delete start end)
        (inner (void) on-delete start end)
        (when (equal? all-string-snips-state #f)
          (let loop ([s (find-snip start 'after-or-none)]
                     [i start])
            (cond
              [(not s) (void)]
              [(not (< i end)) (void)]
              [(is-a? s string-snip%)
               (define size (send s get-count))
               (loop (send s next) (+ i size))]
              [else (set! all-string-snips-state 'dont-know)]))))

    
      ;; (or/c #t #f 'dont-know)
      (define all-string-snips-state #t)
      (define/public (all-string-snips?)
        (cond
          [(boolean? all-string-snips-state)
           all-string-snips-state]
          [else
           (define all-string-snips? (all-string-snips?/slow))
           (set! all-string-snips-state all-string-snips?)
           all-string-snips?]))
    
      (super-new)))

  (define overwrite-disable<%> (interface ()))
  (define overwrite-disable-mixin
    (mixin ((class->interface text%)) (overwrite-disable<%>)
      (inherit set-overwrite-mode)
    
      ;; private field held onto by the object
      ;; because of the weak callback below
      (define (overwrite-changed-callback p v)
        (unless v
          (set-overwrite-mode #f)))

      (preferences:add-callback
       'framework:overwrite-mode-keybindings
       overwrite-changed-callback
       #t)
     
      (super-new))))
