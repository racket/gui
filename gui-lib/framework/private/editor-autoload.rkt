#lang racket/unit

(require "sig.rkt"
         "editor-sig.rkt"
         "../preferences.rkt"
         "focus-table.rkt"
         string-constants
         mred/mred-sig
         racket/class
         racket/match
         file/sha1)
  
(import mred^
        [prefix frame: framework:frame^]
        [prefix editor: editor-misc^])
(export editor-autoload^)
(init-depend mred^)

(define autoload<%>
  (interface (editor:basic<%>)))

(define-local-member-name autoload-file-changed autoload-do-revert)

;; open-dialogs : hash[tlw -o> (cons/c dialog boolean)]
;; the dialogs that are currently being shown, paired with
;; a boolean that indicates if the initial dialog has a checkbox
(define open-dialogs (make-hash))

;; pending-editors : hash[tlw -o> (listof editor<%>)]
;; editors where we are waiting for a reply from the user
(define pending-editors (make-hash))

;; invariant:
;;  (hash-ref pending-editors tlw '()) = (cons ed eds)
;;    ⟺
;;  (hash-ref open-dialogs tlw #f) ≠ #f


;; only called if we have to ask the user about what to do,
;; so we know that the `ask preference is set or window was dirty
(define (handle-autoload-file-changed&need-dialog editor)
  (define tlw (send editor get-top-level-window))
  (define already-pending-editors (hash-ref pending-editors tlw '())) ;; when tlw=#f, this will be '()
  (unless (member editor already-pending-editors)
    (define all-pending-editors (cons editor already-pending-editors))
    (when tlw (hash-set! pending-editors tlw all-pending-editors))
    (cond
      [(and (null? (cdr all-pending-editors))
            (send (car all-pending-editors) is-modified?))
       ;; first one => need to open the dialog, and it is dirty
       (define dlg
         (message-box/custom
          (string-constant warning)
          (get-autoload-warning-message all-pending-editors)
          (string-constant revert)
          (string-constant ignore)
          #f
          tlw
          '(caution no-default)
          2
          #:return-the-dialog? #t
          #:dialog-mixin frame:focus-table-mixin))
       (when tlw
         (hash-set! open-dialogs tlw (cons dlg #f))
         (hash-set! pending-editors tlw (list editor)))
       (define revert? (case (send dlg show-and-return-results)
                         [(1) #t]
                         [(2) #f]))
       (handle-dialog-closed tlw editor revert?)]
      [(null? (cdr all-pending-editors))
       ;; first one => need to open the dialog, but it isn't dirty
       (define dlg
         (message+check-box/custom
          (string-constant warning)
          (get-autoload-warning-message all-pending-editors)
          (string-constant dont-ask-again-always-current)
          (string-constant revert)
          (string-constant ignore)
          #f
          tlw
          '(caution no-default)
          2
          #:return-the-dialog? #t
          #:dialog-mixin frame:focus-table-mixin))
       (when tlw
         (hash-set! open-dialogs tlw (cons dlg #t))
         (hash-set! pending-editors tlw (list editor)))
       (define-values (button checked?) (send dlg show-and-return-results))
       (when checked?
         ;; setting the preference will start the monitor
         ;; if `answer` is #t
         (preferences:set 'framework:autoload revert?))
       (define revert? (case button
                         [(1) #t]
                         [(2) #f]))
       (handle-dialog-closed tlw editor revert?)]
      [else
       ;; dialog is already open, see if we need to tweak the text
       ;; here we know that tlw ≠ #f
       (match-define (cons dlg has-check?) (hash-ref open-dialogs tlw))
       (hash-set! pending-editors tlw all-pending-editors)
       (define any-existing-modified?
         (for/or ([ed (in-list already-pending-editors)])
           (send ed is-modified?)))
       (define this-one-modified? (send editor is-modified?))
       (when has-check? ;; here we know that at least one is clean
         (when this-one-modified? ;; here we know that we are dirty
           (unless any-existing-modified? ;; here we know we are the first dirty one
             ;; which means we need to update the label of the checkbox
             (send dlg set-check-label
                   (string-constant
                    dont-ask-again-always-current/clean-buffer)))))
       (define new-dialog-message
         (get-autoload-warning-message all-pending-editors))
       (send dlg set-message new-dialog-message)])))

(define (get-autoload-warning-message currently-pending-editors)
  (define number-of-pending-editors (length currently-pending-editors))
  (define all-dirty?
    (for/and ([ed (in-list currently-pending-editors)])
      (send ed is-modified?)))
  (define any-dirty?
    (for/or ([ed (in-list currently-pending-editors)])
      (send ed is-modified?)))
  (cond
    [(not any-dirty?)
     ;; none are dirty
     (cond
       [(= 1 number-of-pending-editors)
        (format
         (string-constant autoload-file-changed-on-disk/with-name)
         (send (car currently-pending-editors) get-filename))]
       [else
        (apply
         string-append
         (string-constant autoload-files-changed-on-disk/with-name)
         (for/list ([f (in-list currently-pending-editors)])
           (format "\n  ~a" (send f get-filename))))])]
    [all-dirty?
     (cond
       [(= 1 number-of-pending-editors)
        (format
         (string-constant autoload-file-changed-on-disk-editor-dirty/with-name)
         (send (car currently-pending-editors) get-filename))]
       [else
        (apply
         string-append
         (string-constant autoload-files-changed-on-disk-editor-dirty/with-name)
         (for/list ([f (in-list currently-pending-editors)])
           (format "\n  ~a" (send f get-filename))))])]
    [else
     ;; mixture of clean and dirty .. in this case we know there isn't just one file
     (apply
      string-append
      (string-constant autoload-files-changed-on-disk-editor-dirty&clean/with-name)
      (for/list ([f (in-list currently-pending-editors)])
        (format "\n  ~a~a"
                (send f get-filename)
                (if (send f is-modified?)
                    " ◇"
                    ""))))]))

(define (handle-dialog-closed tlw editor revert?)
  (cond
    [tlw
     (define all-pending-editors (hash-ref pending-editors tlw))
     (hash-remove! open-dialogs tlw)
     (hash-remove! pending-editors tlw)
     (when revert?
       (for ([ed (in-list all-pending-editors)])
         (send ed autoload-do-revert)))]
    [else
     (when revert?
       (send editor autoload-do-revert))]))

(define autoload-mixin
  (mixin (editor:basic<%>) (autoload<%>)
    (inherit get-filename load-file
             begin-edit-sequence end-edit-sequence
             is-modified?)

    (define/augment (on-load-file path format)
      (on-load/save-file path)
      (inner (void) on-load-file path format))

    (define/augment (after-load-file success?)
      (after-load/save-file)
      (inner (void) after-load-file success?))

    (define/augment (on-save-file path format)
      (on-load/save-file path)
      (inner (void) on-load-file path format))

    (define/augment (after-save-file success?)
      (after-load/save-file)
      (inner (void) after-load-file success?))

    (define on/after-communication-channel #f)
    
    (define/private (on-load/save-file path)
      (unless (editor:doing-autosave?)
        (define evt
          (and (preferences:get 'framework:autoload)
               (filesystem-change-evt path (λ () #f))))
        (when evt (monitored-file-sha1-will-change this))
        (set! on/after-communication-channel
              (vector path evt))))

    (define/private (after-load/save-file)
      (unless (editor:doing-autosave?)
        (match on/after-communication-channel
          [(vector path (? filesystem-change-evt? evt))
           (monitor-a-file this path evt (current-eventspace))]
          [(vector path #f)
           ;; event creation failed or the preference is turned off
           (void)])
        (set! on/after-communication-channel #f)))
    
    (define/override (set-filename filename [temporary? #f])
      (unless on/after-communication-channel
        ;; if the filename changes but we aren't doing a
        ;; save or a load, then, well, just give up
        ;; if the file is saved, later on, we'll start
        ;; the monitoring process again
        (un-monitor-a-file this))
      (super set-filename filename temporary?))

    (define/override (update-sha1? path)
      (cond
        [(editor:doing-autosave?) #f]
        [else (super update-sha1? path)]))
    
    (define/augment (on-close)
      (un-monitor-a-file this)
      (inner (void) on-close))

    ;; intentionally not a method; ensures
    ;; the callback stays registered with the
    ;; preferences system as as long as `this`
    ;; is held onto
    (define pref-callback
      (λ (p v)
        (case v
          [(#f) (un-monitor-a-file this)]
          [(#t ask)
           (define path (get-filename))
           (when path
             (monitor-a-file this
                             path
                             (filesystem-change-evt path (λ () #f))
                             (current-eventspace)))])))
    (preferences:add-callback 'framework:autoload pref-callback #t)
    
    (define/public (autoload-file-changed)
      (define pref (preferences:get 'framework:autoload))
      (cond
        [(or (is-modified?) (equal? 'ask pref))
         (handle-autoload-file-changed&need-dialog this)]
        [pref
         (autoload-do-revert)]
        [else
         (un-monitor-a-file this)]))

    (define/public (autoload-do-revert)
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
            (load-file filename 'guess #f)
            #f))
        (unless failed?
          (when (is-a? this text%)
            (send this set-position start start)))
        (end-edit-sequence)))

    (super-new)
    (inherit enable-sha1)
    (enable-sha1)))

(define (monitor-a-file txt path evt eventspace)
  (define the-sha1 (send txt get-file-sha1))
  (channel-put monitor-a-file-chan (vector txt path the-sha1 evt eventspace)))
(define monitor-a-file-chan (make-channel))

(define (monitored-file-sha1-will-change txt)
  (channel-put sha1-will-change-chan txt))
(define sha1-will-change-chan (make-channel))

(define (un-monitor-a-file txt)
  (channel-put unmonitor-a-file-chan txt))
(define unmonitor-a-file-chan (make-channel))

(void
 (thread
  (λ ()
    ;; path: path-string?
    ;; evt: filesystem-change-evt?
    ;; the-sha1: (or/c 'unknown-check 'unknown-no-check bytes?)
    ;;   -- the two symbols mean we don't know the sha1 should be
    ;;      'unknown-check means the evt has woken up and so when
    ;;      we get the sha1 we should check the file and
    ;;      'unknown-no-check means that the evt hasn't yet woken up
    ;;      bytes? is the sha1
    ;; eventspace: eventspace?
    (struct monitored (path evt the-sha1 eventspace) #:transparent)

    ;; state : hash[txt -o> monitored?]
    (let loop ([state (hash)])
      (apply
       sync
       (handle-evt
        unmonitor-a-file-chan
        (λ (txt)
          (define old (hash-ref state txt #f))
          (when old (filesystem-change-evt-cancel (monitored-evt old)))
          (loop (hash-remove state txt))))

       (handle-evt
        monitor-a-file-chan
        (λ (txt+path+eventspace)
          (match-define (vector txt path the-sha1 evt eventspace)
            txt+path+eventspace)
          (define old (hash-ref state txt #f))
          (when old (filesystem-change-evt-cancel (monitored-evt old)))
          (loop (hash-set state
                          txt
                          (monitored path evt
                                     the-sha1
                                     eventspace)))))

       (handle-evt
        sha1-will-change-chan
        (λ (txt)
          (match (hash-ref state txt #f)
            [(? monitored? old)
             (loop (hash-set state txt
                             (struct-copy monitored old
                                          [the-sha1 'unknown-no-check])))]
            [#f (loop state)])))
            
       (for/list ([(txt a-monitored) (in-hash state)])
         (match-define (monitored path evt the-sha1 eventspace)
           a-monitored)
         (handle-evt
          evt
          (λ (_)
            ;; create the new evt before we look at the file's
            ;; sha1 to avoid any moment where the file might
            ;; be unmonitored.
            (define new-evt (filesystem-change-evt path (λ () #f)))
               
            (define state-of-file
              (with-handlers ([exn:fail:filesystem? (λ (x) 'failed)])
                (cond
                  [(symbol? the-sha1) 'need-to-wait]
                  [(equal? the-sha1 (call-with-input-file path sha1-bytes))
                   'unchanged]
                  [else 'changed])))
            (match state-of-file
              ['need-to-wait
               (cond
                 [new-evt
                  (loop (hash-set state txt
                                  (struct-copy monitored a-monitored
                                               [the-sha1 'unknown-check]
                                               [evt new-evt])))]
                 [else
                  ;; we failed to create an evt; give up on
                  ;; monitoring this file (can we do better?)
                  (loop (hash-remove state txt))])]
              ['unchanged
               ;; this appears to be a spurious wakeup
               ;; use the new evt to wait again
               (cond
                 [new-evt
                  (loop (hash-set state txt
                                  (struct-copy monitored a-monitored
                                               [evt new-evt])))]
                 [else
                  (loop (hash-remove state txt))])]
              ['failed
               ;; an exception was raised above so we don't notify,
               ;; but also stop monitoring the file
               (when new-evt (filesystem-change-evt-cancel new-evt))
               (loop (hash-remove state txt))]
              ['changed
               ;; here we know that the content has a new hash
               ;; so it seems safe to safe to reload the buffer.
               (parameterize ([current-eventspace eventspace])
                 (queue-callback
                  (λ ()
                    (send txt autoload-file-changed))))
               ;; we also reenable the monitor here
               (loop (hash-set state txt
                               (struct-copy monitored a-monitored
                                            [evt new-evt])))])))))))))
