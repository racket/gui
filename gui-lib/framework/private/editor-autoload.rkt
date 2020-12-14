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

(define-local-member-name autoload-file-changed)

(define autoload<%>
  (interface (editor:basic<%>)))

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
      (cond
        [(ask-can-revert)
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
           (end-edit-sequence))]
        [else
         (un-monitor-a-file this)]))

    (define/private (ask-can-revert)
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

         (case button
           [(1) #t]
           [(2) #f])]
        [else
         (match (preferences:get 'framework:autoload)
           [`ask
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
            (when checked?
              ;; setting the preference will start the monitor
              ;; if `answer` is #t
              (preferences:set 'framework:autoload answer))
            answer]
           [#t #t]
           [#f #f])]))

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
