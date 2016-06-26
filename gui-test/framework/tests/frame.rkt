#lang racket/base

(require "private/here-util.rkt"
         "private/gui.rkt"
         rackunit
         racket/class
         racket/gui/base
         framework)

(define (test-creation name create)
  (check-true
   (let ()
     (parameterize ([current-eventspace (make-eventspace)])
       (define c (make-channel))
       (define f #f)
       (queue-callback
        (λ ()
          (set! f (create))
          (send f show #t)
          (channel-put c (send f get-label))))
       (define frame-label (channel-get c))
       (wait-for-frame frame-label)
       (queue-callback (λ () (send f close)))
       #t))
   (format "create ~a" name)))

(define (creation-tests)
  (test-creation
   'basic%-creation
   (λ () (new frame:basic% [label "test"])))
  (test-creation
   'basic-mixin-creation
   (λ () (new (frame:focus-table-mixin (frame:basic-mixin frame%)) [label "test"])))
  (test-creation
   'info-mixin-creation
   (λ () (new (frame:info-mixin frame:basic%)
              [label "test"])))
  (test-creation
   'info%-creation
   (λ () (new frame:info% [label "test"])))
  (test-creation
   'text-info-mixin-creation
   (λ () (new (frame:text-info-mixin frame:info%)
              [label "test"])))
  (test-creation
   'text-info%-creation
   (λ () (new frame:text-info% [label "test"])))
  (test-creation
   'pasteboard-info-mixin-creation
   (λ () (new (frame:pasteboard-info-mixin frame:info%)
              [label "test"])))
  (test-creation
   'pasteboard-info%-creation
   (λ () (new frame:pasteboard-info% [label "test"])))
  (test-creation
   'standard-menus%-creation
   (λ () (new frame:standard-menus% [label "test"])))
  (test-creation
   'standard-menus-mixin
   (λ () (new (frame:standard-menus-mixin frame:basic%) [label "test"])))
  
  (test-creation
   'text%-creation
   (λ () (new frame:text%)))
  (test-creation
   'text-mixin-creation
   (λ () (new (frame:text-mixin frame:editor%))))
  (test-creation
   'text-mixin-creation
   (λ () (new (frame:text-mixin frame:editor%))))
  
  (test-creation
   'searchable%-creation
   (λ () (new frame:searchable%)))
  (test-creation
   'searchable-mixin
   (λ () (new (frame:searchable-mixin frame:text%))))
  
  (test-creation
   'pasteboard-mixin-creation
   (λ () (new (frame:pasteboard-mixin frame:editor%))))
  (test-creation
   'pasteboard-mixin-creation
   (λ () (new (frame:pasteboard-mixin (frame:editor-mixin frame:standard-menus%)))))
  (test-creation
   'pasteboard%-creation
   (λ () (new frame:pasteboard%))))

(define (test-open name cls)
  (define test-file-contents "test")
  (check-equal?
   (let ()
     (define tmp-file-name "framework-tmp")
     (define tmp-file (collection-file-path tmp-file-name "framework" "tests"))
     (call-with-output-file tmp-file
       (λ (port) (display test-file-contents port))
       #:exists 'truncate)
     (preferences:set 'framework:file-dialogs 'common)
     (parameterize ([current-eventspace (make-eventspace)])
       (define c (make-channel))
       (queue-callback
        (λ ()
          (define frame (new cls))
          (send frame show #t)
          (channel-put c (send frame get-label))))
       (define frame-name (channel-get c))
       (wait-for-frame frame-name)
       (test:menu-select "File" "Open…")
       (wait-for-frame "Open File")
       (queue-callback
        (λ ()
          (send (find-labelled-window "Filename:") focus)
          (channel-put c (void))))
       (channel-get c)
       (case (system-type)
         [(macos macosx) (test:keystroke #\a '(meta))]
         [(unix) (test:keystroke #\a '(meta))]
         [(windows) (test:keystroke #\a '(control))])
       (for-each test:keystroke (string->list (path->string tmp-file)))
       (test:keystroke #\return)
       (wait-for-frame tmp-file-name)
       (queue-callback
        (λ ()
          (channel-put c (send (send (test:get-active-top-level-window) get-editor) get-text))))
       (define editor-contents (channel-get c))
       (test:close-top-level-window (test:get-active-top-level-window))
       (wait-for-frame frame-name)
       (queue-callback
        (λ ()
          (send (test:get-active-top-level-window) close)))
       editor-contents))
   test-file-contents
   name))

(define (open-tests)
  (test-open "frame:searchable open" frame:searchable%)
  (test-open "frame:text open" frame:text%))

(let ([pref-ht (make-hash)])
  (parameterize ([test:use-focus-table #t]
                 [preferences:low-level-get-preference
                  (λ (sym [fail (λ () #f)])
                    (hash-ref pref-ht sym fail))]
                 [preferences:low-level-put-preferences
                  (λ (syms vals)
                    (for ([sym (in-list syms)]
                          [val (in-list vals)])
                      (hash-set! pref-ht sym val)))])
    (define dummy (make-object frame:basic% "dummy to keep from quitting"))
    (send dummy show #t)
    (creation-tests)
    (open-tests)
    (send dummy show #f)))

