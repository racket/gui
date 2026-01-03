#lang racket/base
(require "test-suite-utils.rkt"
         rackunit
         framework
         racket/gui/base
         racket/class)

(define (test-creation the-frame% the-editor% name)
  (check-not-exn
   (λ ()
     (define c (make-channel))
     (queue-callback
      (λ ()
        (define f
          (new (class the-frame%
                 (define/override (get-editor%) the-editor%)
                 (super-new))))
        (preferences:set 'framework:exit-when-no-frames #f)
        (send f show #t)
        (channel-put c (send f get-label))))
     (define frame-label (channel-get c))
     (define seconds 2)
     (define resolution 1/100)
     (let loop ([n (* seconds resolution)])
       (cond
         [(zero? n)
          (error 'test-creation "never saw the frame\n  test: ~a" name)]
         [(let ([f (get-top-level-focus-window)])
            (and f (equal? (send f get-label) frame-label)))
          (void)]
         [else
          (sleep resolution)
          (loop (- n 1))]))
     (queue-callback
      (λ ()
        (send (get-top-level-focus-window) close)
        (channel-put c (void))))
     (channel-get c))))

(define (run-tests)
  (test-creation frame:editor%
                 (editor:basic-mixin pasteboard%)
                 'editor:basic-mixin-creation)
  (test-creation frame:editor%
                 pasteboard:basic%
                 'pasteboard:basic-creation)

  (test-creation frame:editor%
                 (editor:file-mixin pasteboard:keymap%)
                 'editor:file-mixin-creation)
  (test-creation frame:editor%
                 pasteboard:file%
                 'pasteboard:file-creation)

  (test-creation frame:editor%
                 (editor:backup-autosave-mixin pasteboard:file%)
                 'editor:backup-autosave-mixin-creation)
  (test-creation frame:editor%
                 pasteboard:backup-autosave%
                 'pasteboard:backup-autosave-creation)

  (test-creation frame:pasteboard%
                 (editor:info-mixin pasteboard:backup-autosave%)
                 'editor:info-mixin-creation)
  (test-creation frame:pasteboard%
                 pasteboard:info%
                 'pasteboard:info-creation))

(void
 (yield
  (thread
   run-tests)))

;; this seems to be needed so that the autosave timer's
;; weak boxes empty, so the autosave timer turns itself
;; off, so that racket exits
(void
 (thread
  (λ ()
    (for ([i (in-range 10)])
      (collect-garbage)
      (sleep 1)))))
