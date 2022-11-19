#lang racket
(require rackunit framework string-constants
         racket/gui/base
         "test-suite-utils.rkt")

(define-syntax (in-scratch-directory stx)
  (syntax-case stx ()
    [(_ e1 e2 ...)
     #`(in-scratch-directory/proc
        #,(syntax/loc stx (λ () e1 e2 ...)))]))

(define (in-scratch-directory/proc body)
  (define d
    (make-temporary-file
     "framework-autosave-test~a"
     #:copy-from 'directory))
  (dynamic-wind
   void
   (λ ()
     (parameterize ([current-directory d])
       (body)))
   (λ () (delete-directory/files d))))

(define (wait-for-recover)
  (let loop ()
    (define chan (make-channel))
    (queue-callback
     (λ ()
       (define f (get-top-level-focus-window))
       (channel-put
        chan
        (and f
             (equal? (string-constant recover-autosave-files-frame-title)
                     (send f get-label)))))
     #f)
    (or (channel-get chan)
        (loop))))

(define (wait-for-recover-gone f1)
  (let loop ()
    (define keep-going-chan (make-channel))
    (queue-callback
     (λ ()
       (define f2 (get-top-level-focus-window))
       (cond
         [(equal? f1 f2)
          (channel-put keep-going-chan #t)]
         [(not f2) (channel-put keep-going-chan #f)]
         [else
          ;; this is some debugging code; we don't expect any
          ;; new windows to show up, so printout what it is
          (pretty-write
           (let loop ([w f2])
             (cond
               [(is-a? w area-container<%>)
                (for/list ([w (in-list (send w get-children))])
                  (loop w))]
               [(is-a? w message%) (vector "message%" (send w get-label))]
               [(is-a? w button%) (vector "button%" (send w get-label))]
               [(and (is-a? w editor-canvas%) (is-a? (send w get-editor) text%))
                (vector "tet%" (send (send w get-editor) get-text))]
               [else w]))
           (current-error-port))
          (error 'wait-for-recover-gone "a frame that's not the recovery frame")]))
     #f)
    (when (channel-get keep-going-chan)
      (loop))))

(define (fetch-content fn)
  (define sp (open-output-string))
  (call-with-input-file fn (λ (p) (copy-port p sp)))
  (get-output-string sp))

;; test that the window opens
(let ()
  (define t
    (thread
     (λ ()
       (define f (wait-for-recover))
       (test:button-push (string-constant autosave-done))
       (wait-for-recover-gone f))))
  (in-scratch-directory
   (call-with-output-file "x.rkt" void)
   (autosave:restore-autosave-files/gui
    (list (list #f (build-path (current-directory) "x.rkt"))))
   (yield t)
   (void)))

;; test that the window opens and no files change when we just click "done"
(let ()
  (define t
    (thread
     (λ ()
       (define f (wait-for-recover))
       (test:button-push (string-constant autosave-done))
       (wait-for-recover-gone f))))
  (in-scratch-directory
   (call-with-output-file "x.rkt" (λ (p) (displayln "x.rkt" p)))
   (call-with-output-file "y.rkt" (λ (p) (displayln "y.rkt" p)))
   (autosave:restore-autosave-files/gui
    (list (list (build-path (current-directory) "x.rkt")
                (build-path (current-directory) "y.rkt"))))
   (yield t)
   (check-equal? (fetch-content "x.rkt") "x.rkt\n")
   (check-equal? (fetch-content "y.rkt") "y.rkt\n")
   (void)))


;; test that the window opens with a variety of items
(let ()
  (define t
    (thread
     (λ ()
       (define f (wait-for-recover))
       (test:button-push (string-constant autosave-done))
       (wait-for-recover-gone f))))
  (in-scratch-directory
   (call-with-output-file "x.rkt" void)
   (call-with-output-file "y.rkt" void)
   (call-with-output-file "z.rkt" void)
   (call-with-output-file "a.rkt" void)
   (call-with-output-file "b.rkt" void)
   (call-with-output-file "c.rkt" void)
   (autosave:restore-autosave-files/gui
    (list (list #f (build-path (current-directory) "x.rkt"))
          (list (build-path (current-directory) "z.rkt") (build-path (current-directory) "c.rkt"))
          (list #f (build-path (current-directory) "y.rkt"))
          (list (build-path (current-directory) "a.rkt") (build-path (current-directory) "b.rkt"))))
   (yield t)
   (void)))

;; test that we can click on the details button
(let ()
  (define t
    (thread
     (λ ()
       (define f (wait-for-recover))
       (test:button-push (string-constant autosave-details))
       (test:button-push (string-constant autosave-done))
       (wait-for-recover-gone f))))
  (in-scratch-directory
   (call-with-output-file "x.rkt" void)
   (call-with-output-file "y.rkt" void)
   (autosave:restore-autosave-files/gui
    (list (list #f (build-path (current-directory) "x.rkt"))
          (list #f (build-path (current-directory) "y.rkt"))))
   (yield t)
   (void)))

;; test that we can restore a file
(let ()
  (define t
    (thread
     (λ ()
       (define f (wait-for-recover))
       (test:button-push (string-constant autosave-recover))
       (test:button-push (string-constant autosave-done))
       (wait-for-recover-gone f))))
  (in-scratch-directory
   (call-with-output-file "x.rkt" (λ (p) (displayln "x.rkt" p)))
   (call-with-output-file "y.rkt" (λ (p) (displayln "y.rkt" p)))
   (autosave:restore-autosave-files/gui
    (list (list (build-path (current-directory) "x.rkt")
                (build-path (current-directory) "y.rkt"))))
   (yield t)
   (check-false (file-exists? "y.rkt"))
   (check-equal? (fetch-content "x.rkt") "y.rkt\n")
   ))
