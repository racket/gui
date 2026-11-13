#lang racket/base
(require "private/util.rkt"
         "private/gui.rkt"
         rackunit
         racket/class
         racket/gui/base
         framework
         (only-in framework/private/group
                  pay-attention-to-current-eventspace-has-standard-menus?))

(define windows-menu-prefix 
  (let ([basics (list "Bring Frame to Front…" "Most Recent Window"
                      #f)])
    (if (eq? (system-type) 'macosx)
        (list* "Minimize" "Zoom" basics)
        basics)))

(with-private-prefs
 (parameterize ([test:use-focus-table #t]
                [pay-attention-to-current-eventspace-has-standard-menus? #f])

   (define-syntax car*
     (syntax-rules ()
       [(car* x-expr)
        (let ([x x-expr])
          (if (pair? x)
              (car x)
              (begin
                (eprintf "car* called with ~s\n" 'x-expr)
                (car x))))]))

   (define the-first-frame #f)

   ;; The window manager activates a newly shown frame some time after
   ;; `show` returns, and activating a frame moves it to the front of the
   ;; frame group, so these frames record their activations and the tests
   ;; wait for them before looking at the group
   (define activated (make-hash))
   (define activation-frame%
     (class frame:basic%
       (inherit get-label)
       (define/override (on-activate on?)
         (super on-activate on?)
         (when on? (hash-set! activated (get-label) #t)))
       (super-new)))
   (define (wait-for-activation label)
     (wait-for/here (λ () (hash-ref activated label #f))))

   (yield
    (thread
     (λ ()
       (queue-callback
        (λ ()
          (set! the-first-frame (make-object frame:basic% "first"))
          (send the-first-frame show #t)))
       (preferences:set 'framework:verify-exit #t)
       (wait-for-frame "first")
       (queue-callback
        (λ ()
          (send (test:get-active-top-level-window) close)))
       (wait-for-frame "Warning")
       (test:button-push "Cancel")
       (wait-for-frame "first"))))
   (check-equal? (map (lambda (x) (send x get-label))
                      (send (group:get-the-frame-group) get-frames))
                 '("first"))

   ;; after the first test, we should have one frame
   ;; that will always be in the group.

   (check-equal?
    (let ()
      (send (make-object frame:basic% "test") show #t)
      (define ans (map (lambda (x) (send x get-label))
                       (send (group:get-the-frame-group) get-frames)))
      (send (test:get-active-top-level-window) close)
      ans)
    (list "test" "first"))


   (begin
     (yield
      (thread
       (λ ()
         (hash-clear! activated)
         (queue-callback
          (λ () (send (make-object activation-frame% "test1") show #t)))
         (wait-for-activation "test1")
         (queue-callback
          (λ () (send (make-object activation-frame% "test2") show #t)))
         (wait-for-activation "test2"))))
     (check-equal?
      (let ([frames (send (group:get-the-frame-group) get-frames)])
        (for-each (lambda (x)
                    (unless (equal? (send x get-label) "first")
                      (send x close)))
                  frames)
        (map (lambda (x) (send x get-label)) frames))
      (list "test2" "test1" "first")))

   (begin
     (yield
      (thread
       (λ ()
         (hash-clear! activated)
         (queue-callback
          (λ ()
            (send (make-object activation-frame% "test1") show #t)))
         (wait-for-activation "test1")
         (queue-callback
          (λ ()
            (send (make-object activation-frame% "test2") show #t)))
         (wait-for-activation "test2"))))
     (send (test:get-active-top-level-window) close)
     (check-equal?
      (let ([frames (send (group:get-the-frame-group) get-frames)])
        (for-each (lambda (x)
                    (unless (equal? (send x get-label) "first")
                      (send x close)))
                  frames)
        (map (lambda (x) (send x get-label)) frames))
      (list "test1" "first")))


   (when (eq? (system-type) 'macosx)

     (check-equal?
      (begin
        (send (make-object frame:basic% "test") show #t)
        (let ([mb (send (test:get-active-top-level-window) get-menu-bar)])
          (send mb on-demand)
          (define labels
            (for/list ([x (send (car* (send mb get-items)) get-items)])
              (and (is-a? x labelled-menu-item<%>) (send x get-label))))
          (send (test:get-active-top-level-window) close)
          labels))
      (append windows-menu-prefix (list "first" "test")))

     (check-equal?
      (let ()
        (define frame1 (make-object frame:basic% "test"))
        (define frame2 (make-object frame:basic% "test-not-shown"))
        (send frame1 show #t)
        (define mb (send (test:get-active-top-level-window) get-menu-bar))
        (send mb on-demand)
        (define items
          (for/list ([x (send (car* (send mb get-items)) get-items)])
            (and (is-a? x labelled-menu-item<%>) (send x get-label))))
        (send (test:get-active-top-level-window) close)
        items)
      (append windows-menu-prefix (list "first" "test")))

     (define (get-label-and-close-non-first)
       (define frames (send (group:get-the-frame-group) get-frames))
       (define mb (send (car* frames) get-menu-bar))
       (send mb on-demand)
       (define ans
         (for/list ([x (in-list (send (car* (send mb get-items))
                                      get-items))])
           (and (is-a? x labelled-menu-item<%>) (send x get-label))))
       (for ([x (in-list frames)])
         (unless (equal? (send x get-label) "first")
           (send x close)))
       ans)
    
     (check-equal?
      (let ()
        (define aaa-frame (make-object frame:basic% "aaa"))
        (send aaa-frame show #t)
        (define bbb-frame (make-object frame:basic% "bbb"))
        (send bbb-frame show #t)
        (get-label-and-close-non-first))
      (append windows-menu-prefix (list "aaa" "bbb" "first")))

     (check-equal?
      (let ()
        (define bbb-frame (make-object frame:basic% "bbb"))
        (send bbb-frame show #t)
        (define aaa-frame (make-object frame:basic% "aaa"))
        (send aaa-frame show #t)
        (get-label-and-close-non-first))
      (append windows-menu-prefix (list "aaa" "bbb" "first"))))

   ;; close that original frame so the test suite can exit if run from `racket`
   (send the-first-frame show #f)))
