(module button-snip mzscheme
  
  (require
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "etc.ss"))
  
  (provide
   text-button-snip%
   button-snip%
   toggle-button-snip%)
  
  ;; a snip of a button that can be pushed to invoke a given callback
  (define button-snip%
    (class image-snip%
      (inherit load-file)
      (init images)
      (init-field callback)
      (field
       [got-click? false]
       [inside? false]
       [image (car images)]
       [depressed (cdr images)])
      
      ;; (string? . -> . void?)
      ;; set the image to be displayed on the button when it is not clicked
      (define/public (set-images i)
        (set! image (car i))
        (set! depressed (cdr i))
        (load-file image))
      
      ;; Should I be calling super-on-event?
      (rename [super-on-event on-event])
      (define/override (on-event dc x y editorx editory event)
        (case (send event get-event-type)
          [(left-down)
           (set! got-click? true)
           (set! inside? true)
           (load-file depressed)]
          [(left-up)
           (load-file image)
           (when (and got-click? inside?)
             (callback this event))
           (set! got-click? false)
           (set! inside? false)]
          [(enter)
           (set! inside? true)
           (when got-click?
             (load-file depressed))]
          [(leave)
           (set! inside? false)
           (when got-click?
             (load-file image))]
          [else (void)]))
      
      (super-new)
      (load-file image)))
  
  ;; a textual button of the same type
  (define text-button-snip%
    (class string-snip%
      (init label)
      (init-field callback)
      (field
       [got-click? false]
       [inside? false])
      
      (rename [super-on-event on-event])
      (define/override (on-event dc x y editorx editory event)
        (case (send event get-event-type)
          [(left-down)
           (set! got-click? true)
           (set! inside? true)]
          [(left-up)
           (when (and got-click? inside?)
             (callback this event))
           (set! got-click? false)
           (set! inside? false)]
          [(enter)
           (set! inside? true)]
          [(leave)
           (set! inside? false)]
          [else (void)]))
      
      (super-make-object label)))
  
  ;; a toggle button that displays different images
  (define toggle-button-snip%
    (class button-snip%
      (inherit set-images)
      (init-field images1 images2 callback1 callback2 (state 1))
      (super-new
       (images images1)
       (callback
        (lambda (b e)
          (if (= state 1)
              (begin
                (set-images images2)
                (set! state 2)
                (callback1 b e))
              (begin
                (set-images images1)
                (set! state 1)
                (callback2 b e))))))))
  
  ;;;;;;;;;;
  ;; tests
  
  (require
   (lib "locked-pasteboard.ss" "mrlib" "private" "aligned-pasteboard")
   (lib "click-forwarding-editor.ss" "mrlib"))
  
  (define (test)
    (define f (new frame% (label "test") (width 200) (height 200)))
    (define e (new (locked-pasteboard-mixin
                    (click-forwarding-editor-mixin pasteboard%))))
    (define c (new editor-canvas% (editor e) (parent f)))
    (define b (new button-snip%
                   (images (cons (build-path (collection-path "icons") "turn-up.gif")
                                 (build-path (collection-path "icons") "turn-up-click.gif")))
                   (callback
                    (lambda (b e)
                      (message-box "Test" "Horray!")))))
    (send e insert b)
    (send f show #t))
  
  (define (test2)
    (define f (new frame% (label "test") (width 200) (height 200)))
    (define e (new (locked-pasteboard-mixin
                    (click-forwarding-editor-mixin pasteboard%))))
    (define c (new editor-canvas% (editor e) (parent f)))
    (define t (new text%))
    (define es (new editor-snip% (editor t)))
    (define b (new toggle-button-snip%
                   (images1 (cons (build-path (collection-path "icons") "turn-up.gif")
                                  (build-path (collection-path "icons") "turn-up-click.gif")))
                   (images2 (cons (build-path (collection-path "icons") "turn-down.gif")
                                  (build-path (collection-path "icons") "turn-down-click.gif")))
                   (callback1
                    (lambda (b e)
                      (send* t (erase) (insert "Up"))))
                   (callback2
                    (lambda (b e)
                      (send* t (erase) (insert "Down"))))))
    (send e insert es 50 0)
    (send e insert b)
    (send f show #t))
)