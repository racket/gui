
(module name-message mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "class.ss")
           (lib "file.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "contract.ss"))
  
  (provide/contract
   (draw-button-label
    ((is-a?/c dc<%>) (union false/c string?) (>/c 5) (>/c 5) boolean? boolean?
     . -> .
     void?))

   (calc-button-min-sizes
    (->*
     ((is-a?/c dc<%>) string?)
     (number? number?))))
  
  (provide name-message%)
  
  (define name-message%
    (class canvas% 
      (inherit popup-menu get-dc get-size get-client-size min-width min-height
               stretchable-width stretchable-height
               get-top-level-window)
      
      (define paths #f)
      
      ;; label : string
      (define label (string-constant untitled))
      
      (define full-name-window #f)
      
      (define mouse-grabbed? #f)
      (define mouse-over? #f)

      (define/public (on-choose-directory dir)
        (void))
            
      ;; set-message : boolean (union #f path string) -> void
      ;; if file-name? is #t, path-name should be a path (or #f)
      ;; if file-name? is #f, path-name should be a string (or #f)
      (define/public (set-message file-name? path-name)
        (set! paths (if (and file-name? 
                             path-name 
                             (file-exists? path-name))
                        (map path->string (explode-path (normalize-path path-name)))
                        #f))
        (let ([new-label (cond
                           [(and paths (not (null? paths)))
                            (car (last-pair paths))]
                           [path-name path-name]
                           [else (string-constant untitled)])])
          (unless (equal? label new-label)
            (set! label new-label)
            (update-min-sizes)
            (on-paint))))
      
      (define/override (on-event evt)
        (let* ([needs-update? #f]
               [do-update
                (lambda ()
                  (when needs-update?
                    (on-paint)))])
          
          (let-values ([(max-x max-y) (get-size)])
            (let ([inside? (and (not (send evt leaving?))
                                (<= 0 (send evt get-x) max-x)
                                (<= 0 (send evt get-y) max-y))])
              (unless (eq? inside? mouse-over?)
                (set! mouse-over? inside?)
                (set! needs-update? #t))))
          
          (cond
            [(and paths (not (null? paths))) 
             (cond
               [(send evt button-down?)
                (let-values ([(width height) (get-size)])
                  (set! mouse-over? #t)
                  (set! needs-update? #t)
                  (set! mouse-grabbed? #t)
                  (let ([menu (make-object popup-menu% #f
                                (lambda x
                                  (set! mouse-over? #f)
                                  (set! mouse-grabbed? #f)
                                  (on-paint)))])
                    (let loop ([paths (cdr (reverse paths))])
                      (cond
                        [(null? paths) (void)]
                        [else 
                         (make-object menu-item% (car paths) menu
                           (lambda (evt item)
                             (on-choose-directory (apply build-path (reverse paths)))))
                         (loop (cdr paths))]))
                    (do-update)
                    (popup-menu menu
                                0
                                height)))]
               [else (do-update)])]
            [else
             (cond
               [(send evt button-up? 'left)
                (set! mouse-grabbed? #f)
                (set! needs-update? #t)
                (cond
                  [mouse-over?
                   (set! mouse-over? #f)
                   (do-update)
                   (message-box 
                    (string-constant drscheme)
                    (string-constant no-full-name-since-not-saved)
                    (get-top-level-window))]
                  [else
                   (do-update)])]
               [(send evt button-down? 'left)
                (set! mouse-grabbed? #t)
                (set! mouse-over? #t)
                (set! needs-update? #t)
                (do-update)]
               [else 
                (do-update)])])))
      
      (inherit get-parent)
      (define/private (update-min-sizes)
        (let-values ([(w h) (calc-button-min-sizes (get-dc) label)])
          (min-width w)
          (min-height h)
          (send (get-parent) reflow-container)))
      
      (define/override (on-paint)
        (let ([dc (get-dc)])
          (let-values ([(w h) (get-client-size)])
            (when (and (> w 5) (> h 5))
              (draw-button-label dc label w h mouse-over? mouse-grabbed?)))))
      
      (super-new [style '(transparent)])
      (update-min-sizes)
      (stretchable-width #f)
      (stretchable-height #f)))
  
  (define button-label-font
    (case (system-type)
      [(windows)
       (send the-font-list find-or-create-font 8 'decorative 'normal 'normal #f)]
      [(macosx)
       (send the-font-list find-or-create-font 11 'system 'normal 'bold #f)]
      [else
       (send the-font-list find-or-create-font 10 'decorative 'normal 'normal #f)]))
  
  (define button-label-inset 1)
  (define black-color (make-object color% "BLACK"))
  
  (define unclicked-triangle (make-object bitmap% (build-path (collection-path "icons") "turn-down.png") 'unknown/mask))
  (define clicked-triangle (make-object bitmap% (build-path (collection-path "icons") "turn-down-click.png") 'unknown/mask))
  (define triangle-width (send clicked-triangle get-width))
  (define triangle-height (send clicked-triangle get-height))
  
  (define triangle-space 2)
  (define circle-spacer 3)
  
  (define (offset-color color offset-one)
    (make-object color%
      (offset-one (send color red))
      (offset-one (send color green))
      (offset-one (send color blue))))
  
  (define normal-background "lightgray")
  (define mouse-over/grabbed-background "darkgray")
  
  (define border-color "lightgray")
  (define mouse-over-border-color "darkgray")
  
  (define normal-text-color (send the-color-database find-color "black"))
  (define mouse-over-text-color (send the-color-database find-color "black"))
  
  (define (calc-button-min-sizes dc label)
    (send dc set-font button-label-font)
    (let-values ([(w h a d) (send dc get-text-extent label button-label-font)])
      (let ([ans-w
             (+ circle-spacer
                button-label-inset
                1 ;; becuase "(define ...)" has the wrong size under windows
                (max 0 (inexact->exact (ceiling w)))
                triangle-space
                triangle-width
                button-label-inset
                circle-spacer)]
            [ans-h
             (+ button-label-inset
                (max 0 
                     (+ 2 (inexact->exact (ceiling h)))
                     (+ 2 triangle-height))
                button-label-inset)])
        (values ans-w ans-h))))
  
  
  (define (draw-button-label dc label w h mouse-over? grabbed?)
    (cond
      [mouse-over?
       (send dc set-pen (send the-pen-list find-or-create-pen mouse-over-border-color 1 'solid))
       (send dc set-brush (send the-brush-list find-or-create-brush mouse-over/grabbed-background 'solid))
       (send dc set-text-foreground mouse-over-text-color)]
      [else
       (send dc set-pen (send the-pen-list find-or-create-pen border-color 1 'solid))
       (send dc set-brush (send the-brush-list find-or-create-brush normal-background 'solid))
       (send dc set-text-foreground black-color)])
    
    (send dc draw-ellipse 0 0 h h)
    (send dc draw-ellipse (- w h) 0 h h)
    
    (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
    (send dc draw-rectangle (quotient h 2) 0 (- w h) h)
    
    (cond
      [mouse-over?
       (send dc set-pen (send the-pen-list find-or-create-pen mouse-over-border-color 1 'solid))]
      [else
       (send dc set-pen (send the-pen-list find-or-create-pen border-color 1 'solid))])
    (send dc draw-line (quotient h 2) 0 (- w (quotient h 2)) 0)
    (send dc draw-line (quotient h 2) (- h 1) (- w (quotient h 2)) (- h 1))
    
    (when label
      (send dc set-font button-label-font)
      (let-values ([(tw th _1 _2) (send dc get-text-extent label)])
        (send dc draw-text label
              (+ circle-spacer button-label-inset)
              (- (/ h 2) (/ th 2)))))
    
    (let ([bm (if grabbed? clicked-triangle unclicked-triangle)])
      (send dc draw-bitmap
            bm
            (- w triangle-width circle-spacer)
            (- (/ h 2) (/ triangle-height 2))
            'solid
            black-color
            (send bm get-loaded-mask)))
    (void)))
