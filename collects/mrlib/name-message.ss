
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
      (override on-event on-paint)
      
      (define/public (on-choose-directory dir)
        (void))
      
      (define paths #f)
      
      ;; label : string
      (define label (string-constant untitled))
      
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
      
      (define full-name-window #f)
      
      (define mouse-grabbed? #f)
      (define (on-event evt)
        (let* ([needs-update? #f]
               [do-update
                (lambda ()
                  (when needs-update?
                    (on-paint)))])
          
          (let-values ([(max-x max-y) (get-size)])
            (let ([inside? (and (<= 0 (send evt get-x) max-x)
                                (<= 0 (send evt get-y) max-y))])
              (unless (eq? inside? inverted?)
                (set! inverted? inside?)
                (set! needs-update? #t))))
          
          (cond
            [(and paths (not (null? paths))) 
             (cond
               [(send evt button-down?)
                (let-values ([(width height) (get-client-size)])
                  
                  (set! inverted? #t)
                  (set! needs-update? #t)
                  (let ([menu (make-object popup-menu% #f
                                (lambda x
                                  (set! inverted? #f)
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
                (cond
                  [inverted?
                   (set! inverted? #f)
                   (set! needs-update? #t)
                   (do-update)
                   (message-box 
                    (string-constant drscheme)
                    (string-constant no-full-name-since-not-saved)
                    (get-top-level-window))]
                  [else
                   (do-update)])]
               [(send evt button-down? 'left)
                (set! mouse-grabbed? #t)
                (set! inverted? #t)
                (set! needs-update? #t)
                (do-update)]
               [else 
                (do-update)])])))
      
      (inherit get-parent)
      (define (update-min-sizes)
        (let-values ([(w h) (calc-button-min-sizes (get-dc) label)])
          (min-width w)
          (min-height h)
          (send (get-parent) reflow-container)))
      
      (define inverted? #f)
      
      (define (on-paint)
        (let ([dc (get-dc)])
          (let-values ([(w h) (get-client-size)])
            (when (and (> w 5) (> h 5))
              (draw-button-label dc label w h inverted? mouse-grabbed?)))))
      
      (super-instantiate ())
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
  
  (define triangle-width 10)
  (define triangle-height 7)
  (define triangle-space 2)
  
  (define (calc-button-min-sizes dc label)
    (send dc set-font button-label-font)
    (let-values ([(w h a d) (send dc get-text-extent label button-label-font)])
      (let ([ans-w
             (+ button-label-inset
                1 ;; becuase "(define ...)" has the wrong size under windows
                (max 0 (inexact->exact (ceiling w)))
                triangle-space
                triangle-width
                button-label-inset)]
            [ans-h
             (+ button-label-inset
                (max 0 
                     (inexact->exact (ceiling h))
                     triangle-height)
                button-label-inset)])
        (values ans-w ans-h))))
  
  (define (offset-color color offset-one)
    (make-object color%
      (offset-one (send color red))
      (offset-one (send color green))
      (offset-one (send color blue))))
  
  (define light-button-color (offset-color (get-panel-background)
                                           (lambda (v) (floor (+ v (/ (- 255 v) 2))))))
  (define dark-button-color (offset-color (get-panel-background)
                                          (lambda (v) (floor (- v (/ v 2))))))
  
  (define triangle
    (list (make-object point% 0 0)
          (make-object point% triangle-width 0)
          (make-object point% (/ triangle-width 2) triangle-height)))
  
  (define (draw-button-label dc label w h mouse-over? grabbed?)
    (define inverted? grabbed?)
    
    (send dc set-text-foreground black-color)
    (send dc set-text-background (get-panel-background))
    (send dc set-pen (send the-pen-list find-or-create-pen
                           (get-panel-background) 1 'solid))
    (send dc set-brush (send the-brush-list find-or-create-brush
                             (get-panel-background) 'solid))
    
    (send dc draw-rectangle 0 0 w h)
    
    (send dc set-pen (send the-pen-list find-or-create-pen
                           "BLACK" 1 'solid))
    (send dc set-brush
          (send the-brush-list find-or-create-brush
                (if inverted? dark-button-color light-button-color) 'solid))
    
    #;
    (let ([border
           (lambda (d)
             (send dc draw-rectangle
                   d d
                   (- w drop-shadow-size)
                   (- h drop-shadow-size)))])
      (if inverted?
          (let loop ([n 0])
            (cond
              [(= n drop-shadow-size) (void)]
              [else
               (border n)
               (loop (+ n 1))]))
          (let loop ([n drop-shadow-size])
            (cond
              [(zero? n) (void)]
              [else
               (border (- n 1))
               (loop (- n 1))]))))
    
    (send dc draw-polygon triangle 
          (- w triangle-width)
          (- (/ h 2) (/ triangle-height 2)))
    
    (when label
      (send dc set-font button-label-font)

      ;; 1 is for the outer drop shadow box
      (send dc draw-text label button-label-inset button-label-inset))))
