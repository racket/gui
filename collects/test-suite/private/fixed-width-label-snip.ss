(module fixed-width-label-snip mzscheme
  
  (require
   (lib "class.ss")
   (lib "list.ss")
   (lib "debug.ss" "mike-lib")
   (lib "mred.ss" "mred"))
  
  (provide fixed-width-label-snip)
  
  (define (fixed-width-label-snip labels)
    (define label-snip%
      (class snip%
        (inherit set-snipclass)
        (init-field
         label
         (left-margin 5)
         (right-margin 5)
         (top-margin 5)
         (bottom-margin 5))
        
        (define (get-string-size dc string)
          (let-values ([(width height baseline vspace)
                        (send dc get-text-extent string)])
            (cons width height)))
        
        (define (get-max-string-size dc strings)
          (foldl
           (lambda (str sizes)
             (let ([these-sizes (get-string-size dc str)])
               (cons (max (car these-sizes)
                          (car sizes))
                     (max (cdr these-sizes)
                          (cdr sizes)))))
           (get-string-size dc (car strings))
           (cdr strings)))
        
        (define/override (get-extent dc x y w h descent space lspace rspace)
          (let ([maxes (get-max-string-size dc labels)])
            (set-box! w (+ left-margin (car maxes) right-margin))
            (set-box! h (+ top-margin (cdr maxes) bottom-margin))))
        
        (rename [super-draw draw])
        (define/override (draw dc x y left top right bottom dx dy draw-caret)
          (super-draw dc x y left top right bottom dx dy draw-caret)
          (let ([max-sizes (get-max-string-size dc labels)]
                [sizes (get-string-size dc label)])
            (send dc draw-text label
                  (+ left-margin x (- (car max-sizes) (car sizes)))
                  (+ y top-margin))))
        
        (rename [super-copy copy])
        (define/override (copy)
          (super-copy))
        
        (define/override (resize w h) #f)
        
        ;; write ((is-a?/c editor-stream-out%) . -> . void?)
        ;; write the snip out to the stream
        (define/override (write f)
          (send f put label))
        
        (super-new)
        (set-snipclass (new label-snip-class%))))
    
    (define label-snip-class%
      (class snip-class%
        ;; read ((is-a?/c editor-stream-in%) . -> . snip%)
        ;; read a snip from the stream
        (define/override (read f)
          (new label-snip% (label (send f get-string))))
        (super-new)))
    
    (let ([lsc (new label-snip-class%)])
      (send lsc set-classname "...")
      (send lsc set-version 1)
      (send (get-the-snip-class-list) add lsc))
    
    label-snip%)
  
  (define (test)
    (define mylabels (list "Call" "Expected" "Actual"))
    (define label% (fixed-width-label-snip mylabels))
    (define align? #t)
    (define f (new frame% (label "test") (width 175) (height 175)))
    (define e (new pasteboard%))
    (define c (new editor-canvas% (editor e) (parent f)))
    (for-each
     (lambda (s)
       (send e insert (new label% (label s))))
     '("Expected"))
    (send f show #t))
  )