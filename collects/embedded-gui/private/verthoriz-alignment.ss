(module verthoriz-alignment mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "match.ss")
   (prefix a: "alignment.ss")
   
   "snip-lib.ss"
   "interface.ss"
   "alignment-helpers.ss")
   
  (provide
   horizontal-alignment%
   vertical-alignment%)
  
  (define (vert/horiz-alignment type)
    (class* object% (alignment<%> alignment-parent<%>)
      
      (init-field parent [show? true])
      
      (field
       [pasteboard (send parent get-pasteboard)]
       [children empty]
       [min-width 0]
       [min-height 0])
      
      ;;;;;;;;;;
      ;; alignment<%>
      
      #;(-> void?)
      ;; Tells the alignment that its sizes should be calculated
      (define/public (set-min-sizes)
        (when show?
          (for-each
           (lambda (child)
             (send child set-min-sizes))
           children)
          (let-values ([(x-accum y-accum)
                        (if (symbol=? type 'vertical)
                            (values vacuous-max +)
                            (values + vacuous-max))])
            (set! min-width
                  (apply x-accum
                         (map (lambda (c) (send c get-min-width))
                              children)))
            (set! min-height
                  (apply y-accum
                         (map (lambda (c) (send c get-min-height))
                              children))))))
      
      #;(nonnegative? nonnegative? nonnegative? nonnegative? . -> . void?)
      ;; Tells the alignment to align its children on the pasteboard in the given rectangle
      (define/public (align x-offset y-offset width height)
          
          (define move/resize
            (match-lambda*
              [(child ($ a:rect ($ a:dim x w _) ($ a:dim y h _)))
               (send child align (+ x x-offset) (+ y y-offset) w h)]))
          
          (when (and (is-shown?)
                     (not (empty? children))
                     (not (zero? width))    ; this should be handled by align later
                     (not (zero? height)))  ; this one too
            (for-each move/resize
                      children
                      (a:align type width height
                               (map build-rect children)))))
      
      #;(-> nonnegative?)
      ;; The minimum width this alignment must be
      (define/public (get-min-width)
        (if (is-shown?) min-width 0))
      
      #;(-> nonnegative?)
      ;; The minimum height this alignment must be
      (define/public (get-min-height)
        (if (is-shown?) min-height 0))
      
      #;(-> boolean?)
      ;; True if the alignment can be stretched in the x dimension
      (define/public (stretchable-width?) true)
      
      #;(-> boolean?)
      ;; True if the alignment can be stretched in the y dimension
      (define/public (stretchable-height?) true)
      
      #;(boolean? . -> . void)
      ;; Tells the alignment to show or hide its children
      (define/public (show/hide bool)
        (when show? (show/hide-children bool)))
      
      #;(boolean? . -> . void)
      ;; Tells the alignment that its show state is the given value
      ;; and it should show or hide its children accordingly.
      (define/public (show bool)
        (set! show? bool)
        (when (send parent is-shown?)
          (show/hide-children bool)))
      
      ;;;;;;;;;;
      ;; alignment-parent<%>
      
      #;(-> (is-a?/c pasteboard%))
      ;; The pasteboard that this alignment is being displayed to
      (define/public (get-pasteboard) pasteboard)
      
      #;((is-a?/c alignment<%>) . -> . void?)
      ;; Add the given alignment as a child
      (define/public (add-child child)
        (set! children (append children (list child))))
      
      #;(-> boolean?)
      ;; True if the alignment is being shown (accounting for its parent being shown)
      (define/public (is-shown?)
        (and show? (send parent is-shown?)))
      
      ;;;;;;;;;;
      ;; helpers
      
      #;(boolean? . -> . void?)
      ;; Shows or hides the children
      (define/private (show/hide-children bool)
        (send pasteboard lock-alignment true)
        (for-each (lambda (c) (send c show/hide bool)) children)
        (send pasteboard lock-alignment false))
      
      (super-new)
      (send parent add-child this)))
  
  (define vertical-alignment% (vert/horiz-alignment 'vertical))
  (define horizontal-alignment% (vert/horiz-alignment 'horizontal))
  
  #;((is-a?/c alignment%) . -> . rect?)
  ;; makes a new default rect out of a snip
  (define (build-rect item)
    (a:make-rect
     (a:make-dim 0 (send item get-min-width) (send item stretchable-width?))
     (a:make-dim 0 (send item get-min-height) (send item stretchable-height?))))
  )
