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
    (class* object% (alignment<%>)
      
      (init-field (parent false))
      
      (field
       [pasteboard false]
       [children empty]
       [min-width 0]
       [min-height 0]
       [show? true])
      
      ;; need base class for this method
      (define (show/hide-child child show?)
        (if (is-a? child alignment<%>)
            (send child show show?)
            (if show?
                (send pasteboard insert child)
                (send pasteboard release-snip child))))
      
      ;; STATUS: This function (through lock-alignment false) invokes a call
      ;; to realign of the pasteboard even when this alignement has show? = false
      ;; so the call is not needed.
      (define/public (add child)
        (set! children (append children (list child)))
        (send pasteboard lock-alignment true)
        (cond
          [(is-a? child snip%)
           (when show?
             (send pasteboard insert child false))]
          [(is-a? child alignment<%>)
           (send child set-pasteboard pasteboard)])
        (send pasteboard lock-alignment false))
      
      (define/public (get-min-width)
        (if show? min-width 0))
      (define/public (get-min-height)
        (if show? min-height 0))
      (define/public (set-pasteboard pb) (set! pasteboard pb))
      (define/public (stretchable-width?) true)
      (define/public (stretchable-height?) true)
      (define/public (show bool)
        (unless (boolean=? bool show?)
          (set! show? bool)
          (send pasteboard lock-alignment true)
          (for-each (lambda (c)
                      (show/hide-child c bool))
                    children)
          (send pasteboard lock-alignment false)))
      
      (define/public (align x-offset y-offset width height)
          
          (define move/resize
            (match-lambda*
              [(child ($ a:rect
                         ($ a:dim x w stretchable-width?)
                         ($ a:dim y h stretchable-height?)))
               (let ([global-x (+ x x-offset)]
                     [global-y (+ y y-offset)])
                 (cond
                   [(is-a? child snip%)
                    (send pasteboard move-to child global-x global-y)
                    (when (or stretchable-width? stretchable-height?)
                      (send child stretch w h))]
                   [(is-a? child alignment<%>)
                    (send child align global-x global-y w h)]))]))
          
          (when (and show? (not (empty? children)))
            (for-each move/resize
                      children
                      (a:align type width height
                               (map build-rect children)))))
      
      (define/public (set-min-sizes)
        (when show?
          (for-each
           (lambda (child)
             (when (is-a? child alignment<%>)
               (send child set-min-sizes)))
           children)
          (let-values ([(x-accum y-accum)
                        (if (symbol=? type 'vertical)
                            (values vacuous-max +)
                            (values + vacuous-max))])
            (set! min-width
                  (apply x-accum
                         (map child-width
                              children)))
            (set! min-height
                  (apply y-accum
                         (map child-height
                              children))))))
      
      (super-new)
      (when parent (send parent add this))))
  
  (define vertical-alignment% (vert/horiz-alignment 'vertical))
  (define horizontal-alignment% (vert/horiz-alignment 'horizontal))
  
  ;; build-rect ((is-a?/c snip%) . -> . rect?)
  ;; makes a new default rect out of a snip
  (define (build-rect item)
    (cond
      [(is-a? item snip%)
       (a:make-rect
        (a:make-dim 0 (snip-min-width item) (stretchable-width? item))
        (a:make-dim 0 (snip-min-height item) (stretchable-height? item)))]
      [(is-a? item alignment<%>)
       (a:make-rect
        (a:make-dim 0 (send item get-min-width) (send item stretchable-width?))
        (a:make-dim 0 (send item get-min-height) (send item stretchable-height?)))]))
  )
