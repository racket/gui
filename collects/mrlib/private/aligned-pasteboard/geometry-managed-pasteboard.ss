(module geometry-managed-pasteboard mzscheme
  
  (require
   (lib "class.ss")
   (lib "contracts.ss")
   (lib "list.ss")
   (lib "etc.ss")
   (lib "match.ss")
   "interface.ss"
   "alignment.ss"
   "snip-lib.ss")
  
  (provide/contract
   (geometry-managed-pasteboard-mixin (class? (symbols 'vertical 'horizontal) . -> . class?)))
  
  ;;  mixin to add geometry management to pasteboard with the give type of alignement
  (define (geometry-managed-pasteboard-mixin super% type)
    (class* super% (aligned-pasteboard<%>)
      (inherit resize move-to find-first-snip
               begin-edit-sequence end-edit-sequence)
      
      (field
       [needs-realign? false]
       [ignore-resizing? false]
       [alloted-width 0]
       [alloted-height 0]
       [aligned-min-width 0]
       [aligned-min-height 0]
       [aligned-rects empty])
      
      ;; get-aligned-min-width (-> number?)
      ;; the aligned-min-width of the pasteboard
      (define/public (get-aligned-min-width)
        aligned-min-width)
      
      ;; get-aligned-min-height (-> number?)
      ;; the aligned-min-height of the pasteboard
      (define/public (get-aligned-min-height)
        aligned-min-height)
      
      ;; realign (case-> (-> void?) (positive? positive? . -> . void?))
      ;; called by the parent to realign the pasteboard's children
      (define/public realign
        (case-lambda
          [(width height)
           (set! alloted-width width)
           (set! alloted-height height)
           (realign)]
          [()
           (when (and (positive? alloted-width)
                      (positive? alloted-height))
             (set! needs-realign? false)
             (realign-to-alloted))]))
      
      ;; realign-to-alloted (-> void?)
      ;; realign the snips to fill the alloted width and height
      (define/private (realign-to-alloted)
        (let ([first-snip (find-first-snip)])
          (set! aligned-rects
                (align type alloted-width alloted-height
                       (map-snip build-rect first-snip)))
          (begin-edit-sequence)
          (set! ignore-resizing? true)
          (for-each-snip move/resize first-snip aligned-rects)
          (set! ignore-resizing? false)
          (end-edit-sequence)))
      
      ;; set-algined-min-sizes (-> void?)
      ;; set the aligned min width and height of the pasteboard based on it's children snips
      (define/public (set-aligned-min-sizes)
        (set! ignore-resizing? true)
        (set!-values (aligned-min-width aligned-min-height)
                     (get-aligned-min-sizes type (find-first-snip)))
        (set! ignore-resizing? false))
      
      ;;move/resize (snip-pos? rect? . -> . void?)
      ;;moves and resizes the snips with in pasteboard
      (define move/resize
        (match-lambda*
          [(snip ($ rect
                    ($ dim x width stretchable-width?)
                    ($ dim y height stretchable-height?)))
           (move-to snip x y)
           (when (or stretchable-height? stretchable-width?)
             (resize snip width height))]))
      
      (super-instantiate ())
      ))
  
  ;; build-rect ((is-a?/c snip%) . -> . rect?)
  ;; makes a new default rect out of a snip
  (define (build-rect snip)
    (make-rect
     (make-dim 0 (snip-min-width snip) (stretchable-width? snip))
     (make-dim 0 (snip-min-height snip) (stretchable-height? snip))))
  
  ;; get-aligned-min-sizes (((symbols 'horizontal vertical) (is-a?/c snip%)) . ->* . (number? number?))
  ;; calculate the aligned min sizes for the pasteboard containing the given snips
  (define (get-aligned-min-sizes type init-snip)
    (let-values ([(x-func y-func)
                  (if (symbol=? type 'horizontal)
                      (values + max)
                      (values max +))])
      (let loop ([snip init-snip]
                 [width 0]
                 [height 0])
        (cond
          [(boolean? snip)
           (values width height)]
          [else
           (when (is-a? snip aligned-pasteboard-parent<%>)
             (send snip set-aligned-min-sizes))
           (loop (send snip next)
                 (x-func (snip-min-width snip) width)
                 (y-func (snip-min-height snip) height))]))))
  )