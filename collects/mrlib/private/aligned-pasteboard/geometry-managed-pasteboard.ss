(module geometry-managed-pasteboard mzscheme
  
  (require
   (lib "class.ss")
   (lib "contract.ss")
   (lib "list.ss")
   (lib "etc.ss")
   (lib "match.ss")
   (lib "mred.ss" "mred")
   "interface.ss"
   "alignment.ss"
   "snip-lib.ss"
   "pasteboard-lib.ss")
  
  (provide/contract (make-aligned-pasteboard ((symbols 'vertical 'horizontal) . -> . class?)))
  
  ;;  mixin to add geometry management to pasteboard with the give type of alignement
  (define (make-aligned-pasteboard type)
    (class* pasteboard% (aligned-pasteboard<%>)
      (inherit resize move-to find-first-snip refresh-delayed?
               begin-edit-sequence end-edit-sequence)
      
      (field
       [needs-realign? false]
       [my-edit-sequence? false]
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
          (set! my-edit-sequence? true)
          (set! ignore-resizing? true)
          (for-each-snip move/resize first-snip aligned-rects)
          (set! ignore-resizing? false)
          (set! my-edit-sequence? false)
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
           (when (or stretchable-height? stretchable-width? (is-a? snip aligned-pasteboard-parent<%>))
             (resize snip width height))
           ;(resize snip width height)
           ;(when (is-a? snip editor-snip%)
           ;  (send snip set-min-width 'none)
           ;  (send (send snip get-editor) set-min-width 'none))
           ]))
      
      ;; after-insert ((is-a?/c snip%) (is-a?/c snip%) number? number? . -> . void?)
      ;; called after a snip is inserted to the pasteboard
      (rename [super-after-insert after-insert])
      (define/override (after-insert snip before x y)
        (calc/realign)
        (super-after-insert snip before x y))
      
      ;; after-delete ((is-a?/c snip%) . -> . void?)
      ;; called after a snip is deleted from the pasteboard%
      (rename [super-after-delete after-delete])
      (define/override (after-delete snip)
        (calc/realign)
        (super-after-delete snip))
      
      ;; after-reorder ((is-a?/c snip%) (is-a?/c snip%) boolean? . -> . void?)
      ;; called after a snip is moved in the front to back snip order
      (rename [super-after-reorder after-reorder])
      (define/override (after-reorder snip to-snip before?)
        (realign)
        (super-after-reorder snip to-snip before?))
      
      ;; resized ((is-a?/c snip%) . -> . void?)
      ;; called when a snip inside the editor is resized
      (rename [super-resized resized])
      (define/override (resized snip redraw-now?)
        (unless ignore-resizing?
          (super-resized snip redraw-now?)
          (when (or redraw-now?
                    (and (not (refresh-delayed?))
                         (needs-resize? snip)
                         ))
            (calc/realign))))
      
      ;; after-edit-sequence (-> void?)
      ;; called after an edit-sequence ends
      (rename [super-after-edit-sequence after-edit-sequence])
      (define/override (after-edit-sequence)
        (when needs-realign? (calc/realign)))
      
      ;; calc/realign (-> void?)
      ;; sends a message to the pasteboard to recalculate min sizes and realign
      (define/private (calc/realign)
        (if (refresh-delayed?)
            (unless my-edit-sequence? (set! needs-realign? true))
            (let* ([root (pasteboard-root this)]
                   [parent (pasteboard-parent root)])
              (when parent
                (send parent set-aligned-min-sizes)
                (send root realign)))))
      
      ;; needs-resize? ((is-a?/c snip%) . -> . boolean?)
      ;; determines if the snip's size is smaller than it's min size
      (define/private (needs-resize? snip)
        (with-handlers ([exn? (lambda a false)])
        (match-let ([($ rect
                        ($ dim _ alloted-width _)
                        ($ dim _ alloted-height _))
                     (find-rect snip)])
          (if (is-a? snip aligned-snip<%>)
              (or (< alloted-width (send snip get-aligned-min-width))
                  (< alloted-height (send snip get-aligned-min-height)))
              (if (empty? aligned-rects)
                  false
                  
                    (match-let ([($ rect
                                  ($ dim _ actual-width _)
                                  ($ dim _ actual-height _))
                               (build-rect snip)])
                    (not (and (= alloted-width actual-width)
                              (= alloted-height actual-height)))))))))
      
      ;(define/private (needs-resize? snip)
      ;  (cond
      ;    [(is-a? snip aligned-snip<%>)
      ;     (or (< (snip-width snip)
      ;            (send snip get-aligned-min-width))
      ;         (< (snip-height snip)
      ;            (send snip get-aligned-min-height))
      ;         (and (not (send snip stretchable-width))
      ;              (> (snip-width snip)
      ;                 (send snip get-aligned-min-width)))
      ;         (and (not (send snip stretchable-height))
      ;              (> (snip-height snip)
      ;                 (send snip get-aligned-min-height))))]
      ;    [else false]))
      
      ;; find-rect ((is-a?/c snip%) . -> . rect?)
      ;; finds the rect that corresponds to the given snip
      (define/private (find-rect target-snip)
        (letrec ([find-rect-aux
                  (lambda (snip rects)
                    (cond
                      [(or (equal? snip false) (empty? rects))
                       (error 'find-rect "Snip not found")]
                      [else
                       (if (equal? snip target-snip)
                           (car rects)
                           (find-rect-aux (send snip next)
                                          (rest rects)))]))])
          (find-rect-aux (find-first-snip) aligned-rects)))
      
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