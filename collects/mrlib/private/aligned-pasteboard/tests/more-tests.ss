;; some more advanced aligned-pasteboard tests take from the test-case-boxes

(require
 (lib "class.ss")
 (lib "mred.ss" "mred")
 (lib "etc.ss")
 (lib "list.ss")
 (lib "match.ss")
 "../aligned-editor-container.ss"
 "../interface.ss"
 "../alignment.ss"
 "../snip-lib.ss"
 "../pasteboard-lib.ss")

(define (make-aligned-pasteboard type)
  (class* pasteboard% (aligned-pasteboard<%>)
    (inherit resize move-to find-first-snip refresh-delayed?
             begin-edit-sequence end-edit-sequence)
    
    (field
     [needs-realign? false]
     [ignore-resizing? false]
     [alloted-width 0]
     [alloted-height 0]
     [aligned-min-width 0]
     [aligned-min-height 0]
     [aligned-rects empty]
     [in-edit-sequence? false])
    
    ;;;;;;;;;;
    ;; accessors
    
    ;; get-aligned-min-width (-> number?)
    ;; the aligned-min-width of the pasteboard
    (define/public (get-aligned-min-width) aligned-min-width)
    
    ;; get-aligned-min-height (-> number?)
    ;; the aligned-min-height of the pasteboard
    (define/public (get-aligned-min-height) aligned-min-height)
    
    ;;;;;;;;;;
    ;; size calculations
    
    ;; set-algined-min-sizes (-> void?)
    ;; set the aligned min width and height of the pasteboard based on it's children snips
    (define/public (set-aligned-min-sizes)
      (set! ignore-resizing? true)
      (set!-values (aligned-min-width aligned-min-height)
                   (get-aligned-min-sizes type (find-first-snip)))
      (set! ignore-resizing? false))
    
    ;;;;;;;;;;
    ;; realignment
    
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
    
    ;;move/resize (snip-pos? rect? . -> . void?)
    ;;moves and resizes the snips with in pasteboard
    (define move/resize
      (match-lambda*
        [(snip ($ rect
                  ($ dim x width stretchable-width?)
                  ($ dim y height stretchable-height?)))
         (move-to snip x y)
         ;; let's try this way to do it.
         (when (and (is-a? snip stretchable-snip<%>)
                    (or stretchable-width? stretchable-height?))
           (send snip stretch-to width height))
         ;; one way to do it?
         ;(when (or stretchable-height? stretchable-width? (is-a? snip aligned-pasteboard-parent<%>))
         ;  (resize snip width height))
         ;; another way to do it?
         ;(resize snip width height)
         ;(when (is-a? snip editor-snip%)
         ;  (send snip set-min-width 'none)
         ;  (send (send snip get-editor) set-min-width 'none))
         ]))
    
    ;;;;;;;;;;
    ;; event-handling
    
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
    
    ;; do I need to override release-snip or does after-delete handle this for me?
    ;(rename [super-release-snip release-snip])
    ;(define/override (release-snip snip)
    ;  (super-release-snip snip))
    
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
      (super-resized snip redraw-now?)
      (unless ignore-resizing?
        (when (or redraw-now?
                  (and (not (refresh-delayed?))
                       (needs-resize? snip)))
          (calc/realign))))
    
    ;; after-edit-sequence (-> void?)
    ;; called after an edit-sequence ends
    (rename [super-after-edit-sequence after-edit-sequence])
    (define/override (after-edit-sequence)
      (set! in-edit-sequence? false)
      (when needs-realign? (calc/realign)))
    
    (rename [super-on-edit-sequence on-edit-sequence])
    (define/override (on-edit-sequence)
      (set! in-edit-sequence? true)
      (super-on-edit-sequence))
    
    ;; calc/realign (-> void?)
    ;; sends a message to the pasteboard to recalculate min sizes and realign
    (define/private (calc/realign)
      (if in-edit-sequence?
          (set! needs-realign? true)
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
    
    (super-new)))

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

(define vertical-pasteboard% (make-aligned-pasteboard 'vertical))
(define horizontal-pasteboard% (make-aligned-pasteboard 'horizontal))

;; a text-case snip
(define test-case-box%
  (class aligned-editor-snip%
    
    ;; these edit-sequences are looping
    (define/public (hide-entries)
      (send* editor
        ;(begin-edit-sequence)
        (release-snip call-line)
        (release-snip exp-line)
        (release-snip act-line)
        ;(end-edit-sequence)
        ))
    
    ;; these edit-sequences are looping
    (define/public (show-entries)
      (send* editor
        ;(begin-edit-sequence)
        (insert call-line false)
        (insert exp-line false)
        (insert act-line false)
        ;(end-edit-sequence)
        ))
    
    (field
     [editor (new vertical-pasteboard%)]
     [turn-button (new image-snip%)]
     [comment (new text%)]
     [result (new image-snip%)]
     [call (new text%)]
     [expected (new text%)]
     [actual (new text%)]
     [top-line (make-top-line turn-button comment result)]
     [call-line (make-line "Call" call)]
     [exp-line (make-line "Expected" expected)]
     [act-line (make-line "Actual" actual)])
    
    (send editor insert top-line)
    (show-entries)
    
    (super-new
     (editor editor)
     (stretchable-height false)
     (stretchable-width false))))

;; the top line of the test-case
(define (make-top-line turn-snip comment result-snip)
  (let ([pb (new horizontal-pasteboard%)])
    (send* pb
      (insert turn-snip false)
      (insert (text-field comment) false)
      (insert result-snip false))
    (new aligned-editor-snip%
         (stretchable-height false)
         (editor pb))))

;; a line labeled with the given string and containing a given text
(define (make-line str text)
  (let ([pb (new horizontal-pasteboard%)])
    (send* pb
      (insert (make-object string-snip% str) false)
      (insert (text-field text) false))
    (new aligned-editor-snip% (editor pb))))

;; a text field fit to be in a test-case (no borders or margins etc.)
;;STATUS: this should really return an aligned-snip<%> not an editor-snip% of fixed size.
(define (text-field text)
  (new editor-snip% (editor text)))

(define top
  (case 3
    [(1) (cons vertical-pasteboard% aligned-editor-canvas%)]
    [(2) (cons text% editor-canvas%)]
    [(3) (cons pasteboard% editor-canvas%)]))

(define f (new frame% (label "test") (width 200) (height 200)))
(define e (new (car top)))
(define c (new (cdr top) (editor e) (parent f)))
(define t (new test-case-box%))
(send e insert t)
(send f show #t)
;(send t hide-entries)
;(send t show-entries)