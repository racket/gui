;; notes:   When resize of the editor snip is called, the child pasteboard gets sizes for its get-view-size
;;        method set. These values are based on the snips size and it's margin. Since the snips can be
;;        invisable at times (often due to scroll bars) using get-view-size is not sufficient. I have
;;        calculated the view size myself in the snips resize method. It is possible for the margins to
;;        change size after the resize callback is invoked. This would cause inconsistencies so I may have
;;        to override set-margin (and any other methods that may change the margin) to maintain consistency.

(module aligned-editor-container mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "list.ss")
   "snip-lib.ss"
   "interface.ss"
   "constants.ss")
  
  (provide
   aligned-editor-canvas%
   aligned-editor-snip%
   aligned-snip-mixin)
  
  ;; a canvas that can contain an aligned-pasteboard<%>
  (define aligned-editor-canvas%
    (class* editor-canvas% (aligned-pasteboard-parent<%>)
      (inherit get-editor get-size min-width min-height)
      (init-field (style empty))
      
      (field
       (width-diff 0)
       (height-diff 0))
      
      ;; set-aligned-min-size (-> (void))
      ;; sets the aligned min width and height of all aligned children
      (define/public (set-aligned-min-sizes)
        (let ([editor (get-editor)])
          (send editor set-aligned-min-sizes)
          (when (memq 'no-hscroll style)
            (min-width
             (+ (inexact->exact
                 (send editor get-aligned-min-width))
                machenrys-constant width-diff)))
          (when (memq 'no-vscroll style)
            (min-height
             (+ (inexact->exact
                 (send editor get-aligned-min-height))
                machenrys-constant height-diff)))))
      
      ;; on-size (number? number? . -> . (void))
      ;; called when the canvas's parent size changes
      (rename (super-on-size on-size))
      (define/override (on-size width height)
        (super-on-size width height)
        (send (get-editor) realign
              (- width width-diff machenrys-constant)
              (- height height-diff machenrys-constant)))
      
      ;; calc-view-client-diff (-> (void))
      ;; calculates and sets the difference between client-size and view-size of the editor
      (define/private (calc-view-client-diff)
        (let-values ([(width height) (get-size)])
          (let ([view-width (box 0)]
                [view-height (box 0)])
            (send (get-editor) get-view-size
                  view-width view-height)
            (set! width-diff
                  (- width
                     (inexact->exact
                      (unbox view-width))))
            (set! height-diff
                  (- height
                     (inexact->exact
                      (unbox view-height)))))))
      
      (super-instantiate ()
        (style style))
      (calc-view-client-diff)
      ))
  
  ;; a snip that can contain an aligned-pasteboard<%> and also be stretched within an aligned-pasteboard<%>
  (define aligned-editor-snip%
    (class* editor-snip% (aligned-pasteboard-parent<%> aligned-snip<%>)
      (inherit get-editor get-margin set-min-width set-min-height)
      
      (init
       (stretchable-width true)
       (stretchable-height true))
      
      (field
       (stretchable-width-field stretchable-width)
       (stretchable-height-field stretchable-height))
      
      (public (stretchable-width-method stretchable-width)
              (stretchable-height-method stretchable-height))
      
      ;; stretchable-width (case-> (Boolean . -> . (void)) (-> Boolean))
      ;; get or set the stretchablity of the pasteboards width
      (define stretchable-width-method
        (case-lambda
          [(value) (set! stretchable-width-field value)]
          [() stretchable-width-field]))
      
      ;; stretchable-height (case-> (Boolean . -> .(void)) (-> Boolean))
      ;; get or set the stretchablity of the pasteboards height
      (define stretchable-height-method
        (case-lambda
          [(value) (set! stretchable-height-field value)]
          [() stretchable-height-field]))
      
      ;; resize (number? number? . -> . boolean?)
      ;; called to resize the snip
      (rename [super-resize resize])
      (define/override (resize width height)        
        (super-resize width height)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (send (get-editor) realign
                (- width (unbox left) (unbox right))
                (- height (unbox top) (unbox bottom)))))
      
      ;; get-aligned-min-width (-> number?)
      ;; the minimum width of the snip based on the children
      (define/public (get-aligned-min-width)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (+ (unbox left)
             (unbox right)
             (send (get-editor) get-aligned-min-width)
             machenrys-constant)))
      
      ;; get-aligned-min-height (-> number?)
      ;; the minimum height of the snip based on the children
      (define/public (get-aligned-min-height)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (+ (unbox top)
             (unbox bottom)
             (send (get-editor) get-aligned-min-height)
             machenrys-constant)))
      
      ;; set-aligned-min-size (-> (void))
      ;; calculates and stores the minimum height and width of the snip
      (define/public (set-aligned-min-sizes)
        (send (get-editor) set-aligned-min-sizes)
        (set-min-width (get-aligned-min-width))
        (set-min-height (get-aligned-min-height)))
      
      (super-instantiate ())
      ))
  
  (define (aligned-snip-mixin super%)
    (class* super% (aligned-snip<%>)
      (inherit get-editor get-margin)
      
      (init
       (stretchable-width true)
       (stretchable-height true))
      
      (field
       (stretchable-width-field stretchable-width)
       (stretchable-height-field stretchable-height))
      
      (public (stretchable-width-method stretchable-width)
              (stretchable-height-method stretchable-height))
      
      ;; stretchable-width (case-> (Boolean . -> . (void)) (-> Boolean))
      ;; get or set the stretchablity of the pasteboards width
      (define stretchable-width-method
        (case-lambda
          [(value) (set! stretchable-width-field value)]
          [() stretchable-width-field]))
      
      ;; stretchable-height (case-> (Boolean . -> .(void)) (-> Boolean))
      ;; get or set the stretchablity of the pasteboards height
      (define stretchable-height-method
        (case-lambda
          [(value) (set! stretchable-height-field value)]
          [() stretchable-height-field]))
      
      ;; get-aligned-min-width (-> number?)
      ;; the minimum width of the snip based on the children
      (define/public (get-aligned-min-width)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (+ (unbox left)
             (let* ([ed (get-editor)]
                    [last (send ed last-line)])
               (let loop ([line 0])
                 (if (= line last)
                     0
                     (max (send ed line-length line)
                          (loop (add1 line))))))
             (unbox right))))
      
      ;; get-aligned-min-height (-> number?)
      ;; the minimum height of the snip based on the children
      (define/public (get-aligned-min-height)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)]
              [editor (get-editor)])
          (get-margin left top right bottom)
          (+ (unbox top) (unbox bottom)
             (* (send editor line-location 0 false)
                (add1 (send editor last-line))))))
      
;      ; get-aligned-min-width (-> number?)
;      ; the minimum width of the snip based on the children
;      (inherit get-max-width set-max-width get-min-width set-min-width)
;      (define/public (get-aligned-min-width)
;        (let* ([parent (snip-parent this)]
;               [ed (get-editor)]
;               [ed-max (send ed get-max-width)]
;               [ed-min (send ed get-min-width)]
;               [this-max (get-max-width)]
;               [this-min (get-min-width)])
;          (when (is-a? parent aligned-pasteboard<%>)
;            (send parent ignore-resizing true))
;          (send parent begin-edit-sequence)
;          (send ed begin-edit-sequence)
;          (send ed set-max-width 'none)
;          (send ed set-min-width 'none)
;          (set-max-width 'none)
;          (set-min-width 'none)
;          (begin0
;            (let ([left (box 0)]
;                  [top (box 0)]
;                  [right (box 0)]
;                  [bottom (box 0)])
;              (get-margin left top right bottom)
;              (+ (unbox left)
;                 (snip-width this)))
;            (send ed set-max-width ed-max)
;            (send ed set-max-width ed-min)
;            (set-min-width this-min)
;            (set-max-width this-max)
;            (send ed end-edit-sequence)
;            (send parent end-edit-sequence)
;            (when (is-a? parent aligned-pasteboard<%>)
;              (send parent ignore-resizing false)))))
;      
;      ; get-aligned-min-height (-> number?)
;      ; the minimum height of the snip based on the children
;      (inherit get-max-height set-max-height get-min-height set-min-height)
;      (define/public (get-aligned-min-height)
;        (let* ([parent (snip-parent this)]
;               [ed (get-editor)]
;               [ed-max (send ed get-max-height)]
;               [ed-min (send ed get-min-height)]
;               [this-max (get-max-height)]
;               [this-min (get-min-height)])
;          (when (is-a? parent aligned-pasteboard<%>)
;            (send parent ignore-resizing true))
;          (send parent begin-edit-sequence)
;          (send ed begin-edit-sequence)
;          (send ed set-max-height 'none)
;          (send ed set-min-height 'none)
;          (set-max-height 'none)
;          (set-min-height 'none)
;          (begin0
;            (snip-height this)
;            (send ed set-max-height ed-max)
;            (send ed set-min-height ed-min)
;            (set-min-height this-min)
;            (set-max-height this-max)
;            (send ed end-edit-sequence)
;            (send parent end-edit-sequence)
;            (when (is-a? parent aligned-pasteboard<%>)
;              (send parent ignore-resizing false)))))
      
      (super-instantiate ())
      ))
  )