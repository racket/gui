
(module decorated-editor-snip mzscheme
  (provide decorated-editor-snip%
           decorated-editor-snipclass%)

  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (define decorated-editor-snip%
    (class editor-snip% 
      (inherit get-editor get-style)
      
      ;; make-snip : -> this%
      ;; returns an instance of this class. used in the copy method
      (define/public (make-snip) (make-object decorated-editor-snip%))
      
      ;; make-editor : -> editor<%>
      ;; returns the editor to be used in this snip.
      (define/public (make-editor) (make-object text%))
      
      ;; get-corner-bitmap : -> (union #f (is-a?/c bitmap%))
      ;; returns the bitmap to be shown in the top right corner.
      (define/public (get-corner-bitmap) #f)
      
      ;; get-color : -> (union string (is-a?/c color%))
      (define/public (get-color) "black")
      
      ;; get-menu : -> (union #f (is-a?/c popup-menu%))
      ;; returns the popup menu that should appear
      ;; when clicking in the top part of the snip.
      (define/public (get-menu) #f)

      [define/private (get-pen) (send the-pen-list find-or-create-pen (get-color) 1 'solid)]
      [define/private (get-brush) (send the-brush-list find-or-create-brush "BLACK" 'transparent)]      
      
      (inherit get-admin)
      (rename [super-on-event on-event])
      (define/override (on-event dc x y editorx editory evt)
        (cond
          [(send evt get-right-down)
           (let ([sx (- (send evt get-x) x)]
                 [sy (- (send evt get-y) y)]
                 [bil (box 0)]
                 [bit (box 0)]
                 [bir (box 0)]
                 [bib (box 0)]
                 [bw (box 0)]
                 [bh (box 0)]
                 [bml (box 0)]
                 [bmt (box 0)]
                 [bmr (box 0)]
                 [bmb (box 0)])
             (get-extent dc x y bw bh #f #f #f #f)
             (get-inset bil bit bir bib)
             (get-margin bml bmt bmr bmb)
             (let ([menu (get-menu)])
               (cond
                 [(and menu
                       (<= 0 sx (unbox bw))
                       (<= 0 sy (unbox bmt)))
                  (let ([admin (get-admin)])
                    (send admin popup-menu menu this (+ sx 1) (+ sy 1)))]
                 [else (super-on-event dc x y editorx editory evt)])))]
          [else
           (super-on-event dc x y editorx editory evt)]))
      
      (inherit get-extent get-inset)
      (rename [super-draw draw])
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([bm (get-corner-bitmap)]
              [bil (box 0)]
              [bit (box 0)]
              [bir (box 0)]
              [bib (box 0)]
              [bw (box 0)]
              [bh (box 0)]
              [bml (box 0)]
              [bmt (box 0)]
              [bmr (box 0)]
              [bmb (box 0)])
          (get-extent dc x y bw bh #f #f #f #f)
          (get-inset bil bit bir bib)
          (get-margin bml bmt bmr bmb)
          (super-draw dc x y left top right bottom dx dy draw-caret)
          (let* ([old-pen (send dc get-pen)]
                 [old-brush (send dc get-brush)])
            
            (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
            (send dc set-brush (send the-brush-list find-or-create-brush "white" 'solid))
            (send dc draw-rectangle 
                  (+ x (unbox bml))
                  (+ y (unbox bit))
                  (max 0 (- (unbox bw) (unbox bml) (unbox bmr)))
                  (- (unbox bmt) (unbox bit)))
            
            (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
            (send dc set-brush (send the-brush-list find-or-create-brush "black" 'solid))

            (when bm
              (let ([bm-w (send bm get-width)]
                    [bm-h (send bm get-height)])
                (send dc draw-bitmap
                      bm
                      (+ x (max 0
                                (- (unbox bw)
                                   (unbox bmr)
                                   bm-w)))
                      ;; leave two pixels above and two below (see super-instantiate below)
                      (+ y (unbox bit) 2))))
            
            (send dc set-pen (get-pen))
            (send dc set-brush (get-brush))
            (send dc draw-rectangle
                  (+ x (unbox bil))
                  (+ y (unbox bit))
                  (max 0 (- (unbox bw) (unbox bil) (unbox bir)))
                  (max 0 (- (unbox bh) (unbox bit) (unbox bib))))
            
            (send dc set-pen old-pen)
            (send dc set-brush old-brush))))
      
      (define/override write
        (lambda (stream-out)
          (send (get-editor) write-to-file stream-out 0 'eof)))

      (define/override (copy)
        (let ([snip (make-snip)])
          (send snip set-editor (send (get-editor) copy-self))
          (send snip set-style (get-style))
          snip))
      
      (inherit set-min-width get-margin)
      (define/public (reset-min-width)
        (let ([lib (box 0)]
              [rib (box 0)]
              [lmb (box 0)]
              [rmb (box 0)])
          (get-inset lib (box 0) rib (box 0))
          (get-margin lmb (box 0) rmb (box 0))
          (let ([bm (get-corner-bitmap)])
            (when bm
              (set-min-width 
               (max 0 (send bm get-width)))))))
      
      (super-instantiate ()
        (editor (make-editor))
        (with-border? #f)
        (top-margin (+ 4 
                       (let ([bm (get-corner-bitmap)])
                         (if bm
                             (send bm get-height)
                             0)))))
      
      (reset-min-width)))
  
  (define decorated-editor-snipclass%
    (class snip-class%
      
      ;; make-snip : stream-in -> (is-a?/c snip%)
      ;; returns an unfilled version of the snip
      (define/public (make-snip stream-in) (make-object decorated-editor-snip%))
      
      (define/override (read stream-in)
        (let ([snip (make-snip stream-in)])
          (send (send snip get-editor) read-from-file stream-in)
          snip))
      (super-instantiate ()))))