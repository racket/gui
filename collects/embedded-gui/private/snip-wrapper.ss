(module snip-wrapper mzscheme
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   "interface.ss"
   (prefix sl: "snip-lib.ss"))
  
  (provide snip-wrapper%)
  
  (define snip-wrapper%
    (class* object% (alignment<%>)
      (init-field snip parent)
      (field [pasteboard (send parent get-pasteboard)]
             [show? true]
             [snip-is-inserted? false]
             [min-width 0]
             [min-height 0])
      
      ;;;;;;;;;;
      ;; alignment<%>
      
      #;(-> void?)
      ;; Tells the alignment that its sizes should be calculated
      (define/public (set-min-sizes)
        (if snip-is-inserted?
            (begin (set! min-width (sl:snip-min-width snip))
                   (set! min-height (sl:snip-min-height snip)))
            (begin (set! min-width 0)
                   (set! min-height 0))))
      
      #;(nonnegative? nonnegative? nonnegative? nonnegative? . -> . void?)
      ;; Tells the alignment to align its children on the pasteboard in the given rectangle
      ;; STATUS: I don't currently handle stretchability
      (define/public (align x y w h)
        (send pasteboard move-to snip x y)
        (when (is-a? snip stretchable-snip<%>)
          (send snip stretch w h)))
      
      #;(-> nonnegative?)
      ;; The minimum width this alignment must be.
      (define/public (get-min-width) min-width)
      
      #;(-> nonnegative?)
      ;; The minimum height this alignment must be.
      (define/public (get-min-height) min-height)
      
      #;(-> boolean?)
      ;; True if the alignment can be stretched in the x dimension
      (define/public (stretchable-width?)
        (sl:stretchable-width? snip))
      
      #;(-> boolean?)
      ;; True if the alignment can be stretched in the y dimension
      (define/public (stretchable-height?)
        (sl:stretchable-height? snip))
      
      #;(boolean? . -> . void?)
      ;; Tells the alignment that its show state is the given value
      ;; and it should show or hide its children accordingly.
      (define/public (show bool)
        (set! show? bool)
        (show/hide show?))
      
      #;(boolean? . -> . void)
      ;; Tells the alignment to show or hide its children
      (define/public (show/hide bool)
        (unless (boolean=? bool snip-is-inserted?)
          (send pasteboard lock-alignment true)
          (if bool
              (begin (send pasteboard insert snip)
                     (set! snip-is-inserted? true))
              (begin (send pasteboard release-snip snip)
                     (set! snip-is-inserted? false)))
          (send pasteboard realign)
          (send pasteboard lock-alignment false)))
      
      (super-new)
      (send parent add-child this)
      (show/hide (and show? (send parent is-shown?)))))
  )