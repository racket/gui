(module aligned-pasteboard mzscheme
  
  (provide aligned-pasteboard%)
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "match.ss")
   (prefix a: "alignment.ss")
   (lib "click-forwarding-editor.ss" "mrlib")
   "on-show-pasteboard.ss"
   "really-resized-pasteboard.ss"
   "interface.ss"
   "snip-lib.ss"
   "locked-pasteboard.ss"
   "verthoriz-alignment.ss"
   "suppress-modify-editor.ss")
  
  (require
   (lib "print-debug.ss" "mike-lib"))
  
  (define aligned-pasteboard%
    (class (click-forwarding-editor-mixin
            (on-show-pasteboard-mixin
              (suppress-modify-editor-mixin
               (locked-pasteboard-mixin
                (really-resized-pasteboard-mixin pasteboard%)))))
      
      (inherit begin-edit-sequence end-edit-sequence
               get-max-view-size refresh-delayed?)
      (init align)
      (field
       [alignment (new (case align
                         [(horizontal) horizontal-alignment%]
                         [(vertical) vertical-alignment%]))]
       [lock-alignment? false]
       [needs-alignment? false])
      
      (define/public (get-alignment) alignment)
      
      #|
        snip : snip% object
        before : snip% object or #f
        x : real number
        y : real number
      |#
      (rename [super-after-insert after-insert])
      (define/override (after-insert snip before x y)
        (super-after-insert snip before x y)
        (realign))
      
      #|
        snip : snip% object
      |#
      (rename [super-after-delete after-delete])
      (define/override (after-delete snip)
        (super-after-delete snip)
        (realign))
      
      #|
        snip : snip% object
      |#
      (rename [super-really-resized really-resized])
      (define/override (really-resized snip)
        (super-really-resized snip)
        (realign))
      
      (rename [super-on-show on-show])
      (define/override (on-show)
        (realign)
        (super-on-show))
      
      (define/public (lock-alignment lock?)
        (set! lock-alignment? lock?)
        (when (and needs-alignment? (not lock-alignment?))
          (realign))
        (if lock?
            (begin-edit-sequence)
            (end-edit-sequence)))
      
      (define/public (realign)
        (if lock-alignment?
            (set! needs-alignment? true)
            (fluid-let ([lock-alignment? true])
              (send alignment set-min-sizes)
              (let ([width (send alignment get-min-width)]
                    [height (send alignment get-min-height)])
                (unless (or (zero? width) (zero? height))
                  (send alignment align 0 0 width height)
                  (set! needs-alignment? false))))))
      
      (super-new)
      (send alignment set-pasteboard this)))
  )
