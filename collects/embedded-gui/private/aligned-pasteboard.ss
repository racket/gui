#| Note: It might be a good idea to override insert with an error so that people don't
   insert or delete from the pasteboard without using the alignment<%>. Then the alignments
   could go through a different interface for inserting the snips that would call
   super-insert.
|#

(module aligned-pasteboard mzscheme
  
  (provide aligned-pasteboard%)
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "click-forwarding-editor.ss" "mrlib")
   "on-show-pasteboard.ss"
   "really-resized-pasteboard.ss"
   "interface.ss"
   "locked-pasteboard.ss"
   "suppress-modify-editor.ss")
  
  (define aligned-pasteboard%
    (class* (click-forwarding-editor-mixin
             (on-show-pasteboard-mixin
              (suppress-modify-editor-mixin
               (locked-pasteboard-mixin
                (really-resized-pasteboard-mixin pasteboard%)))))
      (alignment-parent<%>)
      
      (inherit begin-edit-sequence end-edit-sequence
               get-max-view-size refresh-delayed?)
      
      (field
       [alignment false]
       [lock-alignment? false]
       [needs-alignment? false])
      
      ;;;;;;;;;;
      ;; alignment-parent<%>
      
      #;(-> (is-a?/c pasteboard%))
      ;; The pasteboard that this alignment is being displayed to
      (define/public (get-pasteboard) this)
      
      #;((is-a?/c alignment<%>) . -> . void?)
      ;; Set the given alignment as a the child
      (define/public (add-child child)
        (if alignment
            (error 'add-child "There may be only one alignment<%> of a pasteboard")
            (set! alignment child)))
      
      #;(-> boolean?)
      ;; True if the alignment is being shown (accounting for its parent being shown)
      ;; NOTE: Pasteboards are always shown and have no show/hide state.
      (define/public (is-shown?) true)
      
      #;((is-a?/c snip%) . -> . void?)
      ;; Called when a snip in the pasteboard changes its size
      ;; Overriden because the layout will change when a snip gets bigger.
      (rename [super-really-resized really-resized])
      (define/override (really-resized snip)
        (super-really-resized snip)
        (realign))
      
      #;(-> void)
      ;; Called when the pasteboard is first shown.
      ;; Overriden because I need to know when the snips have their size to lay them out.
      (rename [super-on-show on-show])
      (define/override (on-show)
        (realign)
        (super-on-show))
      
      #;(boolean? . -> . void?)
      ;; Locks the pasteboard so that all alignment requests are delayed until after it's done.
      (define/public (lock-alignment lock?)
        (set! lock-alignment? lock?)
        (when (and needs-alignment? (not lock-alignment?))
          (realign))
        (if lock?
            (begin-edit-sequence)
            (end-edit-sequence)))
      
      #;(-> void?)
      ;; Realigns the snips in the pasteboard according to the alignment tree.
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
      
      (super-new)))
  )
