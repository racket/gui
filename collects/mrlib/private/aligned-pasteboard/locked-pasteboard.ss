(module locked-pasteboard mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "contract.ss"))
  
  (provide/contract
   (locked-pasteboard-mixin mixin-contract))
  
  ;; mixin to remove interactive movement of snips from pasteboards
  (define (locked-pasteboard-mixin super%)
    (class super%
      ;; can-interactive-move? (event? . -> . void?)
      ;; whether the pasteboard allows interactive moving
      (define/override (can-interactive-move? event)
        false)
      
      ;; can-interactive-resize? ((is-a?/c snip%) . -> . void?)
      ;; whether the pasteboard allows interactive resizing
      (define/override (can-interactive-resize? snip)
        false)
      (super-instantiate ())
      ))
  )
