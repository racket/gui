(module embedded-gui mzscheme
  
  (define-syntax (require/provide stx)
    (syntax-case stx ()
      [(_ filename ...)
      #'(begin (require filename ...)
               (provide (all-from filename) ...))]))
  
  (require/provide
   "private/grid-alignment.ss"
   "private/aligned-pasteboard.ss"
   "private/interface.ss"
   "private/snip-lib.ss"
   "private/button-snip.ss"
   "private/stretchable-editor-snip.ss"
   "private/tabbable-text.ss"
   "private/fixed-width-label-snip.ss"
   "private/grey-editor.ss"
   "private/verthoriz-alignment.ss"
   "private/snip-wrapper.ss"
   "private/single-line-text.ss"
   "private/embedded-message.ss"
   "private/lines.ss")
  )
