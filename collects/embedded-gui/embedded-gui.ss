(module embedded-gui mzscheme
  
  (require
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
   "private/snip-wrapper.ss")

  (provide
   (all-from "private/grid-alignment.ss")
   (all-from "private/aligned-pasteboard.ss")
   (all-from "private/interface.ss")
   (all-from "private/snip-lib.ss")
   (all-from "private/button-snip.ss")
   (all-from "private/stretchable-editor-snip.ss")
   (all-from "private/tabbable-text.ss")
   (all-from "private/fixed-width-label-snip.ss")
   (all-from "private/grey-editor.ss")
   (all-from "private/verthoriz-alignment.ss")
   (all-from "private/snip-wrapper.ss"))
  )
