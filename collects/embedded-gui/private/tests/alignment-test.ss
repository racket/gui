(require
 (lib "class.ss")
 (lib "etc.ss")
 (lib "mred.ss" "mred")
 (lib "print-debug.ss" "mike-lib")
 "../stretchable-editor-snip.ss"
 "../verthoriz-alignment.ss"
 "../aligned-pasteboard.ss"
 "../grid-alignment.ss"
 "../snip-wrapper.ss")

(define f (new frame% (label "f") (height 500) (width 500)))
(send f show true)
(define a1 (new aligned-pasteboard%))
(define c (new editor-canvas% (editor a1) (parent f)))
(define a2 (new horizontal-alignment% (parent a1)))
(define a3 (new horizontal-alignment% (parent a1)))
(new snip-wrapper%
     (snip (make-object string-snip% "One"))
     (parent a2))
(new snip-wrapper%
     (snip (make-object string-snip% "Two"))
     (parent a2))
(new snip-wrapper%
     (snip (make-object string-snip% "Three"))
     (parent a3))

(send f show true)