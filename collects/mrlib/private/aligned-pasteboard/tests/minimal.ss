(require
 "../aligned-editor-container.ss"
 "../aligned-pasteboard.ss"
 (lib "debug.ss" "mike-lib"))

(define my-string-snip%
  (class string-snip%
    (init-field label)
    (rename [super-size-cache-invalid size-cache-invalid])
    (define/override (size-cache-invalid)
      (mytrace size-cache-invalid ()
        (super-size-cache-invalid)))
    (super-make-object label)))

(define f (new frame% (label "test") (width 200) (height 200)))
(define e (new vertical-pasteboard%))
(define c (new aligned-editor-canvas% (editor e) (parent f)))
(define pb (new vertical-pasteboard%))
(define s (new aligned-editor-snip% (editor pb) (stretchable-height #f) (stretchable-width #f)))
(send pb insert (new my-string-snip% (label "Long snip")))
(send pb insert (new my-string-snip% (label "Longer snip")))
(send e insert s)
(send f show #t)