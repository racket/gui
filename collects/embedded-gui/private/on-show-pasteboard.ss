(module on-show-pasteboard mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred"))
  
  (provide
   on-show-pasteboard%
   on-show-pasteboard-mixin)
  
  (define (on-show-pasteboard-mixin super%)
    (class super%
      (field [shown? false])
      (rename [super-refresh refresh])
      (define/override (refresh x y w h d-c)
        (super-refresh x y w h d-c)
        (unless shown?
          (set! shown? true)
          (on-show)))
      (define/public (showing?)
        shown?)
      (define/public (on-show)
        (void))
      (super-new)))
  
  (define on-show-pasteboard%
    (on-show-pasteboard-mixin
     pasteboard%))
  
  #|
  (define f (new frame% (label "f") (width 400) (height 400)))
  (send f show true)
  (define e (new pasteboard%))
  (define c (new editor-canvas% (editor e) (parent f)))
  (define pb (new on-show-pasteboard%))
  (define es (new editor-snip% (editor pb)))
  (send e insert es)
  (send pb showing?)
  (send e remove es)
  (not (send pb showing?))
  |#
  )
