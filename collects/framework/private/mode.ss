(module mode mzscheme
  (require (lib "surrogate.ss")
           (lib "unitsig.ss")
           "sig.ss")
  
  (provide mode@)
  
  (define mode@
    (unit/sig framework:mode^
      (import)
      
      (define-values (host-text-mixin host-text<%> surrogate-text% surrogate-text<%>)
        (surrogate
         (on-change ())
         (on-char (event))
         (on-default-char (event))
         (on-default-event (event))
         (on-display-size ())
         (on-edit-sequence ())
         (on-event (event))
         (on-focus (on?))
         (on-load-file (filename format))
         (on-local-char (event))
         (on-local-event (event))
         (on-new-box (type))
         (on-new-image-snip (filename kind relative-path? inline?))
         (on-paint (before? dc left top right bottom dx dy draw-caret))
         (on-save-file (filename format))
         (on-snip-modified (snip modified?))
         
         (on-change-style (start len))
         (on-delete (start len))
         (on-insert (start len))
         (on-new-string-snip ())
         (on-new-tab-snip ())
         (on-set-size-constraint ())
         
         (after-change-style (start len))
         (after-delete (start len))
         (after-insert (start len))
         (after-set-position ())
         (after-set-size-constraint ())
         (after-edit-sequence ())
         (after-load-file (success?))
         (after-save-file (success?))
         
         (can-change-style? (start len))
         (can-delete? (start len))
         (can-insert? (start len))
         (can-set-size-constraint? ())
         (can-do-edit-operation? (op) (op recursive?))
         (can-load-file? (filename format))
         (can-save-file? (filename format)))))))
