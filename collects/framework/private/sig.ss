(module sig mzscheme
  (require (lib "unitsig.ss"))

  (provide framework:menu^
	   framework:version^
	   framework:panel^
	   framework:exn^
	   framework:application^
	   framework:preferences^
	   framework:autosave^
	   framework:exit^
	   framework:path-utils^
	   framework:finder^
	   framework:editor^
	   framework:pasteboard^
	   framework:text^
	   framework:canvas^
	   framework:frame^
	   framework:group^
	   framework:handler^
	   framework:icon^
	   framework:keymap^
	   framework:match-cache^
	   framework:scheme-paren^
	   framework:scheme^
	   framework:paren^
	   framework:main^
	   framework:color-model^)	   

    (define-signature framework:menu^
    (can-restore<%>
     can-restore-mixin
     can-restore-menu-item%
     can-restore-checkable-menu-item%))

  (define-signature framework:version^
    (add-spec
     version))

  (define-signature framework:panel^
    (single-mixin
     single<%>

     single-window<%>
     single-window-mixin

     ;;multi-view-mixin
     ;;multi-view<%>

     vertical-resizable<%>
     vertical-resizable-mixin

     two-panel<%>
     horizontal-two-panel<%>
     vertical-two-panel<%>

     two-panel-mixin
     horizontal-two-panel-mixin
     vertical-two-panel-mixin
     
     single%
     single-pane%
     ;;multi-view%
     vertical-resizable%
     vertical-resizable-pane%
     
     horizontal-two-panel%
     vertical-two-panel%))

  (define-signature framework:exn^
    ((struct exn ())
     (struct unknown-preference ())
     (struct during-preferences ())))

  (define-signature framework:application^
    (current-app-name))

  (define-signature framework:preferences^
    (get
     add-callback
     set
     set-default
     set-un/marshall

     save
     read
     restore-defaults

     add-panel
     add-font-panel
     add-general-panel
     show-dialog
     hide-dialog))

  (define-signature framework:autosave^
    (register))

  (define-signature framework:exit^
    (frame-exiting
     insert-on-callback
     insert-can?-callback
     can-exit?
     on-exit
     exit))

  (define-signature framework:path-utils^
    (generate-autosave-name 
     generate-backup-name))

  (define-signature framework:finder^
    (dialog-parent-parameter
     default-extension
     common-put-file 
     common-get-file 
     std-put-file 
     std-get-file 
     common-get-file-list
     get-file
     put-file))

  (define-signature framework:editor^
    (basic<%>
     keymap<%>
     autowrap<%>
     info<%>
     file<%>
     backup-autosave<%>
     
     basic-mixin
     keymap-mixin
     autowrap-mixin
     info-mixin
     file-mixin
     backup-autosave-mixin))

  (define-signature framework:pasteboard^
    (basic%
     keymap%
     file%
     backup-autosave%
     info%))

  (define-signature framework:text^
    (basic<%>
     hide-caret/selection<%>
     searching<%>
     return<%>
     info<%>
     clever-file-format<%>
     
     basic-mixin
     hide-caret/selection-mixin
     searching-mixin
     return-mixin
     info-mixin
     clever-file-format-mixin
     
     basic% 
     hide-caret/selection%
     keymap%
     return%
     autowrap%
     file%
     clever-file-format%
     backup-autosave%
     searching%
     info%))

  (define-signature framework:canvas^
    (basic-mixin
     basic<%>

     info-mixin
     info<%>

     wide-snip-mixin
     wide-snip<%>

     wide-snip%   
     basic%
     info%))

  (define-signature framework:frame^
    (reorder-menus

     basic<%>
     basic-mixin

     standard-menus<%>
     standard-menus-mixin

     editor<%>
     editor-mixin

     text<%>
     text-mixin

     pasteboard<%>
     pasteboard-mixin

     searchable<%>
     searchable-mixin

     searchable-text<%>
     searchable-text-mixin

     info<%>
     info-mixin

     text-info<%>
     text-info-mixin

     pasteboard-info<%>
     pasteboard-info-mixin

     file<%>
     file-mixin

     basic%
     info%
     text-info%
     pasteboard-info%
     standard-menus%
     editor%

     text%
     text-info-file%
     searchable%
     pasteboard%
     pasteboard-info-file%))

  (define-signature framework:group^
    (%
     get-the-frame-group))

  (define-signature framework:handler^
    (handler? handler-name handler-extension handler-handler
	      insert-format-handler
	      find-format-handler 
	      find-named-format-handler 
	      edit-file
	      open-file))

  (define-signature framework:icon^
    (get-paren-highlight-bitmap
     get-autowrap-bitmap

     get-lock-bitmap
     get-unlock-bitmap
     get-anchor-bitmap
     
     get-gc-on-bitmap
     get-gc-off-bitmap))

  (define-signature framework:keymap^
    (send-map-function-meta
     make-meta-prefix-list

     aug-keymap-mixin
     aug-keymap%
     aug-keymap<%>
     
     canonicalize-keybinding-string

     add-to-right-button-menu

     setup-global
     setup-search
     setup-file
     setup-editor

     get-global
     get-search
     get-file
     get-editor

     call/text-keymap-initializer))

  (define-signature framework:match-cache^
    (%))

  (define-signature framework:scheme-paren^
    (get-comments
     get-paren-pairs
     get-quote-pairs
     forward-match
     backward-match
     balanced?
     backward-containing-sexp))

  (define-signature framework:scheme^
    (get-wordbreak-map
     init-wordbreak-map

     get-style-list

     get-keymap
     setup-keymap
     text-mixin
     text<%>
     text%
     add-preferences-panel))

  (define-signature framework:paren^
    (balanced? 
     forward-match 
     backward-match
     skip-whitespace))

  (define-signature framework:main^ ())

  (define-signature framework:color-model^
    (rgb-color-distance rgb->xyz xyz->rgb)))
