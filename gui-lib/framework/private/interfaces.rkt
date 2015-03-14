#lang racket/base
(require racket/class
         racket/gui/base
         "gen-standard-menus.rkt")

(provide editor:basic<%>
         editor:keymap<%>
         text:basic<%>
         frame:basic<%>
         frame:standard-menus<%>
         frame:info<%>
         frame:text-info<%>)

(define editor:basic<%>
  (interface (editor<%>)
    has-focus?
    local-edit-sequence?
    run-after-edit-sequence
    get-top-level-window
    save-file-out-of-date?
    save-file/gui-error
    load-file/gui-error
    on-close
    can-close?
    close
    get-filename/untitled-name
    
    get-pos/text
    get-pos/text-dc-location))

(define editor:keymap<%>
  (interface (editor:basic<%>)
    get-keymaps))

(define text:basic<%>  
  (interface (editor:basic<%> (class->interface text%))
    highlight-range
    unhighlight-range
    unhighlight-ranges
    unhighlight-ranges/key
    get-highlighted-ranges
    get-styles-fixed
    get-fixed-style
    set-styles-fixed
    move/copy-to-edit
    initial-autowrap-bitmap
    get-port-name
    port-name-matches?
    after-set-port-unsaved-name
    set-port-unsaved-name
    get-start-of-line))

(define frame:basic<%>
  (interface ((class->interface frame%))
    get-area-container%
    get-area-container
    get-menu-bar%
    make-root-area-container
    close
    editing-this-file?
    get-filename
    make-visible))

(generate-standard-menus-interface-code)

(define frame:info<%>
  (interface (frame:basic<%>)
    determine-width
    lock-status-changed
    update-info
    set-info-canvas
    get-info-canvas
    get-info-editor
    get-info-panel
    show-info
    hide-info
    is-info-hidden?))

(define frame:text-info<%>
  (interface (frame:info<%>)
    set-macro-recording
    overwrite-status-changed
    anchor-status-changed
    editor-position-changed
    use-file-text-mode-changed
    add-line-number-menu-items))