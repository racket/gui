#lang racket/base
(require racket/unit)

(provide editor-misc-class^
         editor-misc^
         editor-misc-functions^
         editor-autoload^)

(define-signature editor-misc-class^
  (basic<%>
   standard-style-list<%>
   keymap<%>
   autowrap<%>
   info<%>
   file<%>
   backup-autosave<%>
   basic-mixin
   standard-style-list-mixin
   keymap-mixin
   autowrap-mixin
   info-mixin
   file-mixin
   backup-autosave-mixin
   font-size-message%))

(define-signature editor-misc-functions^
  (get-standard-style-list
   set-standard-style-list-pref-callbacks
   set-standard-style-list-delta
   set-default-font-color
   get-default-color-style-name
   add-after-user-keymap
   get-current-preferred-font-size
   set-current-preferred-font-size
   font-size-pref->current-font-size
   set-change-font-size-when-monitors-change?
   get-change-font-size-when-monitors-change?
   doing-autosave?
   silent-cancel-on-save-file-out-of-date?))

(define-signature editor-misc^
  ((open editor-misc-class^)
   (open editor-misc-functions^)))

(define-signature editor-autoload^
  (autoload-mixin
   autoload<%>))


