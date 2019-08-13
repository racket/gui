#lang racket/base
(require racket/unit
         mred/mred-sig
         "text-sig.rkt"
         "sig.rkt")

(provide text-classes@)

(define-unit text-classes@
  (import mred^
          text-basic^
          text-misc^
          text-normalize-paste^
          text-delegate^
          text-port^
          text-search^
          [prefix editor: framework:editor^])
  (export (rename text-mixed-in-classes^ [-keymap% keymap%]))

  (init-depend framework:editor^
               text-basic^
               text-misc^
               text-normalize-paste^
               text-delegate^
               text-port^
               text-search^
               mred^)
  
  (define basic% (basic-mixin (editor:basic-mixin text%)))
  (define line-spacing% (line-spacing-mixin basic%))
  (define hide-caret/selection% (hide-caret/selection-mixin line-spacing%))
  (define nbsp->space% (nbsp->space-mixin line-spacing%))
  (define normalize-paste% (normalize-paste-mixin line-spacing%))
  (define delegate% (delegate-mixin line-spacing%))
  (define wide-snip% (wide-snip-mixin line-spacing%))
  (define standard-style-list% (editor:standard-style-list-mixin wide-snip%))
  (define input-box% (input-box-mixin standard-style-list%))
  (define -keymap% (overwrite-disable-mixin (editor:keymap-mixin standard-style-list%)))
  (define return% (return-mixin -keymap%))
  (define autowrap% (editor:autowrap-mixin -keymap%))
  (define file% (file-mixin (editor:file-mixin autowrap%)))
  (define clever-file-format% (crlf-line-endings-mixin (clever-file-format-mixin file%)))
  (define backup-autosave% (editor:backup-autosave-mixin clever-file-format%))
  (define searching% (searching-mixin backup-autosave%))
  (define info% (info-mixin (editor:info-mixin searching%))))
