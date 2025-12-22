#lang racket/base

(require racket/unit
         mred/mred-sig
         "editor-misc.rkt"
         "editor-autoload.rkt"
         "editor-sig.rkt"
         "sig.rkt")

(provide editor@)

(define-compound-unit/infer editor-cu@
  (import [mred : mred^]
          [autosave : framework:autosave^]
          [finder : framework:finder^]
          [path-utils : framework:path-utils^]
          [keymap : framework:keymap^]
          [text : framework:text^]
          [pasteboard : framework:pasteboard^]
          [frame : framework:frame^]
          [handler : framework:handler^]
          [color-prefs : framework:color-prefs^])

  (export editor-misc^
          editor-autoload^)

  (link editor-misc@
        editor-autoload@))

(define-unit/new-import-export editor@
  (import (prefix mred: mred^)
          framework:autosave^
          (prefix finder: framework:finder^)
          framework:path-utils^
          framework:keymap^
          (prefix text: framework:text^)
          (prefix pasteboard: framework:pasteboard^)
          (prefix frame: framework:frame^)
          framework:handler^
          [prefix color-prefs: framework:color-prefs^]
          )
  (export framework:editor^)
  ((editor-misc^ editor-autoload^)
   editor-cu@
   (prefix mred: mred^)
   framework:autosave^
   (prefix finder: framework:finder^)
   framework:path-utils^
   framework:keymap^
   (prefix text: framework:text^)
   (prefix pasteboard: framework:pasteboard^)
   (prefix frame: framework:frame^)
   framework:handler^
   (prefix color-prefs: framework:color-prefs^)))
