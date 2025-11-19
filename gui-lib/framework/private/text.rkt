#lang racket/base

(require racket/unit
         mred/mred-sig
         "text-ascii-art.rkt"
         "text-autocomplete.rkt"
         "text-basic.rkt"
         "text-classes.rkt"
         "text-column-guide.rkt"
         "text-delegate.rkt"
         "text-first-line.rkt"
         "text-line-numbers.rkt"
         "text-indent-guides.rkt"
         "text-misc.rkt"
         "text-normalize-paste.rkt"
         "text-port.rkt"
         "text-search.rkt"
         "text-inline-overview.rkt"
         "text-max-width-paragraph.rkt"
         "text-sig.rkt"
         "srcloc-snip.rkt"
         "sig.rkt")

(provide text@)

(define-compound-unit/infer text-cu@
  (import [mred : mred^]
          [icon : framework:icon^]
          [editor : framework:editor^]
          [keymap : framework:keymap^]
          [color-model : framework:color-model^]
          [color-prefs : framework:color-prefs^]
          [frame : framework:frame^]
          [racket : framework:racket^]
          [number-snip : framework:number-snip^]
          [finder : framework:finder^]
          [srcloc-snip : framework:srcloc-snip^])

  (export text-ascii-art^
          text-autocomplete^
          text-basic^
          text-mixed-in-classes^
          text-column-guide^
          text-delegate^
          text-first-line^
          text-inline-overview^
          text-line-numbers^
          text-indent-guides^
          text-misc^
          text-normalize-paste^
          text-port^
          text-search^
          text-max-width-paragraph^)

  (link text-ascii-art@
        text-autocomplete@
        text-basic@
        text-column-guide@
        text-delegate@
        text-first-line@
        text-max-width-paragraph@
        text-inline-overview@
        text-line-numbers@
        text-indent-guides@
        text-misc@
        text-normalize-paste@
        text-port@
        text-search@
        text-classes@))

(define-unit/new-import-export text@
  (import (prefix mred: mred^)
          framework:icon^
          (prefix editor: framework:editor^)
          framework:keymap^
          framework:color-model^
          framework:color-prefs^
          (prefix frame: framework:frame^)
          framework:racket^
          framework:number-snip^
          (prefix finder: framework:finder^)
          framework:srcloc-snip^
          )
  (export framework:text^)
  ((text-ascii-art^
    text-autocomplete^
    text-basic^
    text-mixed-in-classes^
    text-column-guide^
    text-delegate^
    text-first-line^
    text-inline-overview^
    text-line-numbers^
    text-indent-guides^
    text-misc^
    text-max-width-paragraph^
    text-normalize-paste^
    text-port^
    text-search^)
   text-cu@
   (prefix mred: mred^)
   framework:icon^
   (prefix editor: framework:editor^)
   framework:keymap^
   framework:color-model^
   framework:color-prefs^
   (prefix frame: framework:frame^)
   framework:racket^
   framework:number-snip^
   (prefix finder: framework:finder^)
   framework:srcloc-snip^))
