#lang racket/base
(require racket/unit)

(provide text-basic^
         text-basic-class^
         text-basic-functions^
         text-line-numbers^
         text-autocomplete^
         text-autocomplete-class^
         text-autocomplete-functions^
         text-normalize-paste^
         text-column-guide^
         text-ascii-art^
         text-misc^
         text-max-width-paragraph^
         text-delegate^
         text-port^
         text-port-class^
         text-port-functions^
         text-search^
         text-first-line^
         text-inline-overview^
         text-mixed-in-classes^
         text-indent-guides^)

(define-signature text-basic-class^
  (basic<%>
   basic-mixin))
(define-signature text-basic-functions^
  (range?
   range-start
   range-end
   range-caret-space? 
   range-style 
   range-color
   lookup-port-name))
(define-signature text-basic^
  ((open text-basic-class^)
   (open text-basic-functions^)))

(define-signature text-line-numbers^
  (line-numbers<%>
   line-numbers-mixin))

(define-signature text-indent-guides^
  (indent-guides<%>
   indent-guides-mixin))

(define-signature text-autocomplete-class^
  (autocomplete<%>
   autocomplete-mixin))
(define-signature text-autocomplete-functions^
  (get-completions/manuals
   autocomplete-append-after
   autocomplete-limit))
(define-signature text-autocomplete^
  ((open text-autocomplete-class^)
   (open text-autocomplete-functions^)))

(define-signature text-column-guide^
  (column-guide<%>
   column-guide-mixin))

(define-signature text-normalize-paste^
  (normalize-paste<%>
   normalize-paste-mixin))

(define-signature text-ascii-art^
  (ascii-art-enlarge-boxes<%>
   ascii-art-enlarge-boxes-mixin))

(define-signature text-delegate^
  (delegate<%>
   delegate-mixin
   1-pixel-string-snip%
   1-pixel-tab-snip%))

(define-signature text-port-class^
  (input-box<%>
   input-box-mixin
   ports<%>
   ports-mixin
   wide-snip<%>
   wide-snip-mixin))
(define-signature text-port-functions^
  (make-snip-special
   send-snip-to-port
   snip-special?))
(define-signature text-port^
  ((open text-port-class^)
   (open text-port-functions^)))

(define-signature text-search^
  (searching<%>
   searching-mixin))

(define-signature text-inline-overview^
  (inline-overview<%>
   inline-overview-mixin
   inline-overview-mpw-mixin))

(define-signature text-first-line^
  (first-line<%>
   first-line-mixin))

(define-signature text-misc^
  (line-spacing<%>
   line-spacing-mixin
   foreground-color<%>
   foreground-color-mixin
   hide-caret/selection<%>
   hide-caret/selection-mixin
   nbsp->space<%>
   nbsp->space-mixin
   return<%>
   return-mixin
   info<%>
   info-mixin
   clever-file-format<%>
   clever-file-format-mixin
   crlf-line-endings<%>
   crlf-line-endings-mixin
   file<%>
   file-mixin
   all-string-snips<%>
   all-string-snips-mixin
   overwrite-disable<%>
   overwrite-disable-mixin))

(define-signature text-max-width-paragraph^
  (max-width-paragraph-mixin
   max-width-paragraph<%>))

(define-signature text-mixed-in-classes^
  (basic%
   line-spacing%
   hide-caret/selection%
   nbsp->space%
   normalize-paste%
   delegate%
   standard-style-list%
   input-box%
   wide-snip%
   keymap%
   return%
   autowrap%
   file%
   clever-file-format%
   backup-autosave%
   searching%
   info%))
