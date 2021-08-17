#lang racket/base
(require racket/class
         racket/draw/private/color
         racket/math
         (only-in racket/draw/unsafe/pango
                  pango_attr_list_new
                  pango_attr_list_insert
                  pango_attr_foreground_new
                  pango_attr_foreground_alpha_new)
         ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "pixbuf.rkt"
         "window.rkt")

(provide
 (protect-out message%

              gtk_label_new_with_mnemonic
              gtk_label_set_text_with_mnemonic))

;; ----------------------------------------

(define-gtk gtk_label_new (_fun _string -> _GtkWidget))
(define-gtk gtk_label_set_text_with_mnemonic (_fun _GtkWidget _string -> _void))
(define-gtk gtk_label_set_attributes (_fun _GtkWidget _pointer -> _void))
(define-gtk gtk_image_new_from_stock (_fun _string _int -> _GtkWidget))
(define-gtk gtk_misc_set_alignment (_fun _GtkWidget _float _float -> _void))
(define-gtk gtk_image_set_from_pixbuf (_fun _GtkWidget _GdkPixbuf -> _void))

(define (gtk_label_new_with_mnemonic s)
  (let ([l (gtk_label_new s)])
    (when (regexp-match? #rx"&" s)
      (let ([s (mnemonic-string s)])
        (gtk_label_set_text_with_mnemonic l s)))
    l))

(define icon-size 6) ; = GTK_ICON_SIZE_DIALOG

(define (color-component->gtk c)
  (exact-round (* (/ c 255.0) 65535)))

(define (do-set-label-color label c)
  (define attrs (pango_attr_list_new))
  (define color-attr (pango_attr_foreground_new
                      (color-component->gtk (color-red c))
                      (color-component->gtk (color-green c))
                      (color-component->gtk (color-blue c))))
  (define color-alpha-attr (pango_attr_foreground_alpha_new
                            (color-component->gtk (* (color-alpha c) 255))))
  (pango_attr_list_insert attrs color-attr)
  (pango_attr_list_insert attrs color-alpha-attr)
  (gtk_label_set_attributes label attrs))

(defclass message% item%
  (init parent label
        x y
        style font color)
  (inherit set-auto-size get-gtk)

  (super-new [parent parent]
             [gtk (cond
                    [(or (string? label) (not label))
                     (define gtk-label
                       (as-gtk-allocation (gtk_label_new_with_mnemonic (or label ""))))
                     (when color
                       (do-set-label-color gtk-label color))
                     gtk-label]
                    [(symbol? label)
                     (as-gtk-allocation
                      (case label
                        [(caution) (gtk_image_new_from_stock "gtk-dialog-warning" icon-size)]
                        [(stop) (gtk_image_new_from_stock "gtk-dialog-error" icon-size)]
                        [else (gtk_image_new_from_stock "gtk-dialog-question" icon-size)]))]
                    [else
                     (define pixbuf (bitmap->pixbuf label (->screen 1.0)))
                     (begin0
                         (as-gtk-allocation
                          (gtk_image_new_from_pixbuf pixbuf))
                       (release-pixbuf pixbuf))])]
             [font font]
             [no-show? (memq 'deleted style)])

  (when (string? label)
    (gtk_misc_set_alignment (get-gtk) 0.0 0.0))

  (set-auto-size)

  (define/override (set-label s)
    (cond
      [(string? s)
       (gtk_label_set_text_with_mnemonic (get-gtk) (mnemonic-string s))]
      [else
       (let ([pixbuf (bitmap->pixbuf s (->screen 1.0))])
         (atomically
          (gtk_image_set_from_pixbuf (get-gtk) pixbuf)
          (release-pixbuf pixbuf)))]))

  (define/public (set-label-color c)
    (do-set-label-color (get-gtk) c))

  (define/public (set-preferred-size)
    (gtk_widget_set_size_request (get-gtk) -1 -1)
    (set-auto-size)
    #t)

  (define/override (gets-focus?) #f)

  (def/public-unimplemented get-font))
