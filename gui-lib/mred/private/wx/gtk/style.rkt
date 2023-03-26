#lang racket/base
(require ffi/unsafe
         "types.rkt"
         "utils.rkt"
         "init.rkt"
         "frame.rkt"
         "../common/queue.rkt")

(provide 
 (protect-out get-selected-text-color
              get-selected-background-color
	      get-label-fg-color
	      get-label-bg-color))

(define-cstruct _GTypeInstance
  ([class _pointer]))

(define-cstruct _GObject
  ([g_type_instance _GTypeInstance]
   [ref_count _uint]
   [qdata _pointer]))

(define-cstruct _GtkStyle
  ([obj _GObject]
   [fg1 _GdkColor]
   [fg2 _GdkColor]
   [fg3 _GdkColor]
   [fg4 _GdkColor]
   [fg5 _GdkColor]
   [bg1 _GdkColor]
   [bg2 _GdkColor]
   [bg3 _GdkColor]
   [bg4 _GdkColor]
   [bg5 _GdkColor]
   [light1 _GdkColor]
   [light2 _GdkColor]
   [light3 _GdkColor]
   [light4 _GdkColor]
   [light5 _GdkColor]
   [dark1 _GdkColor]
   [dark2 _GdkColor]
   [dark3 _GdkColor]
   [dark4 _GdkColor]
   [dark5 _GdkColor]
   [mid1 _GdkColor]
   [mid2 _GdkColor]
   [mid3 _GdkColor]
   [mid4 _GdkColor]
   [mid5 _GdkColor]
   [text1 _GdkColor]
   [text2 _GdkColor]
   [text3 _GdkColor]
   [text4 _GdkColor]
   [text5 _GdkColor]
   [base1 _GdkColor]
   [base2 _GdkColor]
   [base3 _GdkColor]
   [base4 _GdkColor]
   [base5 _GdkColor]
   [text_aa1 _GdkColor]
   [text_aa2 _GdkColor]
   [text_aa3 _GdkColor]
   [text_aa4 _GdkColor]
   [text_aa5 _GdkColor]
   [black _GdkColor]
   [white _GdkColor]
   [font_desc _pointer] ; PangoFontDescription *
   ; ...
   ))

(define-gtk gtk_widget_get_style (_fun  _GtkWidget -> _GtkStyle-pointer))
(define-gtk gtk_rc_get_style (_fun  _GtkWidget -> _GtkStyle-pointer))
(define-gtk gtk_text_view_new (_fun -> _GtkWidget))
(define-gtk gtk_widget_destroy (_fun _GtkWidget -> _void))

(define the-text-style #f)
(define (update-the-text-style!)
  (when the-text-style (g_object_unref the-text-style))
  (define w (gtk_text_view_new))
  (define style (gtk_rc_get_style w))
  (g_object_ref style)
  (set! the-text-style style)
  (gtk_widget_destroy w)
  (void))
(update-the-text-style!)

(define (extract-color-values c)
  (define (s v) (arithmetic-shift v -8))
  (values (s (GdkColor-red c))
          (s (GdkColor-green c))
          (s (GdkColor-blue c))))

(define (get-selected-text-color)
  (extract-color-values (GtkStyle-text4 the-text-style)))
  
(define (get-selected-background-color)
  (extract-color-values (GtkStyle-base4 the-text-style)))

(define (get-label-fg-color)
  (extract-color-values (GtkStyle-text1 the-text-style)))

(define (get-label-bg-color)
  (extract-color-values (GtkStyle-bg1 the-text-style)))

(define _GtkSettings (_cpointer 'GtkSettings))
(define-gtk gtk_settings_get_default (_fun -> _GtkSettings))
(define-gobj g_object_get/string (_fun _GtkSettings _string (r : (_ptr o _pointer)) (_pointer = #f)
                                       -> _void
                                       -> r)
  #:c-id g_object_get)
(define-signal-handler connect-dark "notify::gtk-theme-name"
  (_fun _GtkSettings -> _void)
  (λ (gtk)
    (define new-dark? (calculate-dark?))
    (unless (equal? new-dark? is-dark?)
      (set! is-dark? new-dark?)
      (update-the-text-style!)
      (tell-all-frames-request-refresh-all-canvas-children)
      (queue-dark-mode-event))))
(void (connect-dark (cast (gtk_settings_get_default) _GtkSettings _GtkWidget)))

(define (calculate-dark?)
  (define s (gtk_settings_get_default))
  (define th (g_object_get/string s "gtk-theme-name"))
  (define dark? (regexp-match? #rx"(^|-)dark(-|$)" (cast th _pointer _string)))
  (g_free th)
  dark?)
(define is-dark? (calculate-dark?))
(define (dark?) is-dark?)
