#lang racket/base
(require ffi/unsafe)

(provide gtk3?
	 get-gdk3-lib
	 get-gtk3-lib)

(define (get-gdk3-lib)
  (ffi-lib "libgdk-3" '("0" "")))
(define (get-gtk3-lib)
  (ffi-lib "libgtk-3" '("0" "")))

(define gtk3?
  (and (not (getenv "PLT_GTK2"))
       (get-gdk3-lib)
       (get-gtk3-lib)))
