#lang racket/base
(require ffi/unsafe)

(provide gtk3?
	 get-gdk3-lib
	 get-gtk3-lib)

(define (get-gdk3-lib)
  (ffi-lib "libgdk-3" '("0" "") #:fail (lambda () #f)))
(define (get-gtk3-lib)
  ;; Open in "global" mode so that gtk_print_operation_run()
  ;; can find the printer dialog using _g_module_symbol():
  (ffi-lib "libgtk-3" '("0" "") #:global? #t #:fail (lambda () #f)))

(define gtk3?
  (and (not (getenv "PLT_GTK2"))
       (get-gdk3-lib)
       (get-gtk3-lib)))
