#lang racket/base
(require racket/promise
	 ffi/unsafe
	 "gsettings.rkt"
	 "gtk3.rkt")

(provide get-interface-scale-factor)

(define (get-interface-scale-factor display-num)
  (or (get-environment-variable-scale-factor)
      (get-gnome-interface-scale-factor)
      1.0))

(define (get-environment-variable-scale-factor)
  (define s (getenv "PLT_DISPLAY_BACKING_SCALE"))
  (define n (and s (string->number s)))
  (and (rational? n)
       (positive? n)
       (exact->inexact n)))

(define interface-settings
  (let ([interface-schema "org.gnome.desktop.interface"])
    (delay
      (and (g_settings_schema_source_lookup
	    (g_settings_schema_source_get_default)
	    interface-schema
	    #f)
	   (let* ([gs (g_settings_new interface-schema)]
		  [keys (g_settings_list_keys gs)])
	     (define (check s)
	       (for/or ([i (in-naturals)]
		        #:break (not (ptr-ref keys _pointer i)))
		  (equal? s (ptr-ref keys _string i))))
	     (and (check "scaling-factor")
		  (check "text-scaling-factor")
		  gs))))))

(define (get-gnome-interface-scale-factor)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (define gs (force interface-settings))
    (define v
      (* (if gtk3?
	     ;; For Gtk3, toolbox handles this scaling:
	     1
	     ;; For Gtk2, we can handle explicit settings,
	     ;; but we don't try to infer a scale if the
	     ;; setting is 0:
	     (max 1
		  (g_variant_get_uint32
		   (g_settings_get_value gs "scaling-factor"))))
	 (g_variant_get_double
	  (g_settings_get_value gs "text-scaling-factor"))))
    (and (rational? v)
	 (positive? v)
	 v)))
