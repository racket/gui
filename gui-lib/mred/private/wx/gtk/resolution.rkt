#lang racket/base
(require racket/promise
	 ffi/unsafe
	 "gsettings.rkt")

(provide get-interface-scale-factor)

(define (get-interface-scale-factor display-num)
  (or (get-gnome-interface-scale-factor)
      1.0))

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
      (* (g_variant_get_uint32
	  (g_settings_get_value gs "scaling-factor"))
	 (g_variant_get_double
	  (g_settings_get_value gs "text-scaling-factor"))))
    (g_object_unref gs)
    v))
