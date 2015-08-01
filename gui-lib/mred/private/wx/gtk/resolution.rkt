#lang racket/base
(require "gsettings.rkt")

(provide get-interface-scale-factor)


(define (get-interface-scale-factor display-num)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (define schema "org.gnome.desktop.interface")
    (define gs (and (g_settings_schema_source_lookup
		     (g_settings_schema_source_get_default)
		     schema
		     #f)
		    (g_settings_new schema)))
    (define v
      (* (g_variant_get_uint32
	  (g_settings_get_value gs "scaling-factor"))
	 (g_variant_get_double
	  (g_settings_get_value gs "text-scaling-factor"))))
    (g_object_unref gs)
    v))
