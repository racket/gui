#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
	 ffi/unsafe/alloc
	 racket/draw/unsafe/glib
	 racket/draw/private/libs
	 "types.rkt")

(provide (protect-out (all-defined-out)))

(define-runtime-lib gio-lib
  [(unix) (ffi-lib "libgio-2.0" '("0" "")
                   ;; For old glib, libgio isn't separate;
                   ;; try to find bindings in already-loaded
                   ;; libraries:
                   #:fail (lambda () #f))]
  [(macosx)
   (ffi-lib "libgio-2.0.0.dylib")]
  [(windows) 
   (ffi-lib "libgio-2.0-0.dll")])

(define-ffi-definer define-gio gio-lib
  #:default-make-fail (lambda (id) (lambda () #f)))

(define _GSettings-pointer (_cpointer 'GSettings))
(define _GSettingsSchemeSource-pointer (_cpointer 'GSettingsSchemeSource))
(define _GVariant-pointer (_cpointer 'GVariant))

(define-gobj g_object_unref (_fun _pointer -> _void)
  #:wrap (deallocator))

(define-gio g_settings_schema_source_get_default (_fun -> _GSettingsSchemeSource-pointer))
(define-gio g_settings_schema_source_lookup (_fun _GSettingsSchemeSource-pointer
						  _string
						  _gboolean
						  -> _pointer)) 

(define-gio g_settings_new (_fun _string -> _GSettings-pointer)
  #:wrap (allocator g_object_unref))
(define-gio g_settings_get_value (_fun _GSettings-pointer _string -> _GVariant-pointer))
(define-gio g_settings_list_keys (_fun _GSettings-pointer -> _pointer))

(define-glib g_variant_get_type_string (_fun _GVariant-pointer -> _string)
  #:make-fail make-not-available)
(define-glib g_variant_get_int32 (_fun _GVariant-pointer -> _int32)
  #:make-fail make-not-available)
(define-glib g_variant_get_uint32 (_fun _GVariant-pointer -> _uint32)
  #:make-fail make-not-available)
(define-glib g_variant_get_double (_fun _GVariant-pointer -> _gdouble)
  #:make-fail make-not-available)
