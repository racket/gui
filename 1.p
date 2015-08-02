From 05a142ad286c1ae66add0e2a02a33d8080c089a6 Mon Sep 17 00:00:00 2001
From: Matthew Flatt <mflatt@racket-lang.org>
Date: Sat, 1 Aug 2015 19:03:01 -0600
Subject: [PATCH] fix HiDPI detection

Work (in unscaled mode) on systems where the relevant
setting is not available.
---
 gui-lib/mred/private/wx/gtk/gsettings.rkt  |    1 +
 gui-lib/mred/private/wx/gtk/resolution.rkt |   33 +++++++++++++++++++++-------
 gui-lib/mred/private/wx/gtk/utils.rkt      |   14 +++++++++---
 3 files changed, 37 insertions(+), 11 deletions(-)

diff --git a/gui-lib/mred/private/wx/gtk/gsettings.rkt b/gui-lib/mred/private/wx/gtk/gsettings.rkt
index bd65de9..812f03d 100644
--- a/gui-lib/mred/private/wx/gtk/gsettings.rkt
+++ b/gui-lib/mred/private/wx/gtk/gsettings.rkt
@@ -34,6 +34,7 @@
 (define-gio g_settings_new (_fun _string -> _GSettings-pointer)
   #:wrap (allocator g_object_unref))
 (define-gio g_settings_get_value (_fun _GSettings-pointer _string -> _GVariant-pointer))
+(define-gio g_settings_list_keys (_fun _GSettings-pointer -> _pointer))
 
 (define-glib g_variant_get_type_string (_fun _GVariant-pointer -> _string))
 (define-glib g_variant_get_int32 (_fun _GVariant-pointer -> _int32))
diff --git a/gui-lib/mred/private/wx/gtk/resolution.rkt b/gui-lib/mred/private/wx/gtk/resolution.rkt
index de935e1..6132477 100644
--- a/gui-lib/mred/private/wx/gtk/resolution.rkt
+++ b/gui-lib/mred/private/wx/gtk/resolution.rkt
@@ -1,17 +1,34 @@
 #lang racket/base
-(require "gsettings.rkt")
+(require racket/promise
+	 ffi/unsafe
+	 "gsettings.rkt")
 
 (provide get-interface-scale-factor)
 
-
 (define (get-interface-scale-factor display-num)
+  (or (get-gnome-interface-scale-factor)
+      1.0))
+
+(define interface-settings
+  (let ([interface-schema "org.gnome.desktop.interface"])
+    (delay
+      (and (g_settings_schema_source_lookup
+	    (g_settings_schema_source_get_default)
+	    interface-schema
+	    #f)
+	   (let* ([gs (g_settings_new interface-schema)]
+		  [keys (g_settings_list_keys)])
+	     (define (check s)
+	       (for ([i (in-naturals)]
+		     #:break (not (ptr-ref keys _pointer i)))
+		    (equal? s (ptr-ref keys _string i))))
+	     (and (check "scaling-factor")
+		  (check "text-scaling-factor")
+		  gs))))))
+
+(define (get-gnome-interface-scale-factor)
   (with-handlers ([exn:fail? (lambda (exn) #f)])
-    (define schema "org.gnome.desktop.interface")
-    (define gs (and (g_settings_schema_source_lookup
-		     (g_settings_schema_source_get_default)
-		     schema
-		     #f)
-		    (g_settings_new schema)))
+    (define gs (force interface-settings))
     (define v
       (* (g_variant_get_uint32
 	  (g_settings_get_value gs "scaling-factor"))
diff --git a/gui-lib/mred/private/wx/gtk/utils.rkt b/gui-lib/mred/private/wx/gtk/utils.rkt
index a48b5ba..f2b19a9 100644
--- a/gui-lib/mred/private/wx/gtk/utils.rkt
+++ b/gui-lib/mred/private/wx/gtk/utils.rkt
@@ -1,5 +1,6 @@
 #lang racket/base
-(require ffi/unsafe
+(require racket/promise
+	 ffi/unsafe
          ffi/unsafe/define
          ffi/unsafe/alloc
          racket/string
@@ -203,10 +204,13 @@
 
 ;; ----------------------------------------
 
-(define screen-scale-factor
-  (inexact->exact (get-interface-scale-factor 0)))
+(define screen-scale-factor/promise
+  (delay
+    (inexact->exact (get-interface-scale-factor 0))))
 
 (define (->screen x)
+  (define screen-scale-factor
+    (force screen-scale-factor/promise))
   (and x
        (if (= screen-scale-factor 1)
 	   x
@@ -214,12 +218,16 @@
 	       (ceiling (* x screen-scale-factor))
 	       (* x screen-scale-factor)))))
 (define (->screen* x)
+  (define screen-scale-factor
+    (force screen-scale-factor/promise))
   (if (and (not (= screen-scale-factor 1))
 	   (exact? x))
       (floor (* x screen-scale-factor))
       (->screen x)))
 
 (define (->normal x)
+  (define screen-scale-factor
+    (force screen-scale-factor/promise))
   (and x
        (if (= screen-scale-factor 1)
 	   x
-- 
1.7.9.5

