#lang racket/base
(require racket/class
         racket/promise
         racket/string
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         ffi/unsafe/atomic
         ffi/cvector
         (prefix-in draw: racket/draw/private/gl-context)
         racket/draw/private/gl-config
         "../../lock.rkt"
         "types.rkt"
         "utils.rkt"
         "widget.rkt"
         "window.rkt"
         "x11.rkt"
	 "queue.rkt"
	 "wayland.rkt"
	 "gl-cairo.rkt")

(provide
 (protect-out prepare-widget-gl-context
              create-widget-gl-context

              create-and-install-gl-context
              get-gdk-pixmap
              install-gl-context
	      gl-update-size
	      gl-to-cairo-sync
	      gl-reset-context))

(define (ffi-lib/complaint-on-failure name vers)
  (ffi-lib name vers
           #:fail (lambda ()
                    (log-warning "could not load library ~a ~a"
                                 name vers)
                    #f)))

(define-local-member-name
  gl-update-size
  gl-to-cairo-sync
  gl-reset-context)

;; ===================================================================================================
;; Wayland GL

(define egl-lib
  (and wayland? (ffi-lib "libEGL" '("1" ""))))
(define wayland-egl-lib
  (and wayland? (ffi-lib "libwayland-egl" '("1" ""))))

(define-ffi-definer define-egl egl-lib
  #:default-make-fail make-not-available)
(define-ffi-definer define-wayland-egl wayland-egl-lib
  #:default-make-fail make-not-available)

(define-gdk gdk_wayland_window_get_wl_surface
  (_fun _GdkWindow -> _pointer)
  #:fail (lambda () #f))

(define-wayland-egl wl_egl_window_create
  (_fun _pointer _int _int -> _pointer))
(define-wayland-egl wl_egl_window_resize
  (_fun _pointer _int _int _int _int -> _void))
(define-wayland-egl wl_egl_window_destroy
  (_fun _pointer -> _void))

(define _EGLInt _int32)
(define _EGLBoolean _bool) ; not _stdbool
(define _EGLDisplay (_cpointer/null 'EGLDisplay))
(define _EGLConfig (_cpointer/null 'EGLConfig))
(define _EGLSurface (_cpointer/null 'EGLSurface))
(define _EGLContext (_cpointer/null 'EGLContext))

(define-egl eglGetProcAddress
  (_fun _string -> _fpointer))
(define eglGetPlatformDisplay-type
  (_fun _EGLInt _pointer (_list i _EGLInt) -> _EGLDisplay))
(define-egl eglGetPlatformDisplay eglGetPlatformDisplay-type)
(define-egl eglInitialize
  (_fun _pointer (_ptr o _int) (_ptr o _int) -> _EGLBoolean))
(define-egl eglChooseConfig
  (_fun _EGLDisplay (_list i _EGLInt) (c : (_ptr o _EGLConfig)) (_int = 1) (n : (_ptr o _EGLInt))
	-> (r : _EGLBoolean)
	-> (and r (= n 1) c)))
(define eglCreatePlatformWindowSurface-type
  (_fun _EGLDisplay _EGLConfig _pointer (_list i _int) -> _EGLSurface))
(define-egl eglCreatePlatformWindowSurface
  eglCreatePlatformWindowSurface-type)
(define-egl eglBindAPI
  (_fun _EGLInt -> _EGLBoolean))
(define-egl eglCreateContext
  (_fun _EGLDisplay _EGLConfig _EGLContext (_list i _EGLInt) -> _EGLContext))
(define-egl eglMakeCurrent
  (_fun _EGLDisplay _EGLSurface _EGLSurface _EGLContext -> _EGLBoolean))
(define-egl eglSwapBuffers
  (_fun _EGLDisplay _EGLSurface -> _EGLBoolean))
(define-egl eglCreatePbufferSurface
  (_fun _EGLDisplay _EGLConfig (_list i _EGLInt) -> _EGLSurface))
(define-egl eglGetError
  (_fun -> _EGLInt))
(define-egl eglDestroySurface
  (_fun _EGLDisplay _EGLSurface -> _int))
(define-egl eglDestroyContext
  (_fun _EGLDisplay _EGLContext -> _EGLBoolean))
(define-egl eglGetCurrentDisplay
  (_fun -> _EGLDisplay))
(define-egl eglGetCurrentContext
  (_fun -> _EGLContext))
(define-egl eglGetCurrentSurface
  (_fun _EGLInt -> _EGLSurface))

(define-gdk gdk_wayland_display_get_wl_display (_fun _GdkDisplay -> _pointer)
  #:make-fail make-not-available)
(define-gdk gdk_wayland_display_get_wl_compositor (_fun _GdkDisplay -> _pointer)
  #:make-fail make-not-available)

(define EGL_OPENGL_API #x30A2)
(define EGL_SURFACE_TYPE #x3033)
(define EGL_WINDOW_BIT #x0004)
(define EGL_PBUFFER_BIT #x0001)
(define EGL_RENDERABLE_TYPE #x3040)
(define EGL_RENDER_BUFFER #x3086)
(define EGL_BACK_BUFFER #x3084)
(define EGL_SINGLE_BUFFER #x3085)
(define EGL_OPENGL_BIT #x0008)
(define EGL_OPENGL_ES_BIT #x0001)
(define EGL_OPENGL_ES2_BIT #x0004)
(define EGL_DEPTH_SIZE #x3025)
(define EGL_STENCIL_SIZE #x3026)
(define EGL_RED_SIZE #x3024)
(define EGL_GREEN_SIZE #x3023)
(define EGL_BLUE_SIZE #x3022)
(define EGL_ALPHA_SIZE #x3021)
(define EGL_NONE #x3038)
(define EGL_WIDTH #x3057)
(define EGL_HEIGHT #x3056)
(define EGL_NO_SURFACE #f)
(define EGL_CONTEXT_CLIENT_VERSION #x3098)
(define EGL_CONTEXT_MAJOR_VERSION #x3098)
(define EGL_CONTEXT_MINOR_VERSION #x30FB)
(define EGL_CONTEXT_OPENGL_PROFILE_MASK #x30FD)
(define EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT #x00000001)
(define EGL_PLATFORM_WAYLAND_KHR #x31D8)
(define EGL_DRAW #x3059)
(define EGL_READ #x305A)

;; ===================================================================================================
;; X11/GLX FFI

(define gl-lib (ffi-lib/complaint-on-failure "libGL" '("1" "")))

(define-ffi-definer define-glx gl-lib
  #:default-make-fail make-not-available)

;; X #defines/typedefs/enums
(define _Display (_cpointer 'Display))
(define _XErrorEvent (_cpointer 'XErrorEvent))
(define _XID _ulong)
(define True 1)
(define False 0)
(define None 0)
(define Success 0)

;; GLX #defines/typedefs/enums
(define _GLXFBConfig (_cpointer 'GLXFBConfig))
(define _GLXContext (_cpointer/null 'GLXContext))
(define _XVisualInfo (_cpointer 'XVisualInfo))
;; Attribute tokens for glXGetConfig variants (all GLX versions):
(define GLX_DOUBLEBUFFER     5)
(define GLX_STEREO           6)
(define GLX_DEPTH_SIZE       12)
(define GLX_STENCIL_SIZE     13)
(define GLX_ACCUM_RED_SIZE   14)
(define GLX_ACCUM_GREEN_SIZE 15)
(define GLX_ACCUM_BLUE_SIZE  16)
(define GLX_ACCUM_ALPHA_SIZE 17)
;; GLX 1.3 and later:
(define GLX_X_RENDERABLE     #x8012)
(define GLX_RGBA_TYPE        #x8014)
;; GLX 1.4 and later:
(define GLX_SAMPLES          #x186a1)
(define GLX_SAMPLE_BUFFERS   #x186a0)
;; Attribute tokens for glXCreateContextAttribsARB (also GLX 1.4 and later):
(define GLX_CONTEXT_MAJOR_VERSION_ARB #x2091)
(define GLX_CONTEXT_MINOR_VERSION_ARB #x2092)
(define GLX_CONTEXT_FLAGS_ARB         #x2094)
(define GLX_CONTEXT_PROFILE_MASK_ARB  #x9126)
;; GLX_CONTEXT_FLAGS_ARB bits
(define GLX_CONTEXT_DEBUG_BIT_ARB              #x1)
(define GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB #x2)
;; GLX_CONTEXT_PROFILE_MASK_ARB bits
(define GLX_CONTEXT_CORE_PROFILE_BIT_ARB          #x1)
(define GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB #x2)

(define-x11 XFree (_fun _pointer -> _int)
  #:wrap (deallocator))

(define-x11 XSetErrorHandler
  (_fun _fpointer -> _fpointer))

(define-x11 XSync
  (_fun _Display _int -> _void))

(define-glx glXQueryVersion
  (_fun _Display (major : (_ptr o _int)) (minor : (_ptr o _int))
        -> (ret : _bool)
        -> (values ret major minor)))

(define-glx glXQueryExtensionsString
  (_fun _Display _int -> _string/utf-8))

(define-glx glXChooseFBConfig
  (_fun _Display _int (_list i _int) (len : (_ptr o _int))
        -> (_cvector o _GLXFBConfig len))
  #:wrap (allocator (λ (v) (XFree (cvector-ptr v)))))

(define-glx glXGetFBConfigAttrib
  (_fun _Display _GLXFBConfig _int (out : (_ptr o _int))
        -> (ret : _int)
        -> (values ret out)))

(define-glx glXCreateNewContext
  (_fun _Display _GLXFBConfig _int _GLXContext _bool -> _GLXContext))

(define-glx glXDestroyContext
  (_fun _Display _GLXContext -> _void))

(define-glx glXMakeCurrent
  (_fun _Display _XID _GLXContext -> _bool))

(define-glx glXSwapBuffers
  (_fun _Display _XID -> _void))

(define-glx glXIsDirect
  (_fun _Display _GLXContext -> _bool))

(define-glx glXGetVisualFromFBConfig
  (_fun _Display _GLXFBConfig -> _XVisualInfo)
  #:wrap (allocator XFree))

(define-glx glXCreateGLXPixmap
  (_fun _Display _XVisualInfo _XID -> _XID))

(define-glx glXDestroyGLXPixmap
  (_fun _Display _XID -> _void))

(define-glx glXGetProcAddressARB
  (_fun _string -> _pointer))

(define lazy-glXCreateContextAttribsARB
  (delay
    (function-ptr (glXGetProcAddressARB "glXCreateContextAttribsARB")
                  (_fun _Display _GLXFBConfig _GLXContext _bool (_list i _int)
                        -> _GLXContext))))

(define (glXCreateContextAttribsARB . args)
  (apply (force lazy-glXCreateContextAttribsARB) args))

(define-gtk gtk_widget_get_display (_fun _GtkWidget -> _GdkDisplay))
(define-gtk gtk_widget_get_screen (_fun _GtkWidget -> _GdkScreen))

(define-glx glXSwapIntervalEXT (_fun _Display _XID _int -> _void)
  #:fail (lambda () void))

;; ===================================================================================================
;; GLX versions and extensions queries

(define lazy-get-glx-version
  (delay
    (when wayland?
      (error 'get-glx-version "can't use GLX on Wayland"))

    (define-values (worked? glx-major glx-minor)
      (glXQueryVersion (gdk_x11_display_get_xdisplay (gdk_display_get_default))))
    
    (unless worked?
      (error 'get-glx-version "can't get GLX version using default display"))
    
    (define glx-version (+ glx-major (/ glx-minor 10)))
    
    (when (< glx-version #e1.3)
      (error 'get-glx-version "need GLX version 1.3 or greater; given version ~a.~a"
             glx-major glx-minor))
    
    glx-version))

;; -> positive-exact-rational
(define (get-glx-version)
  (force lazy-get-glx-version))

(define lazy-glx-extensions
  (delay
    (define str
      (glXQueryExtensionsString (gdk_x11_display_get_xdisplay (gdk_display_get_default))
                                (gdk_x11_screen_get_screen_number (gdk_screen_get_default))))
    (string-split str)))

(define lazy-GLX_ARB_create_context?
  (delay (member "GLX_ARB_create_context"
                 (force lazy-glx-extensions))))

(define lazy-GLX_ARB_create_context_profile?
  (delay (member "GLX_ARB_create_context_profile"
                 (force lazy-glx-extensions))))

;; ===================================================================================================
;; Wrapper for the _GLXContext (if we can get one from GLX)

(define gl-context% 
  (class draw:gl-context%
    (init-field gl display drawable pixmap)
    
    (define/override (get-handle) gl)
    
    (define/public (get-gtk-display) display)
    (define/public (get-gtk-drawable) drawable)
    (define/public (get-glx-pixmap) pixmap)
    
    (define/private (get-drawable-xid)
      (if pixmap pixmap (gdk_x11_drawable_get_xid drawable)))

    (define/public (gl-reset-context mapped?) (void))
    
    (define/override (draw:do-call-as-current t)
      (define xdisplay (gdk_x11_display_get_xdisplay display))
      (dynamic-wind
       (lambda ()
         (glXMakeCurrent xdisplay (get-drawable-xid) gl))
       t
       (lambda ()
         (glXMakeCurrent xdisplay 0 #f))))
    
    (define/override (draw:do-swap-buffers)
      (glXSwapBuffers (gdk_x11_display_get_xdisplay display)
                      (get-drawable-xid))
      (void))

    (define/public (gl-update-size x y w h)
      (void))

    (super-new)))

;; ===================================================================================================
;; Wrapper for EGLContext (Wayland)

(define-local-member-name
  egl-finalize)

(define egl-context%
  (class draw:gl-context%
    (init-field context
		display wl-display
		surface [wl-surface #f] [wl-parent-surface #f] [wl-subsurface #f]
		[widget #f] [win #f]
		[cairo-surface #f] [texture-sync #f]
		[create #f])

    (define/public (gl-reset-context mapped?)
      (when (and create
		 (or (and (not mapped?) wl-parent-surface)
		     (and mapped? (not wl-parent-surface))))
	(egl-finalize #f)
	(unless mapped?
	  (set! callback #f)
	  (set! callback-handle #f))
	(define-values (new-surface new-wl-surface new-wl-parent-surface new-wl-subsurface new-win)
	  (create #t))
	(set! surface new-surface)
	(set! wl-surface new-wl-surface)
	(set! wl-parent-surface new-wl-parent-surface)
	(set! wl-subsurface new-wl-subsurface)
	(set! win new-win)))

    (define/public (egl-finalize [including-context? #t])
      ;; If there's a parent surface, it will destroy the
      ;; subsurface when it's destroyed
      (unless wl-parent-surface
	(wayland-surface-destroy wl-surface))
      (eglDestroySurface display surface)
      (wl_egl_window_destroy win)
      (when including-context?
	(eglDestroyContext display context)))
    (define/override (get-handle) context)

    (define cairo-mode? #t)

    (define/override (draw:do-call-as-current t)
      (dynamic-wind
	  (lambda ()
	    (eglMakeCurrent display surface surface context))
	  (lambda ()
	    (when (and texture-sync cairo-mode?)
	      (set! cairo-mode? #f)
	      (update-texture-from-cairo texture-sync cairo-surface))
	    (t))
	  (lambda ()
	    (eglMakeCurrent display #f #f #f))))

    (define/public (gl-to-cairo-sync)
      (unless cairo-mode?
	(call-as-atomic
	 (lambda ()
	   (set! cairo-mode? #t)
	   (call-with-egl-current
	    display surface surface context
	    (lambda ()
	      (update-cairo-from-texture texture-sync cairo-surface)))))))

    (define callback #f)
    (define callback-handle #f)
 
    (define/override (draw:do-swap-buffers)
      (when wl-surface
	(unless callback
	  (eglSwapBuffers display surface)
	  (set! callback (lambda (data callback-h time)
			   (set! callback #f)
			   (gtk_widget_queue_draw widget)))
	  (set! callback-handle (wayland-register-surface-frame-callback wl-surface callback))
	  (wayland-surface-commit wl-surface))))

    (define/public (gl-update-size x y w h)
      (when win
	(wl_egl_window_resize win w h 0 0)
	(when (and widget wl-subsurface)
	  (define toplevel (gtk_widget_get_toplevel widget))
	  (define-values (dx dy)
	    (gtk_widget_translate_coordinates widget toplevel 0 0))
	  (wayland-subsurface-set-position wl-subsurface dx dy))))

    (super-new)))

(define (call-with-egl-current display d-surface r-surface context thunk)
  (define current (list (eglGetCurrentDisplay)
			(eglGetCurrentSurface EGL_DRAW)
			(eglGetCurrentSurface EGL_READ)
			(eglGetCurrentContext)))
  (eglMakeCurrent display d-surface r-surface context)
  (define result (thunk))
  (apply eglMakeCurrent current)
  result)

;; ===================================================================================================
;; Getting OpenGL contexts

;; STUPIDITY ALERT

;; Apparently, the designers of glXCreateNewContext and glXCreateContextAttribsARB didn't trust us to
;; check return values or output arguments, so when these functions fail, they raise an X error and
;; send an error code to the X error handler. X errors, by default, *terminate the program* and print
;; an annoyingly vague, barely helpful error message.

;; This is especially bad with glXCreateContextAttribsARB, which always fails (i.e. crashes the
;; program) if we ask for an unsupported OpenGL version. Worse, this is the only way to find out
;; which OpenGL versions are available!

;; So we override the X error handler to silently fail, and sync right after the calls to make sure
;; the errors are processed immediately. With glXCreateContextAttribsARB, we then try the next lowest
;; OpenGL version. If all attempts to get a context fail, we return #f.

(define create-context-error? #f)
(define (flag-x-error-handler xdisplay xerrorevent)
  (set! create-context-error? #t)
  0)

;; _Display _GLXFBConfig _GLXContext -> _GLXContext
(define (glx-create-new-context xdisplay cfg share-gl)
  ;; Sync right now, or the sync further on could crash Racket with an [xcb] error about events
  ;; happening out of sequence
  (XSync xdisplay False)

  (define old-handler #f)
  (define gl
    (dynamic-wind
     (λ ()
       (set! old-handler
             (XSetErrorHandler
              (cast flag-x-error-handler
                    (_fun #:atomic? #t _Display _XErrorEvent -> _int)
                    _fpointer))))
     (λ ()
       (set! create-context-error? #f)
       (glXCreateNewContext xdisplay cfg GLX_RGBA_TYPE share-gl #t))
     (λ ()
       ;; Sync to ensure errors are processed
       (XSync xdisplay False)
       (XSetErrorHandler old-handler))))

  (cond
    [(and gl create-context-error?)
     (log-error (string-append
		 "gl-context: glXCreateNewContext raised an error but (contrary to standards)"
		 " returned a non-NULL context; ignoring possibly corrupt context"))
     #f]
    [else
     (unless gl
       (log-warning "gl-context: glXCreateNewContext was unable to get an OpenGL context"))
     gl]))

;; OpenGL core versions we'll try to get, in order
(define core-gl-versions '((4 5) (4 4) (4 3) (4 2) (4 1) (4 0) (3 3) (3 2) (3 1) (3 0)))

;; _Display _GLXFBConfig _GLXContext (List Byte Byte) -> _GLXContext
(define (glx-create-context-attribs xdisplay cfg share-gl gl-version)
  ;; Sync right now, or the sync further on could crash Racket with an [xcb] error about events
  ;; happening out of sequence
  (XSync xdisplay False)

  (define gl-major (car gl-version))
  (define gl-minor (cadr gl-version))
  (define context-attribs
    (list GLX_CONTEXT_MAJOR_VERSION_ARB gl-major
          GLX_CONTEXT_MINOR_VERSION_ARB gl-minor
          GLX_CONTEXT_PROFILE_MASK_ARB GLX_CONTEXT_CORE_PROFILE_BIT_ARB
          None))
  
  (define old-handler #f)
  (define gl
    (dynamic-wind
     (λ ()
       (set! old-handler
             (XSetErrorHandler
              (cast flag-x-error-handler
                    (_fun #:atomic? #t _Display _XErrorEvent -> _int)
                    _fpointer))))
     (λ ()
       (set! create-context-error? #f)
       (glXCreateContextAttribsARB xdisplay cfg share-gl #t context-attribs))
     (λ ()
       ;; Sync to ensure errors are processed
       (XSync xdisplay False)
       (XSetErrorHandler old-handler))))
  
  (cond
    [(and gl create-context-error?)
     (log-error (string-append
		 "gl-context: glXCreateContextAttribsARB raised an error for version ~a.~a but"
		 " (contrary to standards) returned a non-NULL context;"
		 " ignoring possibly corrupt context")
                gl-major gl-minor)
     #f]
    [else
     (unless gl
       (log-info "gl-context: glXCreateContextAttribsARB returned NULL for version ~a.~a"
                 gl-major gl-minor))
     gl]))

;; _Display _GLXFBConfig _GLXContext -> _GLXContext
(define (glx-create-core-context xdisplay cfg share-gl)
  (let/ec return
    (for ([gl-version  (in-list core-gl-versions)])
      (define gl (glx-create-context-attribs xdisplay cfg share-gl gl-version))
      (when gl (return gl)))
    (log-warning "gl-context: unable to get core context; falling back")
    (glx-create-new-context xdisplay cfg share-gl)))

;; ===================================================================================================

;; (or/c #f _GtkWidget) -> _GdkDisplay
(define (gtk-maybe-widget-get-display widget)
  (cond [widget  (gtk_widget_get_display widget)]
        [else    (gdk_display_get_default)]))

;; (or/c #f _GtkWidget) -> _GdkScreen
(define (gtk-maybe-widget-get-screen widget)
  (cond [widget  (gtk_widget_get_screen widget)]
        [else    (gdk_screen_get_default)]))

;; _Display _GLXFBConfig int int -> int
(define (glx-get-fbconfig-attrib xdisplay cfg attrib bad-value)
  (define-values (err value) (glXGetFBConfigAttrib xdisplay cfg attrib))
  (if (= err Success) value bad-value))

;; (or/c #f _GtkWidget) (or/c _GdkDrawable (is-a/c bitmap%) gl-config% boolean? -> gl-context%
;;   where X11 uses _GdkDrawable = (or/c _GtkWindow _GdkPixmap)
;;         and Wayland uses bitmap% that holds a Cairo ARGB32 image surface
(define (make-gtk-drawable-gl-context widget drawable conf wants-double?)
  (cond
   [wayland?
    (define gdk-display (gtk-maybe-widget-get-display widget))
    (define wl-display (gdk_wayland_display_get_wl_display gdk-display))

    (define eglGetPlatformDisplayEXT-addr
      (eglGetProcAddress "eglGetPlatformDisplayEXT"))
    (unless eglGetPlatformDisplayEXT-addr
      (error 'EGL "could not get eglGetPlatformDisplayEXT"))
    (define display ((cast eglGetPlatformDisplayEXT-addr _fpointer eglGetPlatformDisplay-type)
		     EGL_PLATFORM_WAYLAND_KHR wl-display (list EGL_NONE)))

    (unless (eglInitialize display)
      (error 'EGL "initialization failed"))

    (define accum-size (send conf get-accum-size))
    (define attribs (list
		     EGL_SURFACE_TYPE EGL_WINDOW_BIT
		     EGL_RENDERABLE_TYPE EGL_OPENGL_BIT
		     EGL_DEPTH_SIZE (send conf get-depth-size)
		     EGL_STENCIL_SIZE (send conf get-stencil-size)
		     EGL_RED_SIZE accum-size
		     EGL_GREEN_SIZE accum-size
		     EGL_BLUE_SIZE accum-size
		     EGL_ALPHA_SIZE accum-size
		     EGL_NONE))
    (define config (or (eglChooseConfig display attribs)
		       (error 'EGL "configuration failed")))

    (unless (eglBindAPI EGL_OPENGL_API)
      (error 'EGL "API bind failed"))

    (define (make-context maj min)
      (define context-attribs (list
			       EGL_CONTEXT_MAJOR_VERSION maj
			       EGL_CONTEXT_MINOR_VERSION min
			       EGL_CONTEXT_OPENGL_PROFILE_MASK EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT
			       EGL_NONE))
      (eglCreateContext display config #f context-attribs))
    (define context (or (for/or ([ver (if (send conf get-legacy?)
					  '((2 1))
					  core-gl-versions)])
				(make-context (car ver) (cadr ver)))
			(error 'EGL "context failed")))

    (define (make-win-surface win)
      (define eglCreatePlatformWindowSurfaceEXT-addr
	(or (eglGetProcAddress "eglCreatePlatformWindowSurfaceEXT")
	    (error 'EGL "could not get eglCreatePlatformWindowSurfaceEXP")))
      (or ((cast eglCreatePlatformWindowSurfaceEXT-addr
		 _fpointer eglCreatePlatformWindowSurface-type)
	   display config win
	   (list
	    EGL_RENDER_BUFFER (if wants-double? EGL_BACK_BUFFER EGL_SINGLE_BUFFER)
	    EGL_NONE))
	  (error 'EGL "surface failed")))

    (define ctxt
      (cond
       [widget
	(define wl-compositor (gdk_wayland_display_get_wl_compositor gdk-display))
	(define wl-subcompositor (or (wayland-get-subcompositor wl-display)
				     (error 'EGL "subcompositor failed")))

	(define (create recreate?)
	  (define-values (width height)
	    (let ([a (widget-allocation widget)])
	      (values (GtkAllocation-width a)
		      (GtkAllocation-height a))))

	  (define wl-surface/sub (or (wayland-compositor-create-surface wl-compositor)
				     (error 'EGL "subsurface create failed")))

	  (define toplevel (gtk_widget_get_toplevel widget))
	  (define wl-surface (gdk_wayland_window_get_wl_surface
			      (widget-window widget)))
	  (define wl-subsurface (and wl-surface
				     (or (wayland-subcompositor-get-subsurface wl-subcompositor
									       wl-surface/sub
									       wl-surface)
					 (error 'EGL "subsurface failed"))))

	  (when wl-surface
	    (define-values (dx dy)
	      (gtk_widget_translate_coordinates widget toplevel 0 0))

	    (let ([region (wayland-compositor-create-region wl-compositor)])
	      (wayland-surface-set-input-region wl-surface/sub region)
	      (wayland-region-destroy region))
	    (wayland-subsurface-set-position wl-subsurface dx dy)
	    (wayland-subsurface-set-sync wl-subsurface #f)
	    (wayland-surface-commit wl-surface/sub)
	    (wayland-surface-commit wl-surface))

	  (define win (wl_egl_window_create wl-surface/sub width height))
	  (define surface (make-win-surface win))

	  (cond
	   [recreate?
	    (values surface wl-surface/sub wl-surface wl-subsurface win)]
	   [else
	    (new egl-context% [context context]
		 [display display] [wl-display wl-display]
		 [surface surface] [wl-surface wl-surface/sub] [wl-parent-surface wl-surface]
		 [wl-subsurface wl-subsurface]
		 [widget widget]
		 [win win]
		 [create create])]))
	(create #f)]
       [else
	(define width (send drawable get-width))
	(define height (send drawable get-height))

	(define wl-compositor (gdk_wayland_display_get_wl_compositor gdk-display))
	(define wl-surface (or (wayland-compositor-create-surface wl-compositor)
			       (error 'EGL "surface create failed")))
	(define win (wl_egl_window_create wl-surface width height))
	(define surface (make-win-surface win))

	(define texture-sync
	  (call-with-egl-current
	   display surface surface context
	   (lambda ()
	     (create-cairo-texture-sync width height))))		

	(new egl-context% [context context]
	     [display display] [wl-display wl-display]
	     [surface surface] [wl-surface wl-surface]
	     [win win]
	     [cairo-surface (send drawable get-handle)] [texture-sync texture-sync])]))
    (register-finalizer ctxt (λ (ctxt) (send ctxt egl-finalize)))
    ctxt]
   [else
    (define glx-version (get-glx-version))

    ;; If widget isn't #f, use its display and screen
    (define display (gtk-maybe-widget-get-display widget))
    (define screen (gtk-maybe-widget-get-screen widget))

    ;; Get the X objects wrapped by the GDK objects
    (define xdisplay (gdk_x11_display_get_xdisplay display))
    (define xscreen (gdk_x11_screen_get_screen_number screen))

    ;; Create an attribute list using the GL config
    (define xattribs
      (append
       ;; Be aware: we may get double buffering even if we don't ask for it
       (if wants-double?
           (if (send conf get-double-buffered) (list GLX_DOUBLEBUFFER True) null)
           null)
       (if (send conf get-stereo) (list GLX_STEREO True) null)
       ;; Finish out with standard GLX 1.3 attributes
       (list
	GLX_X_RENDERABLE True  ; yes, we want to use OpenGL to render today
	GLX_DEPTH_SIZE (send conf get-depth-size)
	GLX_STENCIL_SIZE (send conf get-stencil-size)
	GLX_ACCUM_RED_SIZE (send conf get-accum-size)
	GLX_ACCUM_GREEN_SIZE (send conf get-accum-size)
	GLX_ACCUM_BLUE_SIZE (send conf get-accum-size)
	GLX_ACCUM_ALPHA_SIZE (send conf get-accum-size)
	;; GLX_SAMPLES is handled below - GLX regards it as an absolute lower bound, which makes it
	;; too easy for user programs to fail to get a context
	None)))

    (define multisample-size (send conf get-multisample-size))

    ;; Get all framebuffer configs for this display and screen that match the requested attributes,
    ;; then sort them to put the best in front
    ;; GLX already sorts them pretty well, so we just need a stable sort on multisamples at the moment
    (define cfgs
      (let* ([cfgs  (cvector->list (glXChooseFBConfig xdisplay xscreen xattribs))]
             ;; Keep all configs with multisample size <= requested (i.e. make multisample-size an
             ;; abolute upper bound)
             [cfgs  (if (< glx-version #e1.4)
			cfgs
			(filter (λ (cfg)
                                  (define m (glx-get-fbconfig-attrib xdisplay cfg GLX_SAMPLES 0))
                                  (<= m multisample-size))
				cfgs))]
             ;; Sort all configs by multisample size, decreasing
             [cfgs  (if (< glx-version #e1.4)
			cfgs
			(sort cfgs >
                              #:key (λ (cfg) (glx-get-fbconfig-attrib xdisplay cfg GLX_SAMPLES 0))
                              #:cache-keys? #t))])
	cfgs))
 
    (cond
     [(null? cfgs)  #f]
     [else
      ;; The framebuffer configs are sorted best-first, so choose the first
      (define cfg (car cfgs))
      (define share-gl
	(let ([share-ctxt  (send conf get-share-context)])
          (and share-ctxt (send share-ctxt get-handle))))

      ;; Get a GL context
      (define gl
	(if (and (>= glx-version #e1.4)
                 (not (send conf get-legacy?))
                 (force lazy-GLX_ARB_create_context?)
                 (force lazy-GLX_ARB_create_context_profile?))
            ;; If the GLX version is high enough, legacy? is #f, and GLX has the right extensions,
            ;; try to get a core-profile context
            (glx-create-core-context xdisplay cfg share-gl)
            ;; Otherwise use the old method
            (glx-create-new-context xdisplay cfg share-gl)))
      ;; The above will return a direct rendering context when it can
      ;; If it doesn't, the context will be version 1.4 or lower, unless GLX is implemented with
      ;; proprietary extensions (NVIDIA's drivers sometimes do this)

      (when (and widget (send conf get-sync-swap))
	(glXSwapIntervalEXT xdisplay (gdk_x11_drawable_get_xid drawable) 1))

      ;; Now wrap the GLX context in a gl-context%
      (cond
       [gl
        ;; If there's no widget, this is for a pixmap, so get the stupid GLX wrapper for it or
        ;; indirect rendering may crash on some systems (notably mine)
        (define pixmap
          (if widget #f (glXCreateGLXPixmap xdisplay
                                            (glXGetVisualFromFBConfig xdisplay cfg)
					    (if gtk3?
						(cast drawable _Pixmap _ulong)
						(gdk_x11_drawable_get_xid drawable)))))
        
        (define ctxt (new gl-context% [gl gl] [display display] [drawable drawable] [pixmap pixmap]))
        ;; Refcount these so they don't go away until the finalizer below destroys the GLXContext
        (g_object_ref display)
        (unless (and gtk3? (not widget)) (g_object_ref drawable))
        (register-finalizer
         ctxt
         (λ (ctxt)
           (define gl (send ctxt get-handle))
           (define display (send ctxt get-gtk-display))
           (define drawable (send ctxt get-gtk-drawable))
           (define pixmap (send ctxt get-glx-pixmap))
           (define xdisplay (gdk_x11_display_get_xdisplay display))
           (when pixmap (glXDestroyGLXPixmap xdisplay pixmap))
           (glXDestroyContext xdisplay gl)
           (unless (and gtk3? (not widget)) (g_object_unref drawable))
           (g_object_unref display)))
        ctxt]
       [else  #f])])]))

(define (make-gtk-widget-gl-context widget conf)
  (call-as-atomic
   (lambda ()
     (make-gtk-drawable-gl-context widget (widget-window widget) conf #t))))

(define (make-gtk-pixmap-gl-context pixmap conf)
  (call-as-atomic
   (lambda ()
     (make-gtk-drawable-gl-context #f pixmap conf #f))))

(define (make-wayland-gl-context bm conf)
  (call-as-atomic
   (lambda ()
     (make-gtk-drawable-gl-context #f bm conf #f))))

;; ===================================================================================================

(define widget-config-hash (make-weak-hasheq))

(define (prepare-widget-gl-context widget conf)
  (hash-set! widget-config-hash widget (if conf conf (make-object gl-config%))))

(define (create-widget-gl-context widget)
  (define conf (hash-ref widget-config-hash widget #f))
  (and conf (make-gtk-widget-gl-context widget conf)))

(define-local-member-name
  get-gdk-pixmap
  install-gl-context)

(define (create-and-install-gl-context bm conf)
  (define ctxt (if wayland?
		   (make-wayland-gl-context bm conf)
		   (make-gtk-pixmap-gl-context (send bm get-gdk-pixmap) conf)))
  (and ctxt (send bm install-gl-context ctxt)))
