#lang racket
(require ffi/unsafe
         ffi/unsafe/define
	 racket/draw/unsafe/cairo
	 (only-in "queue.rkt" wayland?))

(provide create-cairo-texture-sync
         update-texture-from-cairo
	 update-cairo-from-texture
         destroy-cairo-texture-sync)

(define libgl (and wayland?
		   (ffi-lib "libGL" '("1" ""))))

;; ============================================================================
;; Type Definitions
;; ============================================================================

;; OpenGL types  
(define _GLenum _uint32)
(define _GLuint _uint32)
(define _GLint _int32)
(define _GLsizei _int32)

;; Cairo types
(define _cairo_surface_t _pointer)
(define _cairo_t _pointer)
(define _cairo_format_t _int)

;; Constants

(define GL_TEXTURE_2D #x0DE1)
(define GL_BGRA #x80E1)
(define GL_RGBA #x1908)
(define GL_RGB #x1907)
(define GL_RED #x1903)
(define GL_UNSIGNED_BYTE #x1401)
(define GL_LINEAR #x2601)
(define GL_CLAMP_TO_EDGE #x812F)
(define GL_TEXTURE_MIN_FILTER #x2801)
(define GL_TEXTURE_MAG_FILTER #x2800)
(define GL_TEXTURE_WRAP_S #x2802)
(define GL_TEXTURE_WRAP_T #x2803)
(define GL_UNPACK_ROW_LENGTH #x0CF2)
(define GL_FRAMEBUFFER #x8D40)
(define GL_COLOR_ATTACHMENT0 #x8CE0)
(define GL_FRAMEBUFFER_COMPLETE #x8CD5)
(define GL_PACK_ROW_LENGTH #x0D02)

;; ============================================================================
;; OpenGL Function Bindings  
;; ============================================================================

(define-ffi-definer define-gl libgl
  #:default-make-fail make-not-available)

(define-gl glGenTextures
  (_fun _GLsizei (i : (_ptr o _GLuint)) -> _void -> i))

(define-gl glBindTexture
  (_fun _GLenum _GLuint -> _void))

(define-gl glTexImage2D
  (_fun _GLenum _GLint _GLint _GLsizei _GLsizei _GLint _GLenum _GLenum _pointer -> _void))

(define-gl glTexSubImage2D
  (_fun _GLenum _GLint _GLint _GLint _GLsizei _GLsizei _GLenum _GLenum _pointer -> _void))

(define-gl glTexParameteri
  (_fun _GLenum _GLenum _GLint -> _void))

(define-gl glPixelStorei
  (_fun _GLenum _GLint -> _void))

(define-gl glGenFramebuffers
  (_fun _GLsizei (i : (_ptr o _GLuint)) -> _void -> i))

(define-gl glBindFramebuffer
  (_fun _GLenum _GLuint -> _void))

(define-gl glFramebufferTexture2D
  (_fun _GLenum _GLenum _GLenum _GLuint _GLint -> _void))

(define-gl glCheckFramebufferStatus
  (_fun _GLenum -> _GLenum))

(define-gl glDeleteTextures
  (_fun _GLsizei (_ptr i _GLuint) -> _void))

(define-gl glDeleteFramebuffers
  (_fun _GLsizei (_ptr i _GLuint) -> _void))

(define-gl glReadPixels
  (_fun _GLint _GLint _GLsizei _GLsizei _GLenum _GLenum _pointer -> _void))

(define-gl glGetTexImage
  (_fun _GLenum _GLint _GLenum _GLenum _pointer -> _void))

(define-gl glFinish
  (_fun -> _void))

;; ============================================================================
;; Main Bridge Structure and Functions
;; ============================================================================

(struct cairo-texture-sync (texture-id
			    framebuffer-id
			    width
			    height
			    gl-format
			    gl-internal-format
			    gl-type))

;; Needs GL context set
(define (create-cairo-texture-sync width height)
  ;; Get GL format parameters
  (define-values (gl-format gl-internal-format gl-type)
    ;; assuming `CAIRO_FORMAT_ARGB32`
    (values GL_BGRA GL_RGBA GL_UNSIGNED_BYTE))
  
  ;; Create texture
  (define texture-id (glGenTextures 1))
  
  (glBindTexture GL_TEXTURE_2D texture-id)
  (glTexImage2D GL_TEXTURE_2D 0 gl-internal-format width height 0
                gl-format gl-type #f)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  
  ;; Create framebuffer
  (define framebuffer-id (glGenFramebuffers 1))
  
  (glBindFramebuffer GL_FRAMEBUFFER framebuffer-id)
  (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 
                         GL_TEXTURE_2D texture-id 0)

  ;; Check framebuffer completeness
  (define status (glCheckFramebufferStatus GL_FRAMEBUFFER))
  (unless (= status GL_FRAMEBUFFER_COMPLETE)
    (error "Framebuffer not complete"))
  
  (cairo-texture-sync texture-id framebuffer-id
		      width height
                      gl-format gl-internal-format gl-type))

;; Needs GL context set
(define (update-texture-from-cairo sync cairo-surface)
  ;; Flush Cairo surface
  (cairo_surface_flush cairo-surface)
  
  ;; Get Cairo surface data
  (define data (if (system-big-endian?)
		   (cairo_image_surface_get_data cairo-surface) ; copies
		   (cairo_image_surface_get_data* cairo-surface)))
  (define stride (cairo_image_surface_get_stride cairo-surface))
  (define width (cairo-texture-sync-width sync))
  (define height (cairo-texture-sync-height sync))

  (when (system-big-endian?)
    (reverse-byte-order data width height stride))

  ;; Bind texture and update
  (glBindTexture GL_TEXTURE_2D (cairo-texture-sync-texture-id sync))
  
  ;; Handle stride alignment if necessary
  (define bytes-per-pixel  4) ; assuming ARGB32
  
  (unless (= stride (* width bytes-per-pixel))
    (glPixelStorei GL_UNPACK_ROW_LENGTH (quotient stride bytes-per-pixel)))

  ;; Update texture
  (glTexSubImage2D GL_TEXTURE_2D 0 0 0 width height
                   (cairo-texture-sync-gl-format sync)
                   (cairo-texture-sync-gl-type sync)
                   data)
  
  ;; Reset stride setting
  (glPixelStorei GL_UNPACK_ROW_LENGTH 0))

(define (update-cairo-from-texture sync cairo-surface)
  ;; Get Cairo surface properties
  (define width (cairo-texture-sync-width sync))
  (define height (cairo-texture-sync-height sync))
  (define cairo-data (cairo_image_surface_get_data* cairo-surface))
  (define stride (cairo_image_surface_get_stride cairo-surface))
  
  ;; Bind the framebuffer with our texture
  (glBindFramebuffer GL_FRAMEBUFFER (cairo-texture-sync-framebuffer-id sync))
  
  ;; Handle stride alignment for reading
  (define bytes-per-pixel 4) ; ARGB32

  (unless (= stride (* width bytes-per-pixel))
    (glPixelStorei GL_PACK_ROW_LENGTH (quotient stride bytes-per-pixel)))
 
  ;; Read pixels from framebuffer into Cairo surface data
  (glReadPixels 0 0 width height
                (cairo-texture-sync-gl-format sync)
                (cairo-texture-sync-gl-type sync)
                cairo-data)
  
  ;; Reset pack alignment
  (glPixelStorei GL_PACK_ROW_LENGTH 0)
  
  ;; Ensure all GL operations complete
  (glFinish)
      
  (when (system-big-endian?)
    (reverse-byte-order cairo-data width height stride))

  ;; Mark Cairo surface as modified
  (cairo_surface_mark_dirty cairo-surface))

;; Needs GL context set
(define (destroy-cairo-texture-sync sync egl-display)
  (glDeleteTextures 1 (cairo-texture-sync-texture-id sync))  
  (glDeleteFramebuffers 1 (cairo-texture-sync-framebuffer-id sync)))

(define (reverse-byte-order data width height stride)
  (for ([j (in-range 0 height)])
       (define start (* j stride))
       (for ([i (in-range 0 width)])
	    (define b (ptr-ref data _byte (+ start (* i 4))))
	    (define g (ptr-ref data _byte (+ start 1 (* i 4))))
	    (define r (ptr-ref data _byte (+ start 2 (* i 4))))
	    (define a (ptr-ref data _byte (+ start 3 (* i 4))))
	    (ptr-set! data _byte (+ start (* i 4)) a)
	    (ptr-set! data _byte (+ start 1 (* i 4)) r)
	    (ptr-set! data _byte (+ start 2 (* i 4)) g)
	    (ptr-set! data _byte (+ start 3 (* i 4)) b))))
