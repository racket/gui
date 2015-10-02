#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/define
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out scheme_add_gc_callback
              scheme_remove_gc_callback
              make-gc-action-desc
              make-gl-install
              make-gl-uninstall))

;; ----------------------------------------
;; 10.10 and earlier: change window opacity

(define objc-lib (ffi-lib "libobjc"))

(define msg-send-proc (get-ffi-obj 'objc_msgSend objc-lib _fpointer))

(define-mz scheme_add_gc_callback (_fun _racket _racket -> _racket))
(define-mz scheme_remove_gc_callback (_fun _racket -> _void))

(define (make-gc-action-desc win sel val)
  (vector
   (vector (if (= (ctype-sizeof _CGFloat) 4)
               'ptr_ptr_float->void
               'ptr_ptr_double->void)
           msg-send-proc 
           win
           sel
           val)))

;; ----------------------------------------
;; 10.11 and later: OpenGL texture

(define gl-lib (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL"))
(define-ffi-definer define-gl gl-lib)

(import-class NSOpenGLContext)

(define _GLsizei _int)
(define _GLint _int)
(define _GLuint _uint)
(define _GLenum _int)
(define _GLbitfield _int)
(define _GLfloat _float)
(define _GLclampf _float)

(define-gl glGenTexture (_fun (_GLsizei = 1) (v : (_ptr o _GLuint)) -> _void -> v)
  #:c-id glGenTextures)
(define-gl glGenLists (_fun _GLsizei -> _GLuint))
(define-gl glNewList (_fun _GLuint _GLenum -> _void))
(define-gl glEndList (_fun -> _void))

(define-gl glBindTexture (_fun _GLenum _GLuint -> _void))
(define-gl glTexParameteri (_fun _GLenum _GLenum _GLint -> _void))
(define-gl glTexImage2D (_fun _GLenum _GLint _GLint _GLsizei _GLsizei _GLint _GLenum _GLenum _pointer -> _void))

(define-gl glBegin (_fun _GLenum -> _void))
(define-gl glEnd (_fun -> _void))
(define-gl glEnable (_fun _GLenum -> _void))
(define-gl glDisable (_fun _GLenum -> _void))

(define-gl glMaterialfv (_fun _GLenum _GLenum (_vector i _GLfloat) -> _void))
(define-gl glTexCoord2f (_fun _GLfloat _GLfloat -> _void))
(define-gl glVertex3f (_fun _GLfloat _GLfloat _GLfloat -> _void))

(define-gl glViewport (_fun _GLint _GLint _GLsizei _GLsizei -> _void))
(define-gl glMatrixMode (_fun _GLenum -> _void))
(define-gl glLoadIdentity (_fun -> _void))
(define-gl glOrtho (_fun _double  _double _double  _double  _double  _double -> _void))
(define-gl glClearColor (_fun _GLclampf _GLclampf _GLclampf _GLclampf -> _void))
(define-gl glClear (_fun _GLbitfield -> _void))

(define-gl glClear-pointer _fpointer
  #:c-id glClear)
(define-gl glCallList-pointer _fpointer
  #:c-id glCallList)
(define-gl glFlush-pointer _fpointer
  #:c-id glFlush)

(define GL_TEXTURE_2D #x0DE1)
(define GL_TEXTURE_MAG_FILTER #x2800)
(define GL_TEXTURE_MIN_FILTER #x2801)
(define GL_TEXTURE_WRAP_S #x2802)
(define GL_TEXTURE_WRAP_T #x2803)

(define GL_LINEAR #x2601)
(define GL_CLAMP  #x2900)

(define GL_RGBA #x1908)

(define GL_UNSIGNED_BYTE #x1401)

(define GL_COMPILE #x1300)

(define GL_FRONT #x0404)
(define GL_AMBIENT_AND_DIFFUSE #x1602)

(define GL_POLYGON #x0009)

(define GL_PROJECTION #x1701)
(define GL_MODELVIEW #x1700)

(define GL_COLOR_BUFFER_BIT #x00004000)

(define (make-gl-square argb uw uh backing-scale)
  (define w (inexact->exact (ceiling (* backing-scale uw))))
  (define h (inexact->exact (ceiling (* backing-scale uh))))
  (define size (* w h 4))
  (define size-4 (- size 4))
  (define rgba (make-bytes size))
  (for ([i (in-range 0 size 4)])
    (define j (- size-4 i))
    (bytes-set! rgba (+ i 3) (bytes-ref argb j))
    (bytes-set! rgba i (bytes-ref argb (+ j 1)))
    (bytes-set! rgba (+ i 1) (bytes-ref argb (+ j 2)))
    (bytes-set! rgba (+ i 2) (bytes-ref argb (+ j 3))))
  
  (define tex (glGenTexture))
  
  (glBindTexture GL_TEXTURE_2D tex)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA
                w h 0
                GL_RGBA GL_UNSIGNED_BYTE rgba)
  
  (define wi (exact->inexact uw))
  (define hi (exact->inexact uh))
  
  (define list-id (glGenLists 1))
  (glNewList list-id GL_COMPILE)
  (glEnable GL_TEXTURE_2D)
  (glBindTexture GL_TEXTURE_2D tex)
  (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE (vector 1.0 1.0 1.0 1.0))
  (glBegin GL_POLYGON)
  (glTexCoord2f 0.0 0.0)
  (glVertex3f 0.0 0.0 0.0)
  (glTexCoord2f 1.0 0.0)
  (glVertex3f wi 0.0 0.0)
  (glTexCoord2f 1.0 1.0)
  (glVertex3f wi hi 0.0)
  (glTexCoord2f 0.0 1.0)
  (glVertex3f 0.0 hi 0.0)
  (glEnd)
  (glDisable GL_TEXTURE_2D)
  (glEndList)
  
  list-id)

(define (make-gl-install win glv w h argb backing-scale)
  (define gl (tell glv openGLContext))
  
  (define old-gl (tell NSOpenGLContext currentContext))
  (tell gl makeCurrentContext)
  (glViewport 0 0 w h)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 (exact->inexact w) 0.0 (exact->inexact h) -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glClearColor 1.0 1.0 1.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)

  (define list-id (make-gl-square argb w h backing-scale))
  
  (if old-gl
      (tellv old-gl makeCurrentContext)
      (tellv NSOpenGLContext clearCurrentContext))
  
  (vector
   (vector 'ptr_ptr->save
           msg-send-proc
           NSOpenGLContext
           (selector currentContext))
   (vector 'ptr_ptr_ptr->void
           msg-send-proc
           gl
           (selector makeCurrentContext)
           #f)
   (vector 'int->void
           glClear-pointer
           GL_COLOR_BUFFER_BIT)
   (vector 'int->void
           glCallList-pointer
           list-id)
   (vector 'int->void
           glFlush-pointer
           0)
   (vector 'ptr_ptr_ptr->void
           msg-send-proc
           gl
           (selector flushBuffer)
           #f)
   (vector 'ptr_ptr_ptr->void
           msg-send-proc
           NSOpenGLContext
           (selector clearCurrentContext)
           #f)
   (vector 'save!_ptr->void
           msg-send-proc
           (selector makeCurrentContext))))

(define (make-gl-uninstall win glv w h)
  (define gl (tell glv openGLContext))
    
  (vector
   (vector 'ptr_ptr->save
           msg-send-proc
           NSOpenGLContext
           (selector currentContext))
   (vector 'ptr_ptr_ptr->void
           msg-send-proc
           gl
           (selector makeCurrentContext)
           #f)
   (vector 'int->void
           glClear-pointer
           GL_COLOR_BUFFER_BIT)
   (vector 'int->void
           glFlush-pointer
           0)
   (vector 'ptr_ptr_ptr->void
           msg-send-proc
           gl
           (selector flushBuffer)
           #f)
   (vector 'ptr_ptr_ptr->void
           msg-send-proc
           NSOpenGLContext
           (selector clearCurrentContext)
           #f)
   (vector 'save!_ptr->void
           msg-send-proc
           (selector makeCurrentContext))))
