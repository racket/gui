#lang racket/base
(require ffi/unsafe
         racket/class
         racket/runtime-path
	 setup/cross-system
	 (for-syntax racket/base
		     setup/cross-system)
	 "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide
 (protect-out play-sound))

(define-winmm PlaySoundW (_wfun _string/utf-16 _pointer _DWORD -> _BOOL))

(define SND_SYNC   #x0000)
(define SND_ASYNC  #x0001)
(define SND_NOSTOP #x0010)

;; Plays a sound using PlaySOund directly, which is limited
;; to playing a single sound at a time in the process:
(define (in-process-play-sound path async?)
  (let ([path (simplify-path path #f)]
	[done (make-semaphore)])
    (and (let ([p (path->string
		   (cleanse-path (path->complete-path path)))])
	   (atomically
	    (when previous-done-sema (semaphore-post previous-done-sema))
	    (set! previous-done-sema done)
	    (PlaySoundW p #f SND_ASYNC)))
	 (or async?
	     ;; Implement synchronous playing by polling, where
	     ;; PlaySound with no sound file and SND_NOSTOP polls.
	     (let loop ()
	       (sleep 0.1)
	       (or (semaphore-try-wait? done)
		   (PlaySoundW #f #f (bitwise-ior SND_ASYNC SND_NOSTOP))
		   (loop)))))))
(define previous-done-sema #f)

;; Runs a separate process to play a sound using a very
;; small executable:
(define (other-process-play-sound path async?)
  (define c (make-custodian))
  (define-values (s i o e)
    (parameterize ([current-custodian c]
		   [current-subprocess-custodian-mode 'kill])
      (subprocess #f #f #f
		  (car racket-playsound.exe)
		  'exact
		  (path->string
		   (simplify-path
		    (path->complete-path path))))))
  (close-input-port i)
  (close-input-port e)
  (close-output-port o)
  (unless async?
    (dynamic-wind
     void
     (lambda () (subprocess-wait s))
     (lambda () (subprocess-kill s #t))))
  (define n (subprocess-status s))
  ;; Sound may be still playing, but assume success if it
  ;; hasn't failed, yet:
  (or (symbol? n)
      (zero? n)))

(define-runtime-path-list racket-playsound.exe
  (if (eq? 'windows (cross-system-type))
      (list '(so "racket-playsound.exe"))
      null))

(define (play-sound path async?)
  (if (and (pair? racket-playsound.exe)
	   (file-exists? (car racket-playsound.exe)))
      (other-process-play-sound path async?)
      (in-process-play-sound path async?)))
