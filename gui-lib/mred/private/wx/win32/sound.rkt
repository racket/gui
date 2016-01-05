#lang racket/base
(require ffi/unsafe
	 ffi/winapi
	 ffi/unsafe/custodian
	 ffi/unsafe/atomic
         racket/class
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide
 (protect-out play-sound))

(define BUFFER-SIZE 512)
(define BUFFER-BYTES-SIZE (* 2 BUFFER-SIZE))

(define-winmm mciGetErrorStringW
  (_fun _int
	[buf : _pointer = (malloc BUFFER-BYTES-SIZE)]
	[_int = BUFFER-SIZE]
	-> [ret : _bool]
	-> (and ret (cast buf _pointer _string/utf-16))))

(define-winmm mciSendStringW
  (_fun _string/utf-16 [_pointer = #f] [_int = 0] [_pointer = #f]
	-> [ret : _int]
	-> (if (zero? ret)
	       (void)
	       (error 'mciSendStringW "~a" (mciGetErrorStringW ret)))))

(define (mci-send fmt . args)
  (mciSendStringW (apply format fmt args)))

(define-winmm mciSendStringW*
  (_fun _string/utf-16
	[buf : _pointer = (malloc BUFFER-BYTES-SIZE)]
	[_int = BUFFER-SIZE]
	[_pointer = #f]
	-> [ret : _int]
	-> (if (zero? ret)
	       (cast buf _pointer _string/utf-16)
	       (error 'mciSendStringW* "~a" (mciGetErrorStringW ret))))
  #:c-id mciSendStringW)

(define (mci-send* fmt . args)
  (mciSendStringW* (apply format fmt args)))

(define (play-sound file async?)
  ;; Generated ID is unique enough, because we only
  ;; instantiate this library in one place:
  (define id (gensym 'play))
  (define cust (make-custodian))
  (call-as-atomic
   (lambda ()
     (mci-send "open \"~a\" alias ~a" (simplify-path file) id)
     (register-custodian-shutdown
      id
      (lambda (id)
        (mci-send "close ~a" id))
      cust)))
  (define (done msec)
    (when msec (sleep (/ msec 1000)))
    (custodian-shutdown-all cust))
  (dynamic-wind
   void
   (lambda ()
     (mci-send "set ~a time format milliseconds" id)
     (define len (let ([s (mci-send* "status ~a length" id)])
                   (string->number s)))
     (unless len (error 'play "mci did not return a numeric length"))
     (mci-send "play ~a" id)
     (if async? (thread (lambda () (done len))) (done len)))
   (lambda ()
     (unless async?
       (done #f))))
  ;; Report success, since otherwise we throw an error:
  #t)
