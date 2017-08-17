#lang scheme/base

(require (for-syntax scheme/base)
         scheme/unit
         racket/promise
         racket/class
         racket/runtime-path
         "bday.rkt"
         "sig.rkt"
         mred/mred-sig)

(provide icon@)

(define-runtime-path eof-bitmap-path '(lib "eof.gif" "icons"))
(define-runtime-path anchor-bitmap-path '(lib "anchor.gif" "icons"))
(define-runtime-path lock-bitmap-path '(lib "lock.gif" "icons"))
(define-runtime-path unlock-bitmap-path '(lib "unlock.gif" "icons"))
(define-runtime-path return-bitmap-path '(lib "return.xbm" "icons"))
(define-runtime-path paren-bitmap-path '(lib "paren.xbm" "icons"))
(define-runtime-path mrf-bitmap-path '(lib "mrf.png" "icons"))
(define-runtime-path gc-on-bitmap-path '(lib "recycle.png" "icons"))

(define-runtime-path up-down-mask-path '(lib "up-down-mask.xbm" "icons"))
(define-runtime-path up-down-csr-path '(lib "up-down-cursor.xbm" "icons"))

(define-runtime-path left-right-mask-path '(lib "left-right-mask.xbm" "icons"))
(define-runtime-path left-right-csr-path '(lib "left-right-cursor.xbm" "icons"))

(define-unit icon@
  (import mred^)
  (export framework:icon^)
  
  (define eof-bitmap (delay/sync (let ([bm (make-object bitmap% eof-bitmap-path)])
                                   (unless (send bm ok?)
                                     (error 'eof-bitmap "not ok ~s\n" eof-bitmap-path))
                                   bm)))
  (define (get-eof-bitmap) (force eof-bitmap))
  
  (define anchor-bitmap (delay/sync (make-object bitmap% anchor-bitmap-path)))
  (define (get-anchor-bitmap) (force anchor-bitmap))
  
  (define lock-bitmap (delay/sync (make-object bitmap% lock-bitmap-path)))
  (define (get-lock-bitmap) (force lock-bitmap))
  
  (define unlock-bitmap (delay/sync (make-object bitmap% unlock-bitmap-path)))
  (define (get-unlock-bitmap) (force unlock-bitmap))
  
  (define autowrap-bitmap (delay/sync (make-object bitmap% return-bitmap-path)))
  (define (get-autowrap-bitmap) (force autowrap-bitmap))
  
  (define paren-highlight-bitmap (delay/sync (make-object bitmap% paren-bitmap-path)))
  (define (get-paren-highlight-bitmap) (force paren-highlight-bitmap))
  
  (define up/down-cursor #f)
  (define (get-up/down-cursor)
    (unless up/down-cursor (set! up/down-cursor (make-object cursor% 'size-n/s)))
    up/down-cursor)
  (define left/right-cursor #f)
  (define (get-left/right-cursor)
    (unless left/right-cursor (set! left/right-cursor (make-object cursor% 'size-e/w)))
    left/right-cursor)
  
  (define mrf-on-bitmap (delay/sync (make-object bitmap% mrf-bitmap-path)))
  (define gc-on-bitmap (delay/sync (read-bitmap gc-on-bitmap-path #:try-@2x? #t)))
  
  (define (make-off-bitmap onb)
    (let* ([bitmap (make-object bitmap%
                     (send onb get-width)
                     (send onb get-height))]
           [bdc (make-object bitmap-dc% bitmap)])
      (send bdc clear)
      (send bdc set-bitmap #f)
      bitmap))
  
  (define mrf-off-bitmap (delay/sync (make-off-bitmap (force mrf-on-bitmap))))
  (define gc-off-bitmap (delay/sync (make-off-bitmap (force gc-on-bitmap))))
  
  (define (get-gc-on-bitmap)
    (force
     (if (mrf-bday?)
         mrf-on-bitmap
         gc-on-bitmap)))
  
  (define (get-gc-off-bitmap)
    (force
     (if (mrf-bday?)
         mrf-off-bitmap
         gc-off-bitmap))))
