#lang scheme/base

(require (for-syntax scheme/base)
         ;typed/racket/unit
         scheme/unit
         racket/promise
         typed/racket/class
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

#;
(define-signature framework:icon^
  ([eof-bitmap : (-> (Instanceof BitMap%))]
   [get-eof-bitmap : (-> (Instanceof BitMap%))]
   [get-anchor-bitmap : (-> (Instanceof BitMap%))]
   [get-lock-bitmap : (-> (Instanceof BitMap%))]
   [get-unlock-bitmap : (-> (Instanceof BitMap%))]
   [get-autowrap-bitmap : (-> (Instanceof BitMap%))]
   [get-paren-highlight-bitmap : (-> (Instanceof BitMap%))]
   [make-off-bitmap : (-> (Instanceof BitMap%) (Instanceof BitMap%))]
   [get-gc-on-bitmap : (-> (Instanceof BitMap%))]
   [get-gc-off-bitmap : (-> (Instanceof BitMap%))]))

#;
(: icon@ (Unit
          (import mred^)
          (export framework:icon^)
          Void))

(define-unit icon@
  (import mred^)
  (export framework:icon^)

  #;
  (: eof-bitmap : (-> (Instanceof BitMap%)))
  (define eof-bitmap (delay/sync (let ([bm (make-object bitmap% eof-bitmap-path)])
                                   (unless (send bm ok?)
                                     (error 'eof-bitmap "not ok ~s\n" eof-bitmap-path))
                                   bm)))
  
  (define (get-eof-bitmap) (force eof-bitmap))

  #;
  (: anchor-bitmap (Instanceof BitMap%))
  (define anchor-bitmap (delay/sync (make-object bitmap% anchor-bitmap-path)))
  (define (get-anchor-bitmap) (force anchor-bitmap))

  #;
  (: lock-bitmap (Instanceof BitMap%))
  (define lock-bitmap (delay/sync (make-object bitmap% lock-bitmap-path)))
  #;
  (: get-lock-bitmap (-> (Instanceof BitMap%)))
  (define (get-lock-bitmap) (force lock-bitmap))

  #;
  (: unlock-bitmap (Instanceof BitMap%))
  (define unlock-bitmap (delay/sync (make-object bitmap% unlock-bitmap-path)))
  #;
  (: get-unlock-bitmap (-> (Instanceof BitMap%)))
  (define (get-unlock-bitmap) (force unlock-bitmap))

  #;
  (: autowrap-bitmap (Instanceof BitMap%))
  (define autowrap-bitmap (delay/sync (make-object bitmap% return-bitmap-path)))
  #;
  (: get-autowrap-bitmap (-> (Instanceof BitMap%)))
  (define (get-autowrap-bitmap) (force autowrap-bitmap))

  #;
  (: paren-highlight-bitmap (Instanceof BitMap%))
  (define paren-highlight-bitmap (delay/sync (make-object bitmap% paren-bitmap-path)))
  #;
  (: get-paren-highlight-bitmap (-> (Instanceof BitMap%)))
  (define (get-paren-highlight-bitmap) (force paren-highlight-bitmap))

  (define-syntax (make-get-cursor stx)
    (syntax-case stx ()
      [(_ id mask-path csr-path fallback)
       (syntax
        (begin
          (define id 
            (let ([ans (delay/sync 
                         (let* ([msk-b (make-object bitmap% mask-path)]
                                [csr-b (make-object bitmap% csr-path)])
                           (if (and (send msk-b ok?)
                                    (send csr-b ok?))
                               (let ([csr (make-object cursor% msk-b csr-b 7 7)])
                                 (if (send csr ok?)
                                     csr
                                     (make-object cursor% fallback)))
                               (make-object cursor% fallback))))])
              (λ () (force ans))))))]))

  (make-get-cursor get-up/down-cursor up-down-mask-path up-down-csr-path 'size-n/s)
  (make-get-cursor get-left/right-cursor left-right-mask-path left-right-csr-path 'size-e/w)

  #;
  (: mrf-on-bitmap (Instanceof BitMap%)) 
  (define mrf-on-bitmap (delay/sync (make-object bitmap% mrf-bitmap-path)))
  #;
  (: gc-on-bitmap (Instanceof Bitmap%))
  (define gc-on-bitmap (delay/sync (read-bitmap gc-on-bitmap-path #:try-@2x? #t)))

  #;
  (: make-off-bitmap (-> (Instanceof BitMap%) (Instanceof BitMap%)))
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

  #;
  (: get-gc-on-bitmap (-> (Instanceof BitMap%)))
  (define (get-gc-on-bitmap)
    (force
     (if (mrf-bday?)
         mrf-on-bitmap
         gc-on-bitmap))) 

  #;
  (: get-gc-off-bitmap (Instanceof Bitmap%))
  (define (get-gc-off-bitmap)
    (force
     (if (mrf-bday?)
         mrf-off-bitmap
         gc-off-bitmap))))  
