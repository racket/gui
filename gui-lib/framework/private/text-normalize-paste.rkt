#lang racket
(require racket/unit
         mred/mred-sig
         string-constants
         "text-sig.rkt"
         "../preferences.rkt")

(provide text-normalize-paste@ as-a-paste)

(define-local-member-name as-a-paste)

(define-unit text-normalize-paste@
  (import mred^
          text-basic^)
  (export text-normalize-paste^)

  (define normalize-paste<%> (interface ((class->interface text%))
                               ask-normalize?
                               string-normalize))
  (define normalize-paste-mixin
    (mixin (basic<%>) (normalize-paste<%>)
      (inherit begin-edit-sequence end-edit-sequence
               delete insert split-snip find-snip
               get-snip-position get-top-level-window find-string)
    
      ;; pasting-info : (or/c #f (listof (list number number)))
      ;; when #f, we are not in a paste
      ;; when a list, we are in a paste and the 
      ;;   list contains the regions that have
      ;;   been changed by the paste
      (define paste-info #f)

      (define/public (ask-normalize?)
        (cond
          [(preferences:get 'framework:ask-about-paste-normalization)
           (define-values (mbr checked?)
             (message+check-box/custom
              (string-constant drscheme)
              (string-constant normalize-string-info)
              (string-constant dont-ask-again)
              (string-constant normalize)
              (string-constant leave-alone)
              #f
              (get-top-level-window)
              (cons (if (preferences:get 'framework:do-paste-normalization)
                        'default=1
                        'default=2)
                    '(caution))
              2))
           (define normalize? (not (equal? 2 mbr)))
           (preferences:set 'framework:ask-about-paste-normalization (not checked?))
           (preferences:set 'framework:do-paste-normalization normalize?)
           normalize?]
          [else
           (preferences:get 'framework:do-paste-normalization)]))
      (define/public (string-normalize s) 
        (regexp-replace* 
         #rx"\u200b" 
         (regexp-replace*
          #rx"\u2212" 
          (string-normalize-nfkc s)
          "-")
         ""))

      ;; method for use in the test suites
      (define/public-final (as-a-paste thunk)
        (dynamic-wind
         (λ () (set! paste-info '()))
         (λ ()
           (thunk)
           (define local-paste-info paste-info)
           (set! paste-info #f)
           (deal-with-paste local-paste-info))
         ;; use the dynamic wind to be sure that the paste-info is set back to #f
         ;; in the case that the middle thunk raises an exception
         (λ () (set! paste-info #f))))
    
      (define/override (do-paste start the-time)
        (as-a-paste (λ () (super do-paste start the-time))))
    
      (define/augment (after-insert start len)
        (when paste-info
          (set! paste-info (cons (list start len) paste-info)))
        (inner (void) after-insert start len))

      (define/private (deal-with-paste local-paste-info)
        (let/ec abort
          (define ask? #t)
          (for ([insertion (in-list local-paste-info)])
            (define start (list-ref insertion 0))
            (define len (list-ref insertion 1))
            (split-snip start)
            (split-snip (+ start len))
            (let loop ([snip (find-snip (+ start len) 'before-or-none)])
              (when snip
                (define prev-snip (send snip previous))
                (define pos (get-snip-position snip))
                (when (pos . >= . start)
                  (when (is-a? snip string-snip%)
                    (define old (send snip get-text 0 (send snip get-count)))
                    (define new (string-normalize old))
                    (unless (equal? new old)
                      (when ask?
                        (set! ask? #f)
                        (unless (ask-normalize?) (abort)))
                      (define snip-pos (get-snip-position snip))
                      (delete snip-pos (+ snip-pos (string-length old)))
                      (insert new snip-pos snip-pos #f)))
                  (loop prev-snip)))))))
    
      (super-new))))
