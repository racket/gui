#lang racket/base

(require mzlib/unit
         racket/gui/base
         "hierlist/hierlist-sig.rkt"
         "hierlist/hierlist-unit.rkt")

(define-values/invoke-unit/infer hierlist@)

(provide-signature-elements hierlist^)

;; ============================================================

(module+ demo
(require racket/class)

(define f (make-object frame% (format "test ~s" (version))))
(define p (make-object horizontal-panel% f))
(define c (make-object (class hierarchical-list%
                         (define/override (on-item-opened i)
                           (let ([f (send i user-data)])
                             (when f (f i))))
                         (define/override (on-select i)
                           (printf "Selected: ~a\n"
                                   (if i 
                                       (send (send i get-editor) get-flattened-text)
                                       i)))
                         (define/override (on-double-select s)
                           (printf "Double-click: ~a\n"
                                   (send (send s get-editor) get-flattened-text)))
                         (super-new))
                       p))

(define a (send c new-list))
(send (send a get-editor) insert "First Item: List")
(send (send (send a new-item) get-editor) insert "Sub1")
(send (send (send a new-item) get-editor) insert "Sub2")
(define a.1 (send a new-list))
(send (send a.1 get-editor) insert "Deeper List")
(send (send (send a.1 new-item) get-editor) insert "Way Down")

(define b (send c new-item))
(send (send b get-editor) insert "Second Item")

(define d (send c new-list))
(send (send d get-editor) insert "dynamic")
(send d user-data (lambda (d)
		    (time (let loop ([i 30])
			    (unless (zero? i)
				    (send (send (send d new-item) get-editor) insert (number->string i))
				    (loop (sub1 i)))))))

(define x (send c new-list))
(send (send x get-editor) insert "x")

(define y (send c new-item))
(send (send y get-editor) insert "y")

(define z (send c new-list))
(send (send z get-editor) insert "a multi-line\nlabel")
(send (send (send z new-item) get-editor) insert "Sub1")
(send (send (send z new-item) get-editor) insert "Sub2")

(send f show #t)

(yield (make-semaphore))
)
