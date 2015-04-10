#lang racket/base
(require racket/contract
         (for-syntax racket/base))

(provide coroutine)
(provide
 (contract-out
  [coroutine-run (-> coroutine? any/c boolean?)]
  [coroutine-runnable? (-> coroutine? boolean?)]
  [coroutine-value (-> coroutine? any/c)]))

(define-syntax (coroutine stx)
  (define-values (timeout more-stx)
    (syntax-case stx ()
      [(_ #:at-least msec-expr . more)
       (values #'msec-expr #'more)]
      [(_ . more) (values #'#f #'more)]))
  (syntax-case more-stx ()
    [(pause-id first-id exp1 exp2 ...)
     #`(coroutine/proc #,timeout (λ (pause-id first-id) exp1 exp2 ...))]))

(struct coroutine ([run-proc #:mutable]
                   [val #:mutable]
                   tag
                   [last-start #:mutable]
                   expiry)
  #:omit-define-syntaxes
  #:extra-constructor-name
  make-coroutine)

(define (coroutine/proc expiry cproc)
  (define tag (make-continuation-prompt-tag 'coroutine))
  (define (pauser)
    (define actually-pause?
      (cond
        [(coroutine-last-start the-coroutine)
         =>
         (λ (start-time)
           (define now (get-time))
           ((- now start-time) . >= . (coroutine-expiry the-coroutine)))]
        [else #t]))
    (when actually-pause?
      (call-with-composable-continuation 
       (λ (k) (abort-current-continuation tag k))
       tag)))
  (define the-coroutine
    (make-coroutine (λ (first-val) (values (cproc pauser first-val) #t))
                    #f
                    tag
                    #f
                    expiry))
  the-coroutine)

(define (get-time) (current-process-milliseconds (current-thread)))

(define (coroutine-run a-coroutine val)
  (cond
    [(coroutine-run-proc a-coroutine)
     =>
     (λ (proc)
       (when (coroutine-expiry a-coroutine)
         (set-coroutine-last-start! a-coroutine (get-time)))
       (define-values (res done?)
         (call-with-continuation-prompt
          (λ () (proc val))
          (coroutine-tag a-coroutine)
          (λ (k)
            (set-coroutine-run-proc! 
             a-coroutine
             (λ (next-val)
               (k next-val)))
            (values #f #f))))
       (cond
         [done?
          (set-coroutine-run-proc! a-coroutine #f)
          (set-coroutine-val! a-coroutine res)
          #t]
         [else #f]))]
    [else
     (error 'coroutine-run "coroutine already terminated")]))

(define (coroutine-runnable? a-coroutine)
  (and (coroutine-run-proc a-coroutine) 
       #t))

(define (coroutine-value a-coroutine)
  (when (coroutine-runnable? a-coroutine)
    (error 'coroutine-value "coroutine not yet finished"))
  (coroutine-val a-coroutine))

(module+ test
  (require rackunit)
  
  (define c
    (coroutine
     pause
     first
     (begin
       (printf "first ~s\n" first)
       (let loop ([i 5])
         (printf "i ~a\n" i)
         (when (zero? (modulo i 3))
           (printf ">> ~a\n" (pause)))
         (cond
           [(zero? i) '()]
           [else
            (cons i (loop (- i 1)))])))))
  
  (define (with-stdout th)
    (define sp (open-output-string))
    (list (parameterize ([current-output-port sp])
            (th))
          (get-output-string sp)))
  
  (check-equal? (with-stdout (λ () (coroutine-run c 123)))
                (list #f "first 123\ni 5\ni 4\ni 3\n"))
  
  (check-equal? (with-stdout (λ () (coroutine-run c 456)))
                (list #f ">> 456\ni 2\ni 1\ni 0\n"))
  
  (check-equal? (with-stdout (λ () (coroutine-run c 789)))
                (list #t ">> 789\n"))
  
  (check-equal? (coroutine-value c)
                '(5 4 3 2 1))
  
  
  (define c2
    (coroutine
     pause first
     (define x first)
     (define (try-it)
       (define new-x (pause))
       (printf "~a => ~a\n" x new-x)
       (set! x new-x))
     (try-it)
     (try-it)
     x))
  
  (check-equal? (with-stdout (λ () (coroutine-run c2 0)))
                (list #f ""))
  (check-equal? (with-stdout (λ () (coroutine-run c2 1)))
                (list #f "0 => 1\n"))
  (check-equal? (with-stdout (λ () (coroutine-run c2 2)))
                (list #t "1 => 2\n"))
  (check-equal? (coroutine-value c2)
                2)

  (check-equal?
   (let ([c (coroutine
             #:at-least 100
             pause first
             (pause)
             (printf "hi"))])
     (with-stdout (λ () (coroutine-run c 'whatever))))
   (list #t "hi"))

  (check-equal?
   (let ([c (coroutine
             #:at-least 0
             pause first
             (pause)
             (printf "hi"))])
     (with-stdout (λ () (coroutine-run c 'whatever))))
   (list #f "")))
