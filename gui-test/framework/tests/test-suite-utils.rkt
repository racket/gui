#lang racket/base

(require (only-in mzscheme fluid-let)
         (for-syntax racket/base)
         "debug.rkt")

(provide
 test-name
 failed-tests
 number-of-tests
 
 ;(struct eof-result ())
 eof-result?
 
 load-framework-automatically
 shutdown-listener shutdown-mred mred-running?
 send-sexp-to-mred queue-sexp-to-mred
 test
 wait-for-frame
 
 ;; sexp -> void
 ;; grabs the frontmost window, executes the sexp and waits for a new frontmost window
 wait-for-new-frame
 
 wait-for
 
 reset-section-jump!
 set-section-jump!
 reset-section-name!
 set-section-name!
 set-only-these-tests!
 get-only-these-tests
 debug-printf
 
 exn->str)

(define section-jump void)
(define (set-section-jump! _s) (set! section-jump _s))
(define (reset-section-jump!) (set! section-jump #f))

(define section-name "<<setup>>")
(define (set-section-name! _s) (set! section-name _s))
(define (reset-section-name!) (set! section-name "<<setup>>"))

(define only-these-tests #f)
(define (get-only-these-tests) only-these-tests)
(define (set-only-these-tests! _t) (set! only-these-tests _t))

(define test-name "<<setup>>")
(define failed-tests null)
(define number-of-tests 0)

(module local-namespace racket/base
  (require racket/gui/base)
  (provide send-sexp-to-mred
           queue-sexp-to-mred
           eof-result?
           shutdown-listener shutdown-mred mred-running?
           load-framework-automatically)

  (define ns? #f)
  (define eventspace (make-eventspace))
  
  (define (send-sexp-to-mred sexp)
    (unless ns?
      (namespace-require 'framework)
      (namespace-require 'racket/gui/base)
      (set! ns? #t))
      
    (define c (make-channel))
    (parameterize ([current-eventspace eventspace])
      (queue-callback
       (λ ()
         (channel-put
          c
          (eval sexp)))))
    (channel-get c))

  (define queue-sexp-to-mred send-sexp-to-mred)

  (define (eof-result? x)
    (error 'local-namespace
           (string-append
            "cannot use eof-result? with the local-namespace setup"
            " (so probably it should be improved)")))

  (define (shutdown-listener)
    (error 'local-namespace
           (string-append
            "cannot use shutdown-listener with the local-namespace setup"
            " (maybe it could be improved -- not clear)")))
  (define (shutdown-mred)
    (error 'local-namespace
           (string-append
            "cannot use shutdown-mred with the local-namespace setup"
            " (maybe it could be improved -- not clear)")))
  
  (define (mred-running? x)
    (error 'local-namespace
           (string-append
            "cannot use mred-running? with the local-namespace setup"
            " (so probably it should be improved)")))
  
  (define load-framework-automatically? #t)
  (define load-framework-automatically
    (case-lambda
      [(new-load-framework-automatically?)
       (unless (eq? (not (not new-load-framework-automatically?))
                    load-framework-automatically?)
         (set! load-framework-automatically? (not (not new-load-framework-automatically?)))
         (error 'local-namespace
                (string-append
                 "cannot change load-framework-automatically with the local-namespace setup"
                 " (so probably it should be improved)")))]
      [() load-framework-automatically?])))
  
(module remote-process racket/base
  (require compiler/find-exe
           racket/system
           "debug.rkt"
           racket/tcp
           racket/pretty)
  (provide send-sexp-to-mred
           queue-sexp-to-mred
           eof-result?
           shutdown-listener shutdown-mred mred-running?
           load-framework-automatically)
  
  (define in-port #f)
  (define out-port #f)
  (define load-framework-automatically? #t)
  
  (define initial-port 6012)
  (define port-filename
    (build-path (find-system-path 'temp-dir)
                "framework-tests-receive-sexps-port.rkt"))
  
  (unless (file-exists? port-filename)
    (call-with-output-file port-filename
      (lambda (port) (write initial-port port))))

  (define listener
    (let loop ([port (call-with-input-file port-filename read)])
      (let ([l (with-handlers ([exn:fail? (lambda (_) #f)])
                 (tcp-listen port))])
        (if l
            (begin (debug-printf mz-tcp "listening to ~a\n" port)
                   (call-with-output-file port-filename
                     (lambda (p) (write port p))
                     #:exists 'truncate)
                   l)
            (begin (debug-printf mz-tcp "  tcp-listen failed for port ~a\n" port)
                   (loop (add1 port)))))))

  (define-struct eof-result ())
  
  (define (restart-mred)
    (shutdown-mred)
    (thread
     (lambda ()
       (define racket-bin (find-exe))
       (unless (system*
                racket-bin
                (path->string
                 (collection-file-path "framework-test-engine.rkt" "framework" "tests")))
         (eprintf "starting gracket failed; used path ~s\n"
                  racket-bin))))
    (debug-printf mz-tcp "accepting listener\n")
    (let-values ([(in out) (tcp-accept listener)])
      (set! in-port in)
      (set! out-port out))
    (when load-framework-automatically?
      (queue-sexp-to-mred
       '(begin (eval '(require framework))
               (eval '(require framework/tests/private/gui))))))
  
  (define shutdown-mred
    (lambda ()
      (when (and in-port
                 out-port)
        (with-handlers ([exn:fail? (lambda (x) (void))])
          (close-output-port out-port))
        (with-handlers ([exn:fail? (lambda (x) (void))])
          (close-input-port in-port))
        (set! in-port #f)
        (set! in-port #f))))
  
  (define load-framework-automatically
    (case-lambda
      [(new-load-framework-automatically?)
       (unless (eq? (not (not new-load-framework-automatically?))
                    load-framework-automatically?)
         (set! load-framework-automatically? (not (not new-load-framework-automatically?)))
         (shutdown-mred))]
      [() load-framework-automatically?]))

  (namespace-require 'racket) ;; in order to make the eval below work right.

  (define (send-sexp-to-mred sexp)
    (let/ec k
      (let ([show-text
             (lambda (sexp)
               (debug-when messages
                           (parameterize ([pretty-print-print-line
                                           (let ([prompt "  "]
                                                 [old-liner (pretty-print-print-line)])
                                             (lambda (ln port ol cols)
                                               (let ([ov (old-liner ln port ol cols)])
                                                 (if ln 
                                                     (begin (display prompt port)
                                                            (+ (string-length prompt) ov))
                                                     ov))))])
                             (pretty-print sexp)
                             (newline))))])
        (unless (and in-port
                     out-port
                     (with-handlers ([tcp-error? (lambda (x) #f)])
                       (or (not (char-ready? in-port))
                           (not (eof-object? (peek-char in-port))))))
          (restart-mred))
        (show-text sexp)
        (with-handlers ([exn:fail?
                         (lambda (x)
                           (cond
                             ;; this means that gracket was closed
                             ;; so we can restart it and try again.
                             [(tcp-error? x)
                              (restart-mred)
                              (write sexp out-port)
                              (newline out-port)
                              (flush-output out-port)]
                             [else (raise x)]))])
          (write sexp out-port)
          (newline out-port)
          (flush-output out-port))
        (let ([answer
               (with-handlers ([exn:fail?
                                (lambda (x)
                                  (if (tcp-error? x);; assume tcp-error means app closed
                                      eof
                                      (list 'cant-read
                                            (string-append
                                             (exn->str x)
                                             "; rest of string: "
                                             (format
                                              "~s"
                                              (apply
                                               string
                                               (let loop ()
                                                 (if (char-ready? in-port)
                                                     (let ([char (read-char in-port)])
                                                       (if (eof-object? char)
                                                           null
                                                           (cons char (loop))))
                                                     null))))))))])
                 (read in-port))])
          (show-text answer)
          (unless (or (eof-object? answer)
                      (and (list? answer)
                           (= 2 (length answer))
                           (memq (car answer)
                                 '(error last-error cant-read normal))))
            (error 'send-sexp-to-mred "unpected result from gracket: ~s\n" answer))
          (if (eof-object? answer)
              (raise (make-eof-result))
              (case (car answer)
                [(error)
                 (error 'send-sexp-to-mred "gracket raised \"~a\"" (list-ref answer 1))]
                [(last-error)
                 (error 'send-sexp-to-mred "gracket (last time) raised \"~a\"" (list-ref answer 1))]
                [(cant-read) (error 'mred/cant-parse (list-ref answer 1))]
                [(normal) 
                 (eval (list-ref answer 1))]))))))

  (define queue-sexp-to-mred
    (lambda (sexp)
      (send-sexp-to-mred
       `(let ([thunk (lambda () ,sexp)]  ;; low tech hygiene
              [c (make-channel)])
          (queue-callback (λ ()
                            (channel-put c
                                         (with-handlers ((exn:fail? (λ (x) (list 'exn x))))
                                           (list 'normal (thunk))))))
          (let ([res (channel-get c)])
            (if (eq? (list-ref res 0) 'normal)
                (list-ref res 1)
                (raise (list-ref res 1))))))))

  (define re:tcp-read-error (regexp "tcp-read:"))
  (define re:tcp-write-error (regexp "tcp-write:"))
  (define (tcp-error? exn)
    (or (regexp-match re:tcp-read-error (exn-message exn))
        (regexp-match re:tcp-write-error (exn-message exn))))
  
  (define shutdown-listener
    (lambda ()
      (shutdown-mred)
      (debug-printf mz-tcp "closing listener\n")
      (tcp-close listener)))
  
  (define (mred-running?)
    (if (char-ready? in-port)
        (not (eof-object? (peek-char in-port)))
        #t)))

(require (prefix-in r: (submod "." remote-process)))
(require (prefix-in l: (submod "." local-namespace)))
(define use-local? (getenv "PLTGUIUSELOCAL"))
(define-syntax (choose stx)
  (syntax-case stx ()
    [(_) #'(void)]
    [(_ n ns ...)
     (let ()
       (define (id-append p)
         (datum->syntax stx (string->symbol (format "~a~a" p (syntax-e #'n)))))
       (with-syntax ([l:n (id-append 'l:)]
                     [r:n (id-append 'r:)])
         #'(begin
             (define n (if use-local? l:n r:n))
             (choose ns ...))))]))

(choose send-sexp-to-mred
        queue-sexp-to-mred
        eof-result?
        shutdown-listener shutdown-mred mred-running?
        load-framework-automatically)

(define test
  (case-lambda
    [(in-test-name passed? sexp/proc) (test in-test-name passed? sexp/proc 'section)]
    [(in-test-name passed? sexp/proc jump)
     (fluid-let ([test-name in-test-name])
       (when (or (not only-these-tests)
                 (memq test-name only-these-tests))
         (let* ([result
                 (with-handlers ([exn:fail?
                                  (lambda (x)
                                    (if (exn? x)
                                        (exn->str x)
                                        x))])
                   (if (procedure? sexp/proc)
                       (sexp/proc)
                       (begin0 (send-sexp-to-mred sexp/proc)
                               (send-sexp-to-mred ''check-for-errors))))]
                [failed (with-handlers ([exn:fail?
                                         (lambda (x)
                                           (string-append
                                            "passed? test raised exn: "
                                            (if (exn? x)
                                                (exn->str x)
                                                (format "~s" x))))])
                          (not (passed? result)))])
           (set! number-of-tests (+ number-of-tests 1))
           (when failed
             (debug-printf schedule "FAILED ~a:\n  ~s\n" test-name result)
             (set! failed-tests (cons (cons section-name test-name) failed-tests))
             (case jump
               [(section) (section-jump)]
               [(continue) (void)]
               [else (jump)])))))]))

(define (wait-for/wrapper wrapper sexp)
  (let ([timeout 10]
        [pause-time 1/2])
    (send-sexp-to-mred
     (wrapper
      `(let ([test (lambda () ,sexp)])
         (let loop ([n ,(/ timeout pause-time)])
           (if (zero? n)
               (error 'wait-for
                      ,(format "after ~a seconds, ~s didn't come true" timeout sexp))
               (unless (test)
                 (sleep ,pause-time)
                 (loop (- n 1))))))))))

(define (wait-for sexp #:queue? [queue? #f]) 
  (wait-for/wrapper
   (lambda (x) x)
   (if queue?
       `(let ([t (λ () ,sexp)]
              [c (make-channel)])
          (queue-callback (λ () (channel-put c (t))))
          (channel-get c))
       sexp)))

(define (wait-for-new-frame sexp)
  (wait-for/wrapper
   (lambda (w)
     `(let ([frame (get-top-level-focus-window)])
        ,sexp
        ,w))
   `(not (eq? frame (get-top-level-focus-window)))))

(define (wait-for-frame name [eventspace #f])
  (let ([exp `(let ([win (get-top-level-focus-window)])
                (and win
                     (string=? (send win get-label) ,name)))])
    (if eventspace
        (wait-for
         `(parameterize ([current-eventspace ,eventspace])
            ,exp))
        (wait-for exp))))
