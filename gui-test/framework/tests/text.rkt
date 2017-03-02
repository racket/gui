#lang racket

(require "private/util.rkt"
         "private/gui.rkt"
         rackunit
         racket/gui/base
         framework)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing highlight-range method
;;



(check-equal?
 (let ([t (new text:basic%)])
   (send t insert "abc")
   (send t highlight-range 1 2 "red")
   (length (send t get-highlighted-ranges)))
 1)

(check-equal?
 (let ([t (new text:basic%)])
   (send t insert "abc")
   ((send t highlight-range 1 2 "red"))
   (length (send t get-highlighted-ranges)))
 0)

(check-equal?
 (let ([t (new text:basic%)])
   (send t insert "abc")
   (send t highlight-range 1 2 "red")
   (send t unhighlight-range 1 2 "red")
   (length (send t get-highlighted-ranges)))
 0)


(check-equal?
 (let ([t (new text:basic%)])
   (send t insert "abc")
   (send t highlight-range 1 2 "red")
   (send t highlight-range 1 2 "red")
   (send t unhighlight-range 1 2 "red")
   (length (send t get-highlighted-ranges)))
 1)

(check-equal?
 (let ([t (new text:basic%)])
   (send t insert "abc")
   (send t highlight-range 1 2 "red")
   (send t highlight-range 1 2 "red")
   (send t unhighlight-range 1 2 "red")
   (send t unhighlight-range 1 2 "red")
   (length (send t get-highlighted-ranges)))
 0)

(let ([tmp-file (path->string (make-temporary-file "fwtesttmp~a"))])
  (dynamic-wind
   void
   (λ ()
     (check-equal?
      (let ([t (new text:basic%)])
        (send t insert "abc")
        (send t save-file tmp-file)
        (send t highlight-range 0 3 "red")
        (call-with-output-file tmp-file
          (λ (port) (display "x\n" port))
          #:exists 'truncate)
        (send t load-file)
        (length (send t get-highlighted-ranges)))
      0))
   (λ () (delete-file tmp-file))))

(check-equal?
 (let ([t (new text:delegate%)])
   (send t insert "abc")
   (send t highlight-range 1 2 "red")
   (send t unhighlight-range 1 2 "red")
   (length (send t get-highlighted-ranges)))
 0)

(check-equal?
 (let ([t (new text:delegate%)])
   (send t set-delegate (new text:basic%))
   (send t insert "abc")
   (send t highlight-range 1 2 "red")
   (send t unhighlight-range 1 2 "red")
   (length (send t get-highlighted-ranges)))
 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing get-pos/text method
;;

(check-true
 (let* ([f (new frame% [label "Test frame"])]
        [t (new text:basic%)]
        [c (new editor-canvas% [parent f] [editor t])]
        [snip (make-object string-snip% "Test string")])
   (send t insert snip)
   (define-values (x-box y-box) (values (box 0) (box 0)))
   (send t get-snip-location snip x-box y-box)
   (send t local-to-global x-box y-box)
   (define event (new mouse-event% [event-type 'motion]
                      [x (add1 (unbox x-box))]
                      [y (add1 (unbox y-box))]))
   (let-values ([(pos edit) (send t get-pos/text event)])
     (and (real? pos) (is-a? edit text%)))))

(check-true
 (let* ([f (new frame% [label "Test frame"])]
        [t (new text:basic%)]
        [c (new editor-canvas% [parent f] [editor t])]
        [snip (make-object string-snip% "Test string")])
   (send t insert snip)
   (define-values (x-box y-box) (values (box 0) (box 0)))
   (send t get-snip-location snip x-box y-box)
   (send t local-to-global x-box y-box)
   (define event (new mouse-event% [event-type 'motion]
                      [x (+ 9999 (unbox x-box))]
                      [y (+ 9999 (unbox y-box))]))
   (let-values ([(pos edit) (send t get-pos/text event)])
     (and (false? pos) (false? edit)))))

(check-true
 (let* ([f (new frame% [label "Test frame"])]
        [t (new text:basic%)]
        [c (new editor-canvas% [parent f] [editor t])]
        [p (new pasteboard%)]
        [s-snip (make-object string-snip% "Test string")]
        [e-snip (new editor-snip% [editor p])])
   (send p insert s-snip)
   (send t insert e-snip)
   (define-values (x-box y-box) (values (box 0) (box 0)))
   (send t get-snip-location e-snip x-box y-box)
   (send t local-to-global x-box y-box)
   (define event (new mouse-event% [event-type 'motion]
                      [x (add1 (unbox x-box))]
                      [y (add1 (unbox y-box))]))
   (let-values ([(pos edit) (send t get-pos/text event)])
     (and (false? pos) (is-a? edit pasteboard%)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  all-string-snips<%>
;;

(check-true
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t all-string-snips?)))

(check-true
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t insert "xx")
   (send t all-string-snips?)))

(check-true
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t insert "xx")
   (send t delete 0 1)
   (send t all-string-snips?)))

(check-true
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t insert "xx")
   (send t delete 0 2)
   (send t all-string-snips?)))

(check-false
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t insert (new snip%))
   (send t all-string-snips?)))

(check-true
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t insert (new snip%))
   (send t delete 0 1)
   (send t all-string-snips?)))

(check-false
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t insert (new snip%))
   (send t insert (new snip%))
   (send t delete 0 1)
   (send t all-string-snips?)))

(check-false
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t insert (new snip%))
   (send t insert "abcdef")
   (send t insert (new snip%))
   (send t delete 2 4)
   (send t all-string-snips?)))

(check-false
 (let ()
   (define t (new (text:all-string-snips-mixin text%)))
   (send t insert "abcdef\n")
   (send t insert (new snip%) (send t last-position))
   (send t all-string-snips?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  searching
;;

(define (run-search-test setup-code)
  (define answer (make-channel))
  (parameterize ([current-eventspace (make-eventspace)])
    (queue-callback
     (λ ()
       (define t (new text:searching%))
       (setup-code t)
       (let loop ()
         (cond
           [(send t search-updates-pending?)
            (queue-callback (λ () (loop)) #f)]
           [else
            (define-values (before total) (send t get-search-hit-count))
            (channel-put answer (list before total))])))))
  (channel-get answer))

(check-equal?
 (run-search-test
  (λ (t)
    (send t insert "abc")
    (send t set-position 0 0)
    (send t set-searching-state "b" #f #f)))
 (list 0 1))

(check-equal?
 (run-search-test
  (λ (t)
    (send t insert "abc")
    (send t set-position 3 3)
    (send t set-searching-state "b" #f #f)))
 (list 1 1))

(check-equal?
 (run-search-test
  (λ (t)
    (send t insert "abc")
    (define t2 (new text%))
    (send t2 insert "abc")
    (send t insert (new editor-snip% [editor t2]))
    (send t2 insert "abc")
    (send t set-position 0 0)
    (send t set-searching-state "b" #f #f)))
 (list 0 3))

(check-equal?
 (run-search-test
  (λ (t)
    (send t insert "abc")
    (define t2 (new text%))
    (send t2 insert "abc")
    (send t insert (new editor-snip% [editor t2]))
    (send t insert "abc")
    (send t set-position (send t last-position) (send t last-position))
    (send t set-searching-state "b" #f #f)))
 (list 3 3))

(check-equal?
 (run-search-test
  (λ (t)
    (send t insert "abc")
    (define t2 (new text%))
    (send t2 insert "abc")
    (define t3 (new text%))
    (send t3 insert "abc")
    (send t2 insert (new editor-snip% [editor t3]))
    (send t2 insert "abc")
    (send t insert (new editor-snip% [editor t2]))
    (send t insert "abc")
    (send t set-position (send t last-position) (send t last-position))
    (send t set-searching-state "b" #f #f)))
 (list 5 5))

(check-equal?
 (run-search-test
  (λ (t)
    (send t insert "abc")
    (define t2 (new text%))
    (send t2 insert "abc")
    (define t3 (new text%))
    (send t3 insert "abc")
    (send t2 insert (new editor-snip% [editor t3]))
    (send t2 insert "abc")
    (send t insert (new editor-snip% [editor t2]))
    (send t insert "abc")
    (send t set-position 0 0)
    (send t set-searching-state "b" #f #f)))
 (list 0 5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  print-to-dc
;;

(check-not-exn
 (λ ()
   (define t (new text:basic%))
   (define bmp (make-object bitmap% 100 40))
   (define dc (new bitmap-dc% (bitmap bmp)))
   (send t insert "Hello world")
   (send dc clear)
   (send t print-to-dc dc 1)))


(check-not-exn
 (λ ()
   (define f (new frame% [label ""]))
   (define t (new text:basic%))
   (define ec (new editor-canvas% [parent f] [editor t]))
   (define bmp (make-object bitmap% 100 40))
   (define dc (new bitmap-dc% (bitmap bmp)))
   (send t insert "Hello world")
   (send t highlight-range 2 5 "orange")
   (send f reflow-container)
   (send dc clear)
   (send t print-to-dc dc 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  text:ports
;;

;; there is an internal buffer of this size, so writes that are larger and smaller are interesting
(define buffer-size 4096)

(let ()
  (define big-str
    (build-string (* buffer-size 2) (λ (i) (integer->char (+ (modulo i 26) (char->integer #\a))))))
  (define non-ascii-str "λαβ一二三四五")
  
  (define (do/separate-thread str mtd)
    (let* ([t (new (text:ports-mixin text:wide-snip%))]
           [op (case mtd
                 [(get-out-port) (send t get-out-port)]
                 [(get-err-port) (send t get-err-port)]
                 [(get-value-port) (send t get-value-port)])]
           [exn #f])
      (yield
       (thread
        (λ () 
          (with-handlers ((exn:fail? (λ (x) (set! exn x))))
            (display str op)
            (flush-output op)))))
      (when exn (raise exn))
      (send t get-text 0 (send t last-position))))

  (check-equal?
   (do/separate-thread "abc" 'get-out-port)
   "abc")

  (check-equal?
   (do/separate-thread big-str 'get-out-port)
   big-str)
  
  (check-equal?
   (do/separate-thread non-ascii-str 'get-out-port)
   non-ascii-str)
  
  (check-equal?
   (do/separate-thread "abc" 'get-err-port)
   "abc")
  
  (check-equal?
   (do/separate-thread big-str 'get-err-port)
   big-str)
  
  (check-equal?
   (do/separate-thread non-ascii-str 'get-err-port)
   non-ascii-str)

  (check-equal? 
   (do/separate-thread "abc" 'get-value-port)
   "abc")
  
  (check-equal?
   (do/separate-thread big-str 'get-value-port) 
   big-str)
  
  (check-equal?
   (do/separate-thread non-ascii-str 'get-value-port)
   non-ascii-str)
  
  
;; display the big string, one char at a time
  (check-equal?
   (let* ([t (new (text:ports-mixin text:wide-snip%))]
          [op (send t get-out-port)]
          [exn #f])
     (yield
      (thread
       (λ () 
         (with-handlers ((exn:fail? (λ (x) (set! exn x))))
           (let loop ([i 0])
             (when (< i (string-length big-str))
               (display (string-ref big-str i) op)
               (loop (+ i 1))))
           (flush-output op)))))
     (when exn (raise exn))
     (send t get-text 0 (send t last-position)))
   big-str)
  
  
  (let ([s "五"])
    (define bts (string->bytes/utf-8 s))
    (check-equal?
     (let ()
       (define t (new (text:ports-mixin text:wide-snip%)))
       (define p (send t get-out-port))
       (write-bytes (bytes (bytes-ref bts 0)) p)
       (flush-output p)
       (void (write-bytes (subbytes bts 1 (bytes-length bts)) p))
       (flush-output p)
       (send t get-text))
     s))
  
  (let ([b (bytes 195 195 (char->integer #\a))])
    (check-equal?
     (let ()
       (define t (new (text:ports-mixin text:wide-snip%)))
       (define p (send t get-out-port))
       (yield
        (thread
         (λ ()
           (write-bytes b p)
           (flush-output p))))
       (send t get-text))
     (let ()
       (define c (bytes-open-converter "UTF-8-permissive" "UTF-8"))
       (define-values (result-bytes src-read-amt termination) (bytes-convert c b))
       (bytes->string/utf-8 result-bytes))))

  ;; the next tests test the interaction when the current
  ;; thread is the same as the handler thread of the eventspace
  ;; where the text was created
  
  (check-equal?
   (let* ([t (new (text:ports-mixin text:wide-snip%))]
          [op (send t get-out-port)]
          [exn #f])
     (display "abc" op)
     (flush-output op)
     (send t get-text 0 (send t last-position)))
   "abc")
  
  (check-equal?
   (let* ([t (new (text:ports-mixin text:wide-snip%))]
          [op (send t get-out-port)])
     (display big-str op)
     (flush-output op)
     (send t get-text 0 (send t last-position)))
   big-str)
  
  (check-equal?
   (let* ([t (new (text:ports-mixin text:wide-snip%))]
          [op (send t get-out-port)])
     (display non-ascii-str op)
     (flush-output op)
     (send t get-text 0 (send t last-position)))
   non-ascii-str)
  
  (check-equal?
   (let* ([t (new (text:ports-mixin text:wide-snip%))]
          [op (send t get-out-port)])
     (display non-ascii-str op)
     (flush-output op)
     (send t get-text 0 (send t last-position)))
   non-ascii-str)


  ;; This test sends a lot of flushes from a separate thread and,
  ;; while doing that, sends a `clear-output-ports` from the
  ;; eventspace main thread where the text was created. The goal
  ;; is to make sure there is no deadlock for this interaction.
  (check-pred
   (λ (x)
     ;; we know we're going to get all 'a's, but some of
     ;; the output could be discarded by `clear-output-ports`
     (and (regexp-match? #rx"^a*$" x)
          (<= 100 (string-length x) 200)))
   (let ()
     (define es (make-eventspace))
     (define-values (text port)
       (let ()
         (define c (make-channel))
         (parameterize ([current-eventspace es])
           (queue-callback
            (λ ()
              (define t
                (new (text:ports-mixin
                      (text:wide-snip-mixin
                       text:basic%))))
              (channel-put c t)
              (channel-put c (send t get-out-port)))))
         (values (channel-get c)
                 (channel-get c))))
     (define clear-output-go (make-semaphore 0))
     (define clear-output-done (make-semaphore 0))
     (void
      (thread
       (λ ()
         (semaphore-wait clear-output-go)
         (parameterize ([current-eventspace es])
           (queue-callback
            (λ ()
              (send text clear-output-ports)
              (semaphore-post clear-output-done)))))))
     (for ([x (in-range 100)])
       (display #\a port)
       (flush-output port))
     (semaphore-post clear-output-go)
     (for ([x (in-range 100)])
       (display #\a port)
       (flush-output port))
     (semaphore-wait clear-output-done)
     (send text get-text))))

(check-pred
 (λ (x+y)
   (equal? (list-ref x+y 0)
           (list-ref x+y 1)))
 (let ()
   (define t (new (text:ports-mixin
                   (editor:standard-style-list-mixin
                    text:wide-snip%))))
       
   (send t set-max-undo-history 'forever)
   (define last-undo? #f)
   (send t add-undo (λ () (set! last-undo? #t)))
       
   (define vp (send t get-value-port))
   (define op (send t get-out-port))
       
   (display "1" vp)
   (display "2" op)
   (flush-output vp)
   (flush-output op)
       
   (define (to-vec c) (vector (send c red) (send c green) (send c blue)))
       
   (define (get-colors)
     (let loop ([s (send t find-first-snip)])
       (cond
         [s (cons (list (send s get-text 0 (send s get-count))
                        (to-vec (send (send s get-style) get-foreground)))
                  (loop (send s next)))]
         [else '()])))
       
   (define before (get-colors))
   (let loop ()
     (unless last-undo?
       (send t undo)
       (loop)))
   (define after (get-colors))
   (list before after)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; move/copy-to-edit tests
;;

(define (get-snips text)
    (define the-snips
      (let loop ([current (send text find-first-snip)]
                 [snips '()])
        (if current
            (loop (send current next) (cons current snips))
            (reverse snips))))
    (for/list ([snip (in-list the-snips)]
               [i (in-naturals)])
      (send snip get-text 0 (send snip get-count))))

(define (edit-style t posns)
  (define (maybe-cdr p) (if (empty? p) '() (cdr p)))
  (for ([i (in-list posns)]
        [j (in-list (append (maybe-cdr posns) '(end)))]
        [s (in-naturals)])
    (define sd (make-object style-delta%
                 (if (even? s) 'change-normal 'change-normal-color)))
    (send t change-style sd i j)))

(check-equal? (let ([t (new text%)])
                (send t insert "ABCDEF")
                (edit-style t '(3))
                (get-snips t))
              '("ABC" "DEF"))

(check-equal? (let ([t (new text%)])
                (send t insert "ABCDEFGH")
                (edit-style t '(3 5))
                (get-snips t))
              '("ABC" "DE" "FGH"))

(define (edit-string str start end dest-pos [this? #f] [style #f] #:try-to-move? [try-to-move? #f])
  (define t1 (new text:basic%))
  (define t2 (if this? t1 (new text:basic%)))
  (send t1 insert str)
  (when style
    (edit-style t1 style))
  (send t1 move/copy-to-edit t2
        start
        end
        (if this? dest-pos 0)
        #:try-to-move? try-to-move?)
  (unless this?
    (send t2 move/copy-to-edit t1 0 (send t2 get-end-position) dest-pos #:try-to-move? #t))
  (send t1 get-text))

(check-equal? (edit-string "ABC" 2 3 0 #t '(0 1 2)) "CABC")
(check-equal? (edit-string "ABC" 2 3 0 #t #f) "CABC")
(check-equal? (edit-string "ABC" 2 3 0 #f '(0 1 2)) "CABC")
(check-equal? (edit-string "ABC" 2 3 0 #f #f) "CABC")
(check-equal? (edit-string "ABC" 2 3 0 #t '(0 1 2) #:try-to-move? #t) "CAB")
(check-equal? (edit-string "ABC" 2 3 0 #t #f #:try-to-move? #t) "CAB")
(check-equal? (edit-string "ABC" 2 3 0 #f '(0 1 2) #:try-to-move? #t) "CAB")
(check-equal? (edit-string "ABC" 2 3 0 #f #f #:try-to-move? #t) "CAB")
(check-equal? (edit-string "X" 0 0 0 #t #f) "X")
(check-equal? (edit-string "ji" 0 0 0 #t #f) "ji")

(check-exn
 #rx"expected dest-pos outside of start to end range"
 (thunk (edit-string "ABCDE" 2 4 3 #t #f)))
(check-exn
 #rx"expected dest-pos outside of start to end range"
 (thunk (edit-string "ABCDE" 2 4 3 #t '(0 1 2 3 4))))
(check-exn
 #rx"expected dest-pos outside of start to end range"
 (thunk (edit-string "ABCDE" 2 4 3 #t #f #:try-to-move? #t)))
(check-exn
 #rx"expected dest-pos outside of start to end range"
 (thunk (edit-string "ABCDE" 2 4 3 #t '(0 1 2 3 4) #:try-to-move? #t)))

(define (random-sequence n [div 2])
  (for/list ([i (in-range n)]
             #:when (zero? (random div)))
    i))

(define (random-check-edit-string)
  (define letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (define str
    (list->string
     (for/list ([i (in-range (add1 (random 1000)))])
       (define ix (random (string-length letters)))
       (string-ref letters ix))))
  (define strlen (string-length str))
  (define start (random strlen))
  (define end (+ start (random (add1 (- strlen start)))))
  (define dest-pos (if (zero? (random 2))
                       (random (add1 start))
                       (+ end (random (add1 (- strlen end))))))
  (define random-style (random-sequence (add1 strlen)))
  (check-equal?
   (edit-string str start end dest-pos #t random-style)
   (string-append (substring str 0 dest-pos)
                  (substring str start end)
                  (substring str dest-pos strlen))))
(for ([i (in-range 5000)])
  (random-check-edit-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ascii art boxes
;;

(define (ascii-art-enlarge-boxes before position overwrite? chars)
  (define t (new (text:ascii-art-enlarge-boxes-mixin text%)))
  (send t set-ascii-art-enlarge #t)
  (define f (new frame% [label ""]))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send t insert before)
  (send t set-position position position)
  (when overwrite? (send t set-overwrite-mode #t))
  (for ([char (in-list chars)])
    (send ec on-char (new key-event% [key-code char])))
  (send t get-text))


(check-equal? (ascii-art-enlarge-boxes
               (string-append
                "╔═╦═╗\n"
                "║ ║ ║\n"
                "╠═╬═╣\n"
                "║ ║ ║\n"
                "╚═╩═╝\n")
               7 #t '(#\a))
              (string-append
               "╔═╦═╗\n"
               "║a║ ║\n"
               "╠═╬═╣\n"
               "║ ║ ║\n"
               "╚═╩═╝\n"))

(check-equal? (ascii-art-enlarge-boxes
               (string-append
                "╔═╦═╗\n"
                "║ ║ ║\n"
                "╠═╬═╣\n"
                "║ ║ ║\n"
                "╚═╩═╝\n")
               7 #t'(#\a #\b))
              (string-append
               "╔══╦═╗\n"
               "║ab║ ║\n"
               "╠══╬═╣\n"
               "║  ║ ║\n"
               "╚══╩═╝\n"))

(check-equal? (ascii-art-enlarge-boxes
               (string-append
                "╔═╦═╗\n"
                "║ ║ ║\n"
                "╠═╬═╣\n"
                "║ ║ ║\n"
                "╚═╩═╝\n")
               7 #f '(#\a))
              (string-append
               "╔══╦═╗\n"
               "║a ║ ║\n"
               "╠══╬═╣\n"
               "║  ║ ║\n"
               "╚══╩═╝\n"))

(check-equal? (ascii-art-enlarge-boxes
               (string-append
                "╔═╦═╗\n"
                "║ ║ ║\n"
                "║ ║ ║\n"
                "╠═╬═╣\n"
                "║ ║ ║\n"
                "╚═╩═╝\n")
               14 #f '(#\f))
              (string-append
               "╔══╦═╗\n"
               "║  ║ ║\n"
               "║ f║ ║\n"
               "╠══╬═╣\n"
               "║  ║ ║\n"
               "╚══╩═╝\n"))
