#lang racket

(require "private/util.rkt"
         "private/gui.rkt"
         rackunit
         racket/gui/base
         framework
         simple-tree-text-markup/data)

(module+ test
  (with-private-prefs
    (highlight-range-tests)
    (port-name-matches-tests)
    (get-pos/text-tests)
    (all-string-snips-tests)
    (normalize-paste-tests)
    (print-to-dc-tests)
    (searching-tests)
    (text:ports-tests)
    (move/copy-to-edit-tests)
    (move/copy-to-edit-random-tests)
    (ascii-art-tests)
    (autocomplete-tests)))

(define (highlight-range-tests)
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

  (let ()
    (define t (new text:basic%))
    (define red (send the-color-database find-color "red"))
    (send t highlight-range 0 1 red #f 'low 'ellipse)
    (define r (list-ref (send t get-highlighted-ranges) 0))
    (check-equal? (text:range-start r) 0)
    (check-equal? (text:range-end r) 1)
    (check-equal? (text:range-caret-space? r) #f)
    (check-equal? (text:range-style r) 'ellipse)
    (check-equal? (text:range-color r) red)))

(define (port-name-matches-tests)
  (let ()
    (define t (new text:basic%))
    (send t set-filename "/a/path/that/does/not/exist/x.rkt")
    (check-true (send t port-name-matches? (string->path "/a/path/that/does/not/exist/x.rkt"))))

  (let ()
    (define t (new text:basic%))
    (send t set-filename "/a/path/that/does/not/exist/x.rkt")
    (check-false (send t port-name-matches? (string->path "/a/path/that/does/not/exist/y.rkt")))))

(define (get-pos/text-tests)
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
       (and (false? pos) (is-a? edit pasteboard%))))))

(define (all-string-snips-tests)
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
     (send t all-string-snips?))))

(require (only-in framework/private/text-normalize-paste as-a-paste))

(define (normalize-paste-tests)
  (preferences:set 'framework:ask-about-paste-normalization #f)

  (let ()
    (preferences:set 'framework:do-paste-normalization #f)
    (define t (new (text:normalize-paste-mixin text:basic%)))
    (send t as-a-paste (λ () (send t insert "a")))
    (check-equal? (send t get-text) "a"))

  (let ()
    (preferences:set 'framework:do-paste-normalization #t)
    (define t (new (text:normalize-paste-mixin text:basic%)))
    (send t as-a-paste (λ () (send t insert "a")))
    (check-equal? (send t get-text) "a"))

  (let ()
    (preferences:set 'framework:do-paste-normalization #f)
    (define t (new (text:normalize-paste-mixin text:basic%)))
    (send t as-a-paste (λ () (send t insert "x²")))
    (check-equal? (send t get-text) "x²"))

  (let ()
    (preferences:set 'framework:do-paste-normalization #t)
    (define t (new (text:normalize-paste-mixin text:basic%)))
    (send t as-a-paste (λ () (send t insert "x²")))
    (check-equal? (send t get-text) "x2"))

  (let ()
    (preferences:set 'framework:do-paste-normalization #t)
    (define t (new (text:normalize-paste-mixin text:basic%)))
    (send t as-a-paste (λ () (send t insert "x² x²\nx² x²\nx² x²\nx² x²\n")))
    (check-equal? (send t get-text) "x2 x2\nx2 x2\nx2 x2\nx2 x2\n"))

  (let ()
    (preferences:set 'framework:do-paste-normalization #t)
    (define t (new (text:normalize-paste-mixin text:basic%)))
    (send t as-a-paste (λ ()
                         (send t insert "x²\n")
                         (send t insert "x²\n")))
    (check-equal? (send t get-text) "x2\nx2\n"))

  (let ()
    (preferences:set 'framework:do-paste-normalization #t)
    (define t (new (text:normalize-paste-mixin text:basic%)))
    (send t as-a-paste (λ ()
                         (send t insert "x²\n")
                         (send t insert "x³\n")))
    (check-equal? (send t get-text) "x2\nx3\n"))

  (let ()
    (preferences:set 'framework:do-paste-normalization #t)
    (define t (new (text:normalize-paste-mixin text:basic%)))
    (send t as-a-paste (λ ()
                         (send t insert "x²\n")
                         (send t insert "x³\n")
                         (send t insert "x³\n")))
    (check-equal? (send t get-text) "x2\nx3\nx3\n")))

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

(define (searching-tests)
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

  (check-equal?
   (run-search-test
    (λ (t)
      (send t insert "abc abc abc")
      (send t set-position 0 0)
      (send t set-searching-state "abc" #f #t)))
   (list 1 3)))

(define (print-to-dc-tests)
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
     (send t print-to-dc dc 1))))

;; there is an internal buffer of this size, so writes that are larger and smaller are interesting
(define buffer-size 4096)

(define (text:ports-tests)
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
     (send text get-text)))

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

  (let ()
    (define t (new (text:ports-mixin text:wide-snip%)))
    (define op (send t get-out-port))
    (write-special (horizontal-markup (list "a" "b" "c")) op)
    (flush-output op)
    (check-equal? (send t get-text) "abc"))

  (let ()
    (define t (new (text:ports-mixin text:wide-snip%)))
    (define op (send t get-out-port))
    (write-special (vertical-markup (list "a" "b" "c")) op)
    (flush-output op)
    (check-equal? (send t get-text) "a\nb\nc"))

  (let ()
    (define t (new (text:ports-mixin text:wide-snip%)))
    (define op (send t get-out-port))
    (write-special (framed-markup (horizontal-markup (list "a" "b" "c"))) op)
    (flush-output op)
    (check-true (is-a? (send t find-first-snip) editor-snip%))
    (check-equal? (send (send (send t find-first-snip) get-editor) get-text) "abc"))

  (let ()
    (define t (new (text:ports-mixin text:wide-snip%)))
    (define op (send t get-out-port))
    (define w 4)
    (define h 6)
    (define bmp (make-bitmap w h))
    (define pixels (make-bytes (* w h 4)))
    (for ([x (in-range (bytes-length pixels))])
      (bytes-set! pixels
                  x
                  (if (zero? (modulo x 4))
                      255
                      (modulo x 255))))
    (send bmp set-argb-pixels 0 0 w h pixels)
    (write-special (horizontal-markup (list "a" (image-markup bmp "x") "b")) op)
    (flush-output op)
    (check-equal? (send t get-text) "a.b")
    (define image-snip (send (send t find-first-snip) next))
    (check-true (is-a? image-snip image-snip%))
    (define bmp2 (send image-snip get-bitmap))
    (check-false (object=? bmp bmp2))
    (check-equal? (send bmp2 get-width) w)
    (check-equal? (send bmp2 get-height) h)
    (define pixels2 (make-bytes (*  w h 4)))
    (send bmp2 get-argb-pixels 0 0 w h pixels2)
    (check-equal? pixels pixels2)
    )

  (let ()
    (define t (new (text:ports-mixin text:wide-snip%)))
    (define op (send t get-out-port))
    (write-special (horizontal-markup
                    (list
                     (number-markup 1/3 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'decimal)
                     " "
                     (number-markup 4/3 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'mixed)
                     " "
                     (number-markup 4/3 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'improper)
                     " "
                     (number-markup #i0.5 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'decimal)
                     " "
                     (number-markup #e0.5 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'decimal)))
                   op)
    (flush-output op)
    (check-equal? (send t get-text) "0.3 1 1/3 4/3 #i0.5 #e0.5")
    (define snip1 (send t find-first-snip))
    (check-true (number-snip:is-number-snip? snip1))
    (check-equal? (send snip1 get-text 0 10 #t) "0.3")
    (define snip2 (send (send snip1 next) next))
    (check-true (number-snip:is-number-snip? snip2))
    (check-equal? (send snip2 get-text 0 10 #t) "1 1/3")
    (define snip3 (send (send snip2 next) next))
    (check-true (number-snip:is-number-snip? snip3))
    (check-equal? (send snip3 get-text 0 10 #t) "4/3"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 
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


(define (edit-string/text str start end dest-pos [this? #f] [style #f]
                     #:try-to-move? [try-to-move? #f]
                     #:contract-this? [contract-this? #f])
  (define t1 (new text:basic%))
  (define t2 (if this?
                 (if contract-this? (contract (object/c) t1 'p 'n) t1)
                 (new text:basic%)))
  (send t1 insert str)
  (when style
    (edit-style t1 style))
  (send t1 move/copy-to-edit t2
        start
        end
        (if this? dest-pos 0)
        #:try-to-move? try-to-move?)
  (unless this?
    (send t2 move/copy-to-edit
          t1
          0
          (send t2 get-end-position)
          (cond
            [(and try-to-move? (>= dest-pos end))
             (- dest-pos (- end start))]
            [(and try-to-move? (>= dest-pos start))
             start]
            [else dest-pos])
          #:try-to-move? #t))
  (send t1 get-text))

(define (move/copy-to-edit-tests)
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

  (check-equal? (edit-string/text "lR" 0 2 1 #f '(1) #:try-to-move? #t) "lR")
  (check-equal? (edit-string/text "DaMoe" 0 5 1 #:try-to-move? #t) "DaMoe")
  (check-equal? (edit-string/text "lR" 0 2 1 #f '(1) #:try-to-move? #t) "lR")
  (check-equal? (edit-string/text "ABC" 2 3 0 #t '(0 1 2)) "CABC")
  (check-equal? (edit-string/text "ABC" 2 3 0 #t #f) "CABC")
  (check-equal? (edit-string/text "ABC" 2 3 0 #f '(0 1 2)) "CABC")
  (check-equal? (edit-string/text "ABC" 2 3 0 #f #f) "CABC")
  (check-equal? (edit-string/text "ABC" 2 3 0 #t '(0 1 2) #:try-to-move? #t) "CAB")
  (check-equal? (edit-string/text "ABC" 2 3 0 #t #f #:try-to-move? #t) "CAB")
  (check-equal? (edit-string/text "ABC" 2 3 0 #f '(0 1 2) #:try-to-move? #t) "CAB")
  (check-equal? (edit-string/text "ABC" 2 3 0 #f #f #:try-to-move? #t) "CAB")
  (check-equal? (edit-string/text "X" 0 0 0 #t #f) "X")
  (check-equal? (edit-string/text "ji" 0 0 0 #t #f) "ji")
  (check-equal? (edit-string/text "ABCDE" 2 4 4 #t #f) "ABCDCDE")
  (check-equal? (edit-string/text "ABCDE" 2 4 2 #t #f) "ABCDCDE")

  (check-equal? (edit-string/text "ABCDE" 2 4 3 #t #f) "ABCCDDE")
  (check-equal? (edit-string/text "ABCDE" 2 4 3 #t '(0 1 2 3 4)) "ABCCDDE")
  (check-equal? (edit-string/text "ABCDE" 2 4 3 #t #f #:try-to-move? #t) "ABCDE")
  (check-equal? (edit-string/text "ABCDE" 2 4 3 #t '(0 1 2 3 4) #:try-to-move? #t) "ABCDE")

  (check-exn
   #rx"expected start position smaller than end position"
   (thunk (edit-string/text "ABCDE" 4 2 0 #t #f)))
  (check-exn
   #rx"expected start position smaller than end position"
   (thunk (edit-string/text "ABCDE" 4 2 0 #t '(0 1 2 3 4))))
  (check-exn
   #rx"expected start position smaller than end position"
   (thunk (edit-string/text "ABCDE" 4 2 0 #t #f #:try-to-move? #t)))
  (check-exn
   #rx"expected start position smaller than end position"
   (thunk (edit-string/text "ABCDE" 4 2 0 #t '(0 1 2 3 4) #:try-to-move? #t)))

  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" -1 4 4 #t #f)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" -1 4 4 #t '(0 1 2 3 4))))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" -1 4 4 #t #f #:try-to-move? #t)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" -1 4 4 #t '(0 1 2 3 4) #:try-to-move? #t)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" 0 -1 4 #t #f)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" 0 -1 4 #t '(0 1 2 3 4))))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" 0 -1 4 #t #f #:try-to-move? #t)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" 0 -1 4 #t '(0 1 2 3 4) #:try-to-move? #t)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" 0 4 -1 #t #f)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" 0 4 -1 #t '(0 1 2 3 4))))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" 0 4 -1 #t #f #:try-to-move? #t)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" 0 4 -1 #t '(0 1 2 3 4) #:try-to-move? #t)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" -1 -1 0 #t #f)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" -1 -1 0 #t '(0 1 2 3 4))))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" -1 -1 0 #t #f #:try-to-move? #t)))
  (check-exn
   #rx"expected start, end, and dest-pos to be non-negative"
   (thunk (edit-string/text "ABCDE" -1 -1 0 #t '(0 1 2 3 4) #:try-to-move? #t)))

  ;; non-string snips
  (let ([t (new text:basic%)])
    (define snip (make-object image-snip% (collection-file-path "plt.gif" "icons")))
    (send t insert "ABCD")
    (check-equal? (send t get-text) "ABCD")
    (send t insert snip 0)
    (check-equal? (send t get-text) ".ABCD")
    (send t move/copy-to-edit t 0 3 5 #:try-to-move? #t)
    (check-equal? (send t get-text) "CD.AB")
    (check-equal? (send t get-snip-position snip) 2)
    (check-equal? (send t find-snip 2 'after) snip))

  (let ([t (new text:basic%)])
    (define snip (make-object image-snip% (collection-file-path "plt.gif" "icons")))
    (send t insert "ABCD")
    (send t insert snip 0)
    (send t move/copy-to-edit t 0 3 5 #:try-to-move? #f)
    (check-equal? (send t get-text) ".ABCD.AB")
    (check-equal? (send t get-snip-position snip) 0)
    (check-equal? (send t find-snip 0 'after) snip))

  (let ([t1 (new text:basic%)]
        [t2 (new text:basic%)])
    (define snip (make-object image-snip% (collection-file-path "plt.gif" "icons")))
    (send t1 insert "ABCD")
    (send t1 insert snip 0)
    (send t1 move/copy-to-edit t2 0 3 0 #:try-to-move? #t)
    (check-equal? (send t1 get-text) "CD")
    (check-equal? (send t2 get-text) ".AB")
    (check-equal? (send t2 get-snip-position snip) 0)
    (check-equal? (send t2 find-snip 0 'after) snip))

  (let ([t1 (new text:basic%)]
        [t2 (new text:basic%)])
    (define snip (make-object image-snip% (collection-file-path "plt.gif" "icons")))
    (send t1 insert "ABCD")
    (send t1 insert snip 0)
    (send t1 move/copy-to-edit t2 0 3 0 #:try-to-move? #f)
    (check-equal? (send t1 get-text) ".ABCD")
    (check-equal? (send t2 get-text) ".AB")))

;; Random Tests
(define (random-sequence n [div 2])
  (for/list ([i (in-range n)]
             #:when (zero? (random div)))
    i))

(define (edit-string str start end dest-pos move?)
  (define strlen (string-length str))
  (define sub (substring str start end))
  (cond
    [(and move? (<= start dest-pos end)) str]
    [else
     (string-append
      (if (and move? (<= end dest-pos))
          (string-append
           (substring str 0 start)
           (substring str end dest-pos))
          (substring str 0 dest-pos))
      sub
      (if (and move? (<= dest-pos start))
          (string-append
           (substring str dest-pos start)
           (substring str end strlen))
          (substring str dest-pos strlen)))]))

(define (random-string n)
  (define letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (list->string
   (for/list ([i (in-range (add1 (random n)))])
       (define ix (random (string-length letters)))
       (string-ref letters ix))))

(define (random-check-edit-string)
  (define str (random-string 1000))
  (define strlen (string-length str))
  (define start (random strlen))
  (define end (+ start (random (add1 (- strlen start)))))
  (define dest-pos (random (add1 strlen)))
  (define random-style (random-sequence (add1 strlen)))
  (define this? (zero? (random 2)))
  (define contract-this? (zero? (random 2)))
  (define try-to-move? (zero? (random 2)))
  (check-equal?
   (edit-string/text str start end dest-pos this? random-style
                     #:try-to-move? try-to-move?
                     #:contract-this? contract-this?)
   (edit-string str start end dest-pos try-to-move?)))


(define (check-move/copy+delete-property)
  (define t1 (new text:basic%))
  (define t2 (new text:basic%))
  (define str (random-string 1000))
  (define strlen (string-length str))
  (define start (random strlen))
  (define end (+ start (random (add1 (- strlen start)))))
  (define dest-pos (random (add1 strlen)))
  (define random-style (random-sequence (add1 strlen)))
  (send t1 insert str)
  (send t2 insert str)
  (edit-style t1 random-style)
  (edit-style t2 random-style)
  (send t1 move-to t1 start end dest-pos)
  (send t2 copy-to t2 start end dest-pos)
  (cond
    [(<= start dest-pos end)
     (send t2 delete (+ dest-pos (- end start)) (+ end (- end start)))
     (send t2 delete start dest-pos)]
    [(<= dest-pos start)
     (send t2 delete (+ start (- end start)) (+ end (- end start)))]
    [(<= end dest-pos)
     (send t2 delete start end)])
  (check-equal? (send t1 get-text) (send t2 get-text)))

(define (move/copy-to-edit-random-tests)
  (check-equal? (edit-string "ABCDEF" 1 3 0 #f) "BCABCDEF")
  (check-equal? (edit-string "ABCDEF" 1 3 0 #t) "BCADEF")
  (check-equal? (edit-string "ABCDEF" 1 3 5 #f) "ABCDEBCF")
  (check-equal? (edit-string "ABCDEF" 1 3 5 #t) "ADEBCF")
  (for ([i (in-range 10)])
    (random-check-edit-string))
  (for ([i (in-range 10)])
    (check-move/copy+delete-property)))

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

(define (ascii-art-tests)
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
                 "╚══╩═╝\n")))

(define (autocomplete-tests)
  (define t (new (class (text:autocomplete-mixin text%)
                   (define/override (get-all-words)
                     '("abcd"))
                   (super-new))))
  (define f (new frame% [label ""]))
  (define ec (new editor-canvas% [parent f] [editor t]))

  (send t insert "pqr abc xyz")
  (send t set-position 7 7)
  (send t auto-complete)
  (send ec on-char (new key-event%
                        [key-code #\backspace]))
  (send ec on-char (new key-event%
                        [key-code #\return]))

  (check-equal? (send t get-text) "pqr abcd xyz"))
