(module mem mzscheme
  (require "test-suite-utils.ss")

; mem-boxes : (list-of (list string (list-of (weak-box TST))))
(send-sexp-to-mred '(define mem-boxes null))

(define mem-count 10)

(define (test-allocate tag open close)
  (send-sexp-to-mred
   `(let ([new-boxes
           (let loop ([n ,mem-count])
             (cond
               [(zero? n) null]
               [else
                (let* ([o (,open)]
                       [b (make-weak-box o)])
                  (,close o)
                  (cons b (loop (- n 1))))]))])
      (sleep/yield 1/10) (collect-garbage)
      (sleep/yield 1/10) (collect-garbage)
      (sleep/yield 1/10) (collect-garbage)
      (set! mem-boxes (cons (list ,tag new-boxes) mem-boxes)))))

(define (done)
  (send-sexp-to-mred
   `(begin
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (yield) (collect-garbage)
      (let ([f (make-object dialog% "Results")]
            [anything? #f])
        (for-each
         (lambda (boxl)
           (let* ([tag (car boxl)]
                  [boxes (cadr boxl)]
                  [calc-results
                   (lambda ()
		     (let loop ([boxes boxes]
				[n 0])
		       (cond
			 [(null? boxes) n]
			 [else (if (weak-box-value (car boxes))
				   (loop (cdr boxes) (+ n 1))
				   (loop (cdr boxes) n))])))])
	     (let loop ([tries 16])
	       (unless (zero? tries)
		 (when (> (calc-results) 0)
		   (yield) (yield) (sleep/yield 1/5) (collect-garbage)
		   (loop (- tries 1)))))
             (let ([res (calc-results)])
               (when (> res 0)
                 (set! anything? #t)
                 (make-object message% (format "~a: ~a of ~a~n" tag res ,mem-count) f)))))
         (reverse mem-boxes))
        (cond
          [anything? (make-object button% "Close" f (lambda x (send f show #f)))]
          [else (make-object button% "NOTHING!" f (lambda x (send f show #f)))])
        (send f show #t)))))

(define (test-frame-allocate %)
  (let ([name (symbol->string %)])
    (send-sexp-to-mred '(preferences:set 'framework:exit-when-no-frames #f))
    (test-allocate name
		   `(lambda () (let ([f (make-object ,% ,name)])
				 (send f show #t)
				 (yield) (yield)
				 f))
		   `(lambda (f)
		      (yield) (yield)
		      (send f close)
		      (when (send f is-shown?)
			(error 'test-frame-allocate "~a instance didn't close" ',%))
		      (yield) (yield)))
    (send-sexp-to-mred '(preferences:set 'framework:exit-when-no-frames #t))))

(test-allocate "frame%"
               '(lambda () (let ([f (make-object frame% "test frame")])
                            (send f show #t)
                            f))
	       '(lambda (f) (send f show #f)))


(define (test-editor-allocate object-name)
  (test-allocate (symbol->string object-name)
		 `(lambda () (make-object ,object-name))
		 '(lambda (e) (send e on-close))))

(test-editor-allocate 'text:basic%)
(test-editor-allocate 'text:keymap%)
(test-editor-allocate 'text:autowrap%)
(test-editor-allocate 'text:file%)
(test-editor-allocate 'text:clever-file-format%)
(test-editor-allocate 'text:backup-autosave%)
(test-editor-allocate 'text:searching%)
(test-editor-allocate 'text:info%)

(test-editor-allocate 'pasteboard:basic%)
(test-editor-allocate 'pasteboard:keymap%)
(test-editor-allocate 'pasteboard:file%)
(test-editor-allocate 'pasteboard:backup-autosave%)
(test-editor-allocate 'pasteboard:info%)

(test-editor-allocate 'scheme:text%)

(test-allocate "text:return%"
	       '(lambda () (make-object text:return% void))
	       '(lambda (t) (void)))


(test-frame-allocate 'frame:basic%)
(test-frame-allocate 'frame:info%)
(test-frame-allocate 'frame:text-info%)
(test-frame-allocate 'frame:pasteboard-info%)
(test-frame-allocate 'frame:standard-menus%)

;(test-frame-allocate 'frame:text%)
;(test-frame-allocate 'frame:text-info-file%)
;(test-frame-allocate 'frame:searchable%)

;(test-frame-allocate 'frame:pasteboard%)
;(test-frame-allocate 'frame:pasteboard-info-file%)
(done)
)
