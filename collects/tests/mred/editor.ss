
(load-relative "loadtest.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Editor Tests                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Undo tests

(define e (make-object text%))

(stv e insert "Hello")
(st #t e is-modified?)
(stv e undo)
(st #f e is-modified?)
(stv e redo)
(st #t e is-modified?)
(stv e set-modified #f)
(st #f e is-modified?)
(stv e undo)
(st #t e is-modified?)
(stv e redo)
(st #f e is-modified?)
(stv e undo)
(st #t e is-modified?)
(stv e redo)
(st #f e is-modified?)
(stv e undo)
(stv e set-modified #f)
(st #f e is-modified?)
(stv e redo)
(st #t e is-modified?)
(st "Hello" e get-text)
(define undone? #f)
(stv e add-undo (letrec ([f (lambda ()
			      (set! undone? #t)
			      (send e add-undo f) ; reinstall self!
			      #f)])
		  f))
(stv e undo)
(st "Hello" e get-text)
(test #t 'undone? undone?)
(stv e undo)
(st "" e get-text)
(set! undone? #f)
(stv e redo)
(st "Hello" e get-text)
(test #f 'undone? undone?)
(stv e redo)
(st "Hello" e get-text)
(test #t 'undone? undone?)
(set! undone? #f)
(stv e redo)
(st "Hello" e get-text)
(test #f 'undone? undone?)
(stv e insert "x")
(st "Hellox" e get-text)
(stv e add-undo (lambda ()
		  (set! undone? #t)
		  #t)) ; do next one, too
(stv e undo)
(test #t 'undone? undone?)
(st "Hello" e get-text)

;; Editor ports

(let ([e (make-object text%)]
      [res-mode? #f])
  (stv e insert "hello")
  (let ([p (open-input-text-editor e)])
    (test 'hello 'read (read p))
    (test eof 'read (read p)))
  (stv e insert " there")
  (let ([p (open-input-text-editor e)])
    (test 'hello 'read (read p))
    (test 'there 'read (read p))
    (test eof 'read (read p)))
  (stv e insert (make-object
		 (class* snip% 
		   (readable-snip<%>)
		   (define/public (read-special src line col pos)
		     (if res-mode?
			 'res
			 (error 'ack)))
		   (super-new))))
  (let ([p (open-input-text-editor e)])
    (port-count-lines! p)
    (test '(1 0 1) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test 'hello 'read (read p))
    (test '(1 5 6) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test 'there 'read (read p))
    (test '(1 11 12) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test 'got-ack 'read (with-handlers ([exn:fail? (lambda (x)
						      'got-ack)])
			   (read p)))
    (test '(1 12 13) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test eof 'read (read p)))
  (set! res-mode? #t)
  (let ([p (open-input-text-editor e)])
    (port-count-lines! p)
    (test 'hello 'read (read p))
    (test 'there 'read (read p))
    (test 'res 'read (read p))
    (test '(1 12 13) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test eof 'read (read p)))
  (stv e insert (make-object image-snip% (build-path
					  (collection-path "icons") 
					  "plt.gif")))
  (let ([p (open-input-text-editor e)])
    (test 'hello 'read (read p))
    (test 'there 'read (read p))
    (test 'res 'read (read p))
    (test #t 'read (is-a? (read p) image-snip%))))

(let ()
  (define x (new text%))
  (define s (make-object image-snip% "no-such-image.jpg" 
			 'unknown #f #f))
  (define t (make-object image-snip% "no-such-image.jpg" 
			 'unknown #f #f))
  (send x insert s 0 'same #t)
  (send x insert t 1 'same #t)
  (send x insert "1" 2 'same #t)
  
  (let ([i (open-input-text-editor x 0 'end (lambda (x) (eq? x s)))])
    (test #t 'peek-s (peek-byte-or-special i 0))
    (test #t 'read-s (read-byte-or-special i))
    (test #f 'peek-t (peek-byte-or-special i 0))
    (test 49 'read-1 (peek-byte-or-special i 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Snips and Streams                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define number-snip-class%
  (class snip-class%
    (define/override (read f)
      (let* ([number-str (send f get-bytes)]
             [number (string->number (bytes->string/utf-8 number-str))]
             [decimal-prefix (bytes->string/utf-8 (send f get-bytes))]
             [snip
              (instantiate number-snip% ()
                [number number]
                [decimal-prefix decimal-prefix])])
        snip))
    (super-instantiate ())))

(define snip-class (make-object number-snip-class%))
(send snip-class set-classname (format "~s" `(lib "number-snip.ss" "drscheme" "private")))
(send (get-the-snip-class-list) add snip-class)

(define number-snip%
  (class snip%
    (init-field number)
    (define/public (get-number) number)
    (define/public (get-prefix) decimal-prefix)
    (init-field [decimal-prefix ""])
    (define/override (write f)
      (send f put (string->bytes/utf-8 (number->string number)))
      (send f put (string->bytes/utf-8 decimal-prefix)))
    (define/override (copy)
      (instantiate number-snip% ()
        [number number]
        [decimal-prefix decimal-prefix]))
    (inherit get-style)
    (super-instantiate ())
    (inherit set-snipclass set-flags get-flags)
    (set-snipclass snip-class)))

(define t (new text%))
(define t2 (new text%))
(send t insert (instantiate number-snip% () [number 1/2]))
(send t set-position 0 1)
(send t copy)
;; Under X, force snip to be marshalled:
(let ([s (send the-clipboard get-clipboard-data "WXME" 0)])
  (send the-clipboard set-clipboard-client
	(make-object (class clipboard-client%
		       (define/override (get-data fmt)
			 (and (string=? fmt "WXME")
			      s))
		       (inherit add-type)
		       (super-new)
		       (add-type "WXME")))
	0))
(send t2 paste)
(let ([s (send t2 find-first-snip)])
  (st 1/2 s get-number)
  (st "" s get-prefix))

(let ()
  (define orig-snip (make-object string-snip% "hello"))
  
  (define out (make-object editor-stream-out-bytes-base%))
  (define out-stream (make-object editor-stream-out% out))
  
  (define _ (send orig-snip write out-stream))
  
  (define in (make-object editor-stream-in-bytes-base% (send out get-bytes)))
  (define in-stream (make-object editor-stream-in% in))
  
  (define new-snip
    (send (send (get-the-snip-class-list) 
		find 
		(send (send (new string-snip%) get-snipclass) get-classname))
	  read in-stream))
  
  (st "hello" new-snip get-text 0 10))

;; ----------------------------------------
;; Check CRLF conversion

(let ([crlf-s (apply
	       string-append
	       (let loop ([n 100])
		 (if (zero? n)
		     null
		     (cons (make-string (random 40) #\a)
			   (cons "\r\n"
				 (loop (sub1 n)))))))]
      [e (new text%)])
  (let ([lf-s (regexp-replace* #rx"\r\n" crlf-s "\n")]
	[lflf-s (regexp-replace* #rx"\r\n" crlf-s "\n\n")])
    (st "" e get-text 0 'eof)
    (send e insert-port (open-input-string crlf-s))
    (st lf-s e get-text 0 'eof)
    (send e erase)
    (send e insert crlf-s)
    (st lflf-s e get-text 0 'eof)))

;; ----------------------------------------

(report-errs)

