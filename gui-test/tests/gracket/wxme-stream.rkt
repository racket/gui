#lang racket/base
(require racket/class
         racket/contract
         racket/match
         racket/math
         mred/private/wxme/stream
         (for-syntax racket/base))

(define wrong-cnt 0)
(define test-cnt 0)

(define-syntax (expect stx)
  (syntax-case stx ()
    [(_ a b #:extra-stuff e)
     #`(expect/proc #,(syntax-line stx) a b e)]
    [(_ a b)
     #`(expect/proc #,(syntax-line stx) a b #f)]))

(define (expect/proc line v v2 extra-stuff)
  (set! test-cnt (add1 test-cnt))
  (unless (equal? v v2)
    (set! wrong-cnt (add1 wrong-cnt))
    (eprintf "FAILED: line ~a\nexpected: ~s\n     got: ~s\n"
             line
             v2
             v)
    (when extra-stuff
      (eprintf "   extra: ~s\n" extra-stuff))))

(define (done)
  (printf "\n~a tests\n" test-cnt)
  (flush-output)
  (if (zero? wrong-cnt)
      (printf "all passed\n")
      (begin
        (eprintf "~s FAILED\n" wrong-cnt)
        (exit 1))))

;; ----------------------------------------
;; Stream out base

(define fbo (make-object editor-stream-out-bytes-base%))
(expect (send fbo tell) 0)
(expect (send fbo write-bytes #"abc") (void))
(expect (send fbo tell) 3)
(expect (send fbo get-bytes) #"abc")
(send fbo seek 2)
(expect (send fbo write-bytes #"012345" 1 4) (void))
(expect (send fbo tell) 5)
(expect (send fbo get-bytes) #"ab123")
(expect (send fbo bad?) #f)
(expect (send fbo write '(#\o #\l #\d)) (void))
(expect (send fbo get-bytes) #"ab123old")

;; ----------------------------------------
;; Stream in base

(define fbi (make-object editor-stream-in-bytes-base% #"ab123old"))
(define ibuf (make-bytes 3))
(expect (send fbi tell) 0)
(expect (send fbi read-bytes ibuf) 3)
(expect ibuf #"ab1")
(expect (send fbi tell) 3)
(send fbi seek 2)
(expect (send fbi read-bytes ibuf 1 2) 1)
(expect ibuf #"a11")
(send fbi skip 2)
(expect (send fbi read-bytes ibuf 0 2) 2)
(expect ibuf #"ol1")
(expect (send fbi bad?) #f)

;; ----------------------------------------
;; Stream writing

(define fbo2 (make-object editor-stream-out-bytes-base%))
(define fo (make-object editor-stream-out% fbo2))

(expect (send fo tell) 0)
(void (send fo put 2))
(expect (send fbo2 get-bytes) #"\n2")
(void (send fo put 2.0))
(expect (send fbo2 get-bytes) #"\n2 2.0")
(expect (send fo tell) 2)
(send fo jump-to 0)
(void (send fo put 3))
(send fo jump-to 2)
(expect (send fbo2 get-bytes) #"\n3 2.0")
(void (send fo put #"hi"))
(expect (send fbo2 get-bytes) #"\n3 2.0 3 #\"hi\\0\"")
(void (send fo put 3 #"bye?"))
(expect (send fbo2 get-bytes) #"\n3 2.0 3 #\"hi\\0\"\n3 #\"bye\"")
(void (send fo put 80
            #"0123456789abcdefghij0123456789ABCDEFGHIJ0123456789abcdefghij0123456\"89ABCDEFGHIJ"))
(expect (send fbo2 get-bytes) 
        (bytes-append
         #"\n3 2.0 3 #\"hi\\0\"\n3 #\"bye\"\n80\n"
         #"(0 80\n"
         #"0123456789abcdefghij0123456789ABCDEFGHIJ0123456789abcdefghij0123456\"89ABCDEFGHIJ"
         #"\n"
         #")"))

(define fbo3 (make-object editor-stream-out-bytes-base%))
(define fo3 (make-object editor-stream-out% fbo3))
(void (send fo3 put 2))
(expect (send fo3 tell) 1)
(void (send fo3 put-fixed 5))
(expect (send fo3 tell) 2)
(void (send fo3 put-fixed -8))
(void (send fo3 put 2 #"hi"))
(expect (send fbo3 get-bytes) #"\n2           5          -8 2 #\"hi\"")
(send fo3 jump-to 1)
(void (send fo3 put-fixed -4))
(send fo3 jump-to 2)
(void (send fo3 put-fixed 7))
(expect (send fbo3 get-bytes) #"\n2          -4           7 2 #\"hi\"")

;; ----------------------------------------
;; Stream reading

(let ()
  (define fbi2 (make-object editor-stream-in-bytes-base%
                 (bytes-append #"1 ; comment \n 2 "
                               #"#| | x # #|  |# q |# 4.0"
                               #" 2 #\"hi\""
                               #" 3 #\"hi\\\"\""
                               ;; this is in the old format, without an id at
                               ;; the start of the bytes
                               #" 23 (#\"0123456789ABCDEFappl\" #\"e!\\0\" ) 88")))
  (define fi2 (make-object editor-stream-in% fbi2))

  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 0)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 1)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 1)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 2)
  (expect (send fi2 ok?) #t)
  (expect (let ([b (box 0.0)]) (send fi2 get b) (unbox b)) 4.0)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 3)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 5)
  (expect (send fi2 get-unterminated-bytes) #"hi\"")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 get-bytes) #"0123456789ABCDEFapple!")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 9)

  (send fi2 jump-to 3)
  (expect (send fi2 tell) 3)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (send fi2 skip 4)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 88)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 10)

  (send fi2 jump-to 3)
  (send fi2 set-boundary 2)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (send fi2 jump-to 3)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 3)
  (send fi2 set-boundary 1)
  (expect (with-handlers ([values (lambda (exn) #"")])
            (send fi2 get-unterminated-bytes))
          #"")
  (expect (send fi2 ok?) #f))

(let ()
  (define fbi2 (make-object editor-stream-in-bytes-base%
                 (bytes-append #"1 ; comment \n 2 "
                               #"#| | x # #|  |# q |# 4.0"
                               #" 2 #\"hi\""
                               #" 3 #\"hi\\\"\""
                               ;; this is in the new format, with an id at
                               ;; the start of the bytes
                               #" 23 (0 #\"0123456789ABCDEFappl\" #\"e!\\0\" ) 88")))
  (define fi2 (make-object editor-stream-in% fbi2))

  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 0)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 1)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 1)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 2)
  (expect (send fi2 ok?) #t)
  (expect (let ([b (box 0.0)]) (send fi2 get b) (unbox b)) 4.0)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 3)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 5)
  (expect (send fi2 get-unterminated-bytes) #"hi\"")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 get-bytes) #"0123456789ABCDEFapple!")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 9)

  (send fi2 jump-to 3)
  (expect (send fi2 tell) 3)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (send fi2 skip 4)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 88)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 10)

  (send fi2 jump-to 3)
  (send fi2 set-boundary 2)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (send fi2 jump-to 3)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 3)
  (send fi2 set-boundary 1)
  (expect (with-handlers ([values (lambda (exn) #"")])
            (send fi2 get-unterminated-bytes))
          #"")
  (expect (send fi2 ok?) #f))

;; this is a duplicate of the previous test, but using a newer format
;; for the underlying data
(let ()
  (define fbi2 (make-object editor-stream-in-bytes-base%
                 (bytes-append #"1 ; comment \n 2 "
                               #"#| | x # #|  |# q |# 4.0"
                               #" 2 #\"hi\""
                               #" 3 #\"hi\\\"\""
                               #" 23 (0 23\n0123456789ABCDEFapple!\0\n) 88")))
  (define fi2 (make-object editor-stream-in% fbi2))

  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 0)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 1)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 1)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 2)
  (expect (send fi2 ok?) #t)
  (expect (let ([b (box 0.0)]) (send fi2 get b) (unbox b)) 4.0)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 3)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 5)
  (expect (send fi2 get-unterminated-bytes) #"hi\"")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 get-bytes) #"0123456789ABCDEFapple!")
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 9)

  (send fi2 jump-to 3)
  (expect (send fi2 tell) 3)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (send fi2 skip 4)
  (expect (let ([b (box 0)]) (send fi2 get b) (unbox b)) 88)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 10)

  (send fi2 jump-to 3)
  (send fi2 set-boundary 2)
  (expect (send fi2 get-unterminated-bytes) #"hi")
  (send fi2 jump-to 3)
  (expect (send fi2 ok?) #t)
  (expect (send fi2 tell) 3)
  (send fi2 set-boundary 1)
  (expect (with-handlers ([values (lambda (exn) #"")])
            (send fi2 get-unterminated-bytes))
          #"")
  (expect (send fi2 ok?) #f))


(let ()
  ;; this test ensures that no matter which way a bytes is
  ;; written the amount by which tell changes remains the same
  (define the-allowed-delta #f)

  (define fbo (make-object editor-stream-out-bytes-base%))
  (define fo (make-object editor-stream-out% fbo))
  (let ([before (send fo tell)])
    (send fo put 100 (make-bytes 100 (char->integer #\a)))
    (set! the-allowed-delta (- (send fo tell) before)))

  (let ([before (send fo tell)])
    (send fo put 100 (make-bytes 100 (char->integer #\a)))
    (expect (send fo tell) (+ before the-allowed-delta)))

  (let ([before (send fo tell)])
    (send fo put 4 (make-bytes 4 (char->integer #\a)))
    (expect (send fo tell) (+ before the-allowed-delta)))

  (let ([before (send fo tell)])
    (send fo put 4 (make-bytes 4 (char->integer #\a)))
    (expect (send fo tell) (+ before the-allowed-delta)))

  (define fbi (make-object editor-stream-in-bytes-base% (send fbo get-bytes)))
  (define fi (make-object editor-stream-in% fbi))

  (define one (send fi tell))

  (send fi get-unterminated-bytes)
  (define two (send fi tell))
  (expect two (+ one the-allowed-delta))

  (send fi get-unterminated-bytes)
  (define three (send fi tell))
  (expect three (+ two the-allowed-delta))

  (send fi get-unterminated-bytes)
  (define four (send fi tell))
  (expect four (+ three the-allowed-delta)))

(let ()
  ;; test that both inexacts and exacts written to the
  ;; stream come back from the `get-exact` method
  (define fbo2 (make-object editor-stream-out-bytes-base%))
  (define fo (make-object editor-stream-out% fbo2))

  (void (send fo put 2))
  (send fbo2 get-bytes)
  (void (send fo put 2.0))
  (send fbo2 get-bytes)

  (define fbi2 (make-object editor-stream-in-bytes-base%
                 (send fbo2 get-bytes)))
  (define fi2 (make-object editor-stream-in% fbi2))

  (define n1 (send fi2 get-exact))
  (define n2 (send fi2 get-exact))
  (expect (exact? n1) #t)
  (expect (= n1 2) #t)
  (expect (exact? n2) #f)
  (expect (= n1 2) #t))

(let ()
  (define (wash-it b)
    (define out-base (new editor-stream-out-bytes-base%))
    (define out-stream (make-object editor-stream-out% out-base))
    (send out-stream put (bytes-length b) b)
    (define bstr (send out-base get-bytes))
    (define in-base (make-object editor-stream-in-bytes-base% bstr))
    (define in-stream (make-object editor-stream-in% in-base))
    (send in-stream get-unterminated-bytes))
  (define ex-b #"object ...  ;;\351\232\234\347\242\215\347\211\251\345\210\227\350\241\250")
  (expect (wash-it ex-b) ex-b))

;; a what is either:
;;   (vector n bytes)
;;      -- writes with `put`'s two arugment (number and bytes) case
;;      -- reads with `get-unterminated-bytes`
;;   (and/c real? (not/c exact?))
;;      -- writes with `put`
;;      -- reads with `get-inexact`
;;   exact-integer?
;;      -- writes with `put`
;;      -- reads with `get-exact`
;;   (vector 'fixed n)  the number must be in bounds
;;      -- writes with `put-fixed`
;;      -- reads with `get-fixed`
;;   bytes?
;;      -- writes with `put` with one argument (after adding a nul to the given bytes)
;;      -- reads with `get-bytes`

;; in/out : (listof what) -> void
;; sends the `what`s into and then back out from a stream,
;; checking to make sure they come out the same as they went in

(define what/c
  (or/c (and/c (vector-immutable/c natural? bytes?)
               (λ (v) (<= (vector-ref v 0)
                          (bytes-length (vector-ref v 1)))))
        (and/c real? (not/c exact?))
        (and/c exact-integer? (<=/c (expt 10 49)))
        (vector-immutable/c 'fixed (integer-in -9999999999 99999999999))
        bytes?))

(define/contract (in/out whats)
  (-> (listof what/c) void?)
  (set! test-cnt (+ test-cnt 1))
  (define bb  (make-object editor-stream-out-bytes-base%))
  (define out (make-object editor-stream-out% bb))
  (for ([what (in-list whats)])
    (match what
      [(vector n (? bytes? b)) (send out put n b)]
      [(? (and/c real? (not/c exact?)) v) (send out put v)]
      [(? exact-integer? v) (send out put v)]
      [(vector 'fixed n) (send out put-fixed n)]
      [(? bytes? v) (send out put-unterminated (bytes-append v (bytes 0)))]))
  (define in (make-object editor-stream-in%
               (make-object editor-stream-in-bytes-base%
                 (send bb get-bytes))))
  (let/ec escape
    (for ([what (in-list whats)]
          [i (in-naturals)])
      (define (check-em got expected)
        (unless (equal? got expected)
          (set! wrong-cnt (+ wrong-cnt 1))
          (eprintf "failure!\n     index: ~a\n       got: ~s\n  expected: ~s\n     whats: ~s\n"
                   i
                   got
                   expected
                   whats)
          (escape (void))))
      (match what
        [(vector n (? bytes? b))
         (define bx (box #f))
         (define got-bytes (send in get-unterminated-bytes bx))
         (check-em got-bytes (subbytes b 0 n))
         (check-em (unbox bx) n)]
        [(? (and/c real? (not/c exact?)) v)
         (check-em (send in get-inexact) v)]
        [(? exact-integer? v)
         (check-em (send in get-exact) v)]
        [(vector 'fixed n)
         (check-em (send in get-fixed-exact) n)]
        [(? bytes? v)
         (define bx (box #f))
         (check-em (send in get-bytes bx) v)
         (check-em (- (unbox bx) 1) (bytes-length v))]))))


(in/out (list (vector-immutable 0 #"abc")))
(in/out (list (vector-immutable 1 #"abc")))
(in/out (list (vector-immutable 2 #"abc")))
(in/out (list (vector-immutable 999 (make-bytes 1000 (char->integer #\a)))))
(in/out (list (vector-immutable 1000 (make-bytes 1000 (char->integer  #\a)))))
(in/out (list 1.0))
(in/out (list 1.5))
(in/out (list -1))
(in/out (list 0))
(in/out (list 1))
(in/out (list (expt 10 20)))
(in/out (list (vector-immutable 'fixed -1)))
(in/out (list (vector-immutable 'fixed 0)))
(in/out (list (vector-immutable 'fixed 1)))
(in/out (list #"abc"))
(in/out (list (make-bytes 1000 (char->integer  #\a))))
(in/out (list (make-bytes 1000 (char->integer  #\a))
              (make-bytes 1000 (char->integer  #\a))))
(in/out (list (make-bytes 100 (char->integer  #\a))
              (make-bytes 200 (char->integer  #\b))
              (make-bytes 300 (char->integer  #\c))
              (make-bytes 100 (char->integer  #\c))
              (make-bytes 200 (char->integer  #\b))
              (make-bytes 300 (char->integer  #\a))
              (make-bytes 200 (char->integer  #\b))
              (make-bytes 300 (char->integer  #\a))))
(in/out (list (make-bytes 100 (char->integer  #\a))
              (make-bytes 101 (char->integer  #\b))
              (make-bytes 102 (char->integer  #\c))
              (make-bytes 103 (char->integer  #\d))
              (make-bytes 104 (char->integer  #\e))
              (make-bytes 105 (char->integer  #\f))
              (make-bytes 106 (char->integer  #\g))
              (make-bytes 107 (char->integer  #\h))
              (make-bytes 108 (char->integer  #\i))
              (make-bytes 109 (char->integer  #\j))
              (make-bytes 110 (char->integer  #\k))
              (make-bytes 111 (char->integer  #\l))
              (make-bytes 100 (char->integer  #\a))
              (make-bytes 101 (char->integer  #\b))
              (make-bytes 102 (char->integer  #\c))
              (make-bytes 103 (char->integer  #\d))
              (make-bytes 104 (char->integer  #\e))
              (make-bytes 105 (char->integer  #\f))
              (make-bytes 106 (char->integer  #\g))
              (make-bytes 107 (char->integer  #\h))
              (make-bytes 108 (char->integer  #\i))
              (make-bytes 109 (char->integer  #\j))
              (make-bytes 110 (char->integer  #\k))
              (make-bytes 111 (char->integer  #\l))))
(in/out (list (make-bytes 100 (char->integer  #\a))
              (make-bytes 101 (char->integer  #\b))
              (make-bytes 102 (char->integer  #\c))
              (make-bytes 103 (char->integer  #\d))
              (make-bytes 104 (char->integer  #\e))
              (make-bytes 105 (char->integer  #\f))
              (make-bytes 106 (char->integer  #\g))
              (make-bytes 107 (char->integer  #\h))
              (make-bytes 108 (char->integer  #\i))
              (make-bytes 109 (char->integer  #\j))
              (make-bytes 110 (char->integer  #\k))
              (make-bytes 111 (char->integer  #\l))
              (make-bytes 111 (char->integer  #\l))
              (make-bytes 110 (char->integer  #\k))
              (make-bytes 109 (char->integer  #\j))
              (make-bytes 108 (char->integer  #\i))
              (make-bytes 107 (char->integer  #\h))
              (make-bytes 106 (char->integer  #\g))
              (make-bytes 105 (char->integer  #\f))
              (make-bytes 104 (char->integer  #\e))
              (make-bytes 103 (char->integer  #\d))
              (make-bytes 102 (char->integer  #\c))
              (make-bytes 101 (char->integer  #\b))
              (make-bytes 100 (char->integer  #\a))))

(for ([x (in-range 1000)])
  (in/out (contract-random-generate
           (listof what/c))))

(done)
