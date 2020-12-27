#lang racket/base
(require racket/class
         racket/port
         wxme)

(provide reader)

(define what "collapsed-sexp")

(define collapsed-readable%
  (class* object% (readable<%>)
    (init-field snips/bytes)
    (define/public (read-special source line column position)
      (define-values (in out) (make-pipe-with-specials))
      (set-port-next-location! out line column position)
      (thread
       (λ ()
         (for ([snip/byte (in-list snips/bytes)])
           (cond
             [(bytes? snip/byte) (write-bytes snip/byte out)]
             [else
              (define-values (line column position) (port-next-location out))
              (write-special (snip/byte source line column position) out)]))
         (close-output-port out)))
      ;; this datum->syntax and read is to match the one in
      ;; the framework%'s sexp-snip%'s read-special method
      (datum->syntax
       #f
       (read in)
       (list source line column position 1)))
    (super-new)))

(define reader
  (new (class* object% (snip-reader<%>)
         (define/public (read-header version stream) (void))
         (define/public (read-snip text-only? version stream)
           (define left (send stream read-bytes what))
           (define right (send stream read-bytes what))
           (define count (send stream read-integer what))
           (define all-bytes? #t)
           (define snips/bytes
             (for/list ([_ (in-range 0 count)])
               (define snip-class-name (bytes->string/utf-8 (send stream read-bytes what)))
               (define got
                 (read-snip-from-port snip-class-name
                                      'collapsed-snipclass-wxme.rkt
                                      stream))
               (cond
                 [(bytes? got) got]
                 [text-only?
                  ;; here we just make an attempt to turn the special into
                  ;; something someone might recognize
                  (string->bytes/utf-8 (format "~s" got))]
                 [else
                  (set! all-bytes? #f)
                  got])))
           (cond
             [all-bytes? (apply bytes-append snips/bytes)]
             [else (new collapsed-readable% [snips/bytes snips/bytes])]))
         (super-new))))
