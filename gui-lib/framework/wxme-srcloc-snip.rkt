#lang racket/base
(require racket/class
         racket/format
         wxme
         wxme/private/readable-editor)

(provide reader)

(define (datum->source source)
  (if (bytes? source)
      (bytes->path source)
      source))

(define (datum->source-expression source)
  (if (path? source)
      `(bytes->path ,(path->bytes source))
      `',source))

(define (srcloc->expression srcloc)
  `(srcloc
    ,(datum->source-expression (srcloc-source srcloc))
    ,(srcloc-line srcloc)
    ,(srcloc-column srcloc)
    ,(srcloc-position srcloc)
    ,(srcloc-span srcloc)))

(define editor-reader (new editor-reader%))

(define srcloc-snip-reader%
  (class* object% (snip-reader<%>)
    (define/public (read-header version stream) (void))
    (define/public (read-snip text-only? version stream)
      (let* ((bytes (send stream read-raw-bytes "srcloc"))
             (port (open-input-bytes bytes 'srcloc))
             (datum (read port))
             (srcloc (apply
                      (lambda (_ source line column position span)
                        (srcloc (datum->source source) line column position span))
                      datum))
             (editor (send editor-reader read-snip text-only? version stream))) ; don't need this
      (cond
        [text-only?
         (string->bytes/utf-8 (~s (srcloc->expression srcloc)))]
        [else
         (new srcloc-readable [srcloc srcloc])])))
    (super-new)))

(define srcloc-readable
  (class* object% (readable<%>)
    (init-field srcloc)
    (define/public (read-special source line column position)
      (datum->syntax #f
                     (srcloc->expression srcloc)
                     (vector source line column position 1)
                     #f))
    (super-new)))

(define reader (new srcloc-snip-reader%))
