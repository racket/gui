#lang racket/base
(require racket/gui/base racket/class)
(provide frame:lookup-focus-table
         frame:set-focus-table
         find-parent/editor
         find-parent/window)

;; focus-table : hash[eventspace -o> (listof frame)]
(define focus-table (make-hash))
(define (frame:lookup-focus-table [eventspace (current-eventspace)])
  (hash-ref focus-table eventspace '()))
(define (frame:set-focus-table eventspace new)
  (if (null? new)
      (hash-remove! focus-table eventspace)
      (hash-set! focus-table eventspace new)))

(define (find-parent/editor editor)
  (let loop ([editor editor])
    (define ed-admin (send editor get-admin))
    (cond
      [(not ed-admin) #f]
      [(is-a? ed-admin editor-snip-editor-admin<%>)
       (define snip (send ed-admin get-snip))
       (define snip-admin (send snip get-admin))
       (loop (send snip-admin get-editor))]
      [else
       (define canvas (send editor get-canvas))
       (and canvas (find-parent/window canvas))])))

(define (find-parent/window win)
  (let loop ([win win])
    (cond
      [(or (is-a? win frame%)
           (is-a? win dialog%))
       win]
      [else
       (define p (send win get-parent))
       (and p (loop p))])))
