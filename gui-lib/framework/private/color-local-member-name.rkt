#lang racket/base
(require racket/class)
(define-local-member-name tokenizing-give-up-early)
(provide tokenizing-give-up-early
         check-colorer-results-match-port-before-and-afters)

(define (check-colorer-results-match-port-before-and-afters
         type pos-before new-token-start new-token-end pos-after)
  (unless (equal? 'eof type)
    (unless (<= pos-before new-token-start pos-after)
      (error 'color:text<%>
             "expected the token start to be between ~s and ~s, got ~s"
             pos-before pos-after new-token-start))
    (unless (<= pos-before new-token-end pos-after)
      (error 'color:text<%>
             "expected the token end to be between ~s and ~s, got ~s"
             pos-before pos-after new-token-end))))
