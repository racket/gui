(module collapsed-snipclass-helpers mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (provide make-sexp-snipclass%)
  
  (define (make-sexp-snipclass% sexp-snip%)
    (class snip-class%
      (define/override (read in)
        (let* ([left-bracket (string-ref (send in get-string) 0)]
               [right-bracket (string-ref (send in get-string) 0)]
               [snip-count (send in get-exact)]
               [saved-snips
                (let loop ([n snip-count])
                  (cond
                    [(zero? n) null]
                    [else
                     (let* ([classname (send in get-string)]
                            [snipclass (send (get-the-snip-class-list) find classname)])
                       (cons (send snipclass read in)
                             (loop (- n 1))))]))])
          (instantiate sexp-snip% ()
            (left-bracket left-bracket)
            (right-bracket right-bracket)
            (saved-snips saved-snips))))
      (super-instantiate ()))))
  