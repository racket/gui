(test
 'basic-mixin-creation
 (lambda (x) x)
 (lambda ()
   (send-sexp-to-mred
    '(send (make-object (frame:basic-mixin frame%) "test") show #t))
   (wait-for-frame "test")
   (send-sexp-to-mred
    '(send (get-top-level-focus-window) show #f))
   #t))

(test
 'basic-mixin-creation
 (lambda (x) x)
 (lambda ()
   (send-sexp-to-mred
    '(send (make-object (frame:basic-mixin frame%) "test") show #t))
   (wait-for-frame "test")
   (send-sexp-to-mred
    '(send (get-top-level-focus-window) show #f))
   #t))
