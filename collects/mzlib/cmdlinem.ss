
(begin-elaboration-time
 (require-library "invoke.ss"))

(begin-elaboration-time
 (define-values/invoke-unit (command-line)
   (require-library "cmdlinemr.ss")))

(define-macro command-line command-line)
