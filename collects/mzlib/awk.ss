
(begin-elaboration-time 
 (require-library "invoke.ss"))

(begin-elaboration-time 
 (define-values/invoke-unit (awk) 
   (require-relative-library "awkr.ss")))

(define-macro awk awk)

(define-values/invoke-unit (match:start match:end match:substring regexp-exec)
  (require-relative-library "awkr.ss"))

