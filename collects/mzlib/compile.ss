
(require-library "compileu.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:compile^
  mzlib:compile@)
