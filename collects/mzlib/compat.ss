
(require-library "compatu.ss")
(require-library "functio.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:compat^
  mzlib:compat@
  #f
  mzlib:function^)
