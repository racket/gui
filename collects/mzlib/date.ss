
(require-library "dateu.ss")
(require-library "functio.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:date^
  mzlib:date@
  #f
  mzlib:function^)
