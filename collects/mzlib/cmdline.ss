
(require-library "cmdlineu.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:command-line^ mzlib:command-line@ #f)

(require-library "cmdlinem.ss")
