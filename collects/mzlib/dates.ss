
(begin-elaboration-time
 (require-library "functios.ss"))

(define-signature mzlib:date^ 
  (date->string
   date-display-format
   find-seconds

   date->julian/scalinger
   julian/scalinger->string))
