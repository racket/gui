
(begin-elaboration-time
 (require-library "functios.ss"))

(define-signature mzlib:compat^
  (real-time
   1+ 1- #%1+ #%1-
   >=? <=? >? <? =?
   flush-output-port
   bound?
   sort
   gentemp
   atom?
   putprop getprop
   new-cafe))

