
(begin-elaboration-time
 (require-relative-library "prettys.ss")
 (require-relative-library "files.ss")
 (require-relative-library "functios.ss")
 (require-relative-library "strings.ss")
 (require-relative-library "compiles.ss")
 (require-relative-library "maths.ss")
 (require-relative-library "threads.ss")

 (require-library "refer.ss"))

(define-signature mzlib:core^
  ((unit pretty-print : mzlib:pretty-print^)
   (unit file : mzlib:file^)
   (unit function : mzlib:function^)
   (unit string : mzlib:string^)
   (unit compile : mzlib:compile^)
   (unit math : mzlib:math^)
   (unit thread : mzlib:thread^)))

