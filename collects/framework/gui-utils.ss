(module gui-utils mzscheme
  (require (lib "unitsig.ss")
	   "gui-utils-sig.ss"
	   "gui-utils-unit.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred"))

  (provide-signature-elements ((unit gui-utils : framework:gui-utils^)))

  (define-values/invoke-unit/sig 
    ((unit gui-utils : framework:gui-utils^))
    (compound-unit/sig
      (import [mred : mred^])
      (link [gui-utils : framework:gui-utils^ (framework:gui-utils@ mred)])
      (export (unit gui-utils)))
    #f
    mred^))

#|
(require gui-utils)
(define f (make-object frame% "frame" #f 300 600))
(define lb (instantiate gui-utils:alphabetic-list-box% ()
             (label #f)
             (parent f)
             (callback void)
             (choices '("abcz" "b" "c" "d" "e" "f" "xbcdefghi"))))
(send f show #t)
|#