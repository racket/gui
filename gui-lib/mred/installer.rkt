#lang racket/base
(require launcher 
         compiler/embed
         setup/dirs
         setup/cross-system
         (submod racket/gui/installer private-install-helpers))

(provide installer)

;; Platforms that get a `MrEd' executable:
(define mred-exe-systems '(unix))

(define (installer path coll user? no-main?)
  (dispatch-to-installer-maker path coll user? no-main? do-installer))

(define (do-installer path coll user? tethered?)
  (define variants (available-mred-variants))
  (when (memq (cross-system-type) mred-exe-systems)
    (for ([v variants] #:when (memq v '(3m cgc cs)))
      (parameterize ([current-launcher-variant v])
        (define exe-name (mred-program-launcher-path "MrEd"
                                                     #:user? user?
                                                     #:tethered? tethered?))
        (unless (exists-in-another-layer? exe-name user? tethered? #:gui? #t)
          (create-embedding-executable
           (prep-dir exe-name)
           #:cmdline (append
                      ;; doing the same thing as a `make-gracket-launcher` #:tether-mode:
                      (if tethered? (if user? (addon-flags) (config-flags)) null)
                      '("-I" "scheme/gui/init"))
           #:variant v
           #:launcher? #t
           #:gracket? #t
           #:aux `((relative? . ,(not user?))))))))
  ;; add a mred-text executable that uses the -z flag (preferring a script)
  (define tether-mode (and tethered? (if user? 'addon 'config)))
  (for ([vs '((script-3m 3m) (script-cgc cgc) (script-cs cs))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (define exe-name (mred-program-launcher-path "mred-text"
                                                       #:user? user?
                                                       #:tethered? tethered?
                                                       #:console? #t))
          (unless (exists-in-another-layer? exe-name user? tethered? #:gui? #f)
            (make-gracket-launcher
             #:tether-mode tether-mode
             '("-I" "scheme/gui/init" "-z")
             (prep-dir exe-name)
             `([relative? . ,(not (or user? tethered?))]
               [subsystem . console]
               [single-instance? . #f])))))))
  ;; add bin/mred script under OS X
  (when (eq? 'macosx (cross-system-type))
    (for ([v variants] #:when (memq v '(script-3m script-cgc script-cs)))
      (parameterize ([current-launcher-variant v])
        (define exe-name (mred-program-launcher-path "MrEd"
                                                     #:user? user?
                                                     #:tethered? tethered?))
        (unless (exists-in-another-layer? exe-name user? tethered? #:gui? #f)
          (make-gracket-launcher
           #:tether-mode tether-mode
           null
           (prep-dir exe-name)
           `([exe-name . "GRacket"]
             [relative? . ,(not (or user? tethered?))]
             [exe-is-gracket . #t])))))))

(define (addon-flags)
  (append
   (config-flags)
   (list "-A" (path->string (find-system-path 'addon-dir)))))

(define (config-flags)
  (list "-G" (path->string (find-config-dir))))
