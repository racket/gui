#lang racket/base
(require launcher 
         compiler/embed
         racket/file
         racket/path
         setup/dirs
         setup/cross-system)

(provide installer)

;; Platforms that get a `MrEd' executable:
(define mred-exe-systems '(unix))

(define (installer path coll user? no-main?)
  (unless no-main?
    (do-installer path coll user? #f)
    (when (and (not user?)
               (find-config-tethered-console-bin-dir))
      (do-installer path coll #f #t)))
  (when (find-addon-tethered-console-bin-dir)
    (do-installer path coll #t #t)))

(define (do-installer path coll user? tethered?)
  (define variants (available-mred-variants))
  (when (memq (cross-system-type) mred-exe-systems)
    (for ([v variants] #:when (memq v '(3m cgc cs)))
      (parameterize ([current-launcher-variant v])
        (create-embedding-executable
         (prep-dir (mred-program-launcher-path "MrEd"
                                               #:user? user?
                                               #:tethered? tethered?))
         #:cmdline (append
                    (if tethered? (if user? (addon-flags) (config-flags)) null)
                    '("-I" "scheme/gui/init"))
         #:variant v
         #:launcher? #t
         #:gracket? #t
         #:aux `((relative? . ,(not user?)))))))
  ;; add a mred-text executable that uses the -z flag (preferring a script)
  (define tether-mode (and tethered? (if user? 'addon 'config)))
  (for ([vs '((script-3m 3m) (script-cgc cgc) (script-cs cs))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (make-gracket-launcher
           #:tether-mode tether-mode
           '("-I" "scheme/gui/init" "-z")
           (prep-dir (mred-program-launcher-path "mred-text"
                                                 #:user? user?
                                                 #:tethered? tethered?
                                                 #:console? #t))
           `([relative? . ,(not (or user? tethered?))]
             [subsystem . console]
             [single-instance? . #f]))))))
  ;; add bin/mred script under OS X
  (when (eq? 'macosx (cross-system-type))
    (for ([v variants] #:when (memq v '(script-3m script-cgc script-cs)))
      (parameterize ([current-launcher-variant v])
        (make-gracket-launcher
         #:tether-mode tether-mode
         null
         (prep-dir (mred-program-launcher-path "MrEd"
                                               #:user? user?
                                               #:tethered? tethered?))
         `([exe-name . "GRacket"]
           [relative? . ,(not (or user? tethered?))]
           [exe-is-gracket . #t]))))))

(define (prep-dir p)
  (define dir (path-only p))
  (make-directory* dir)
  p)

(define (addon-flags)
  (append
   (config-flags)
   (list "-A" (path->string (find-system-path 'addon-dir)))))

(define (config-flags)
  (list "-C" (path->string (find-config-dir))))
