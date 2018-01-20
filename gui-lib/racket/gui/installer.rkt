#lang racket/base
(require launcher
         racket/path
         racket/file
         setup/dirs)

(provide installer)

(define (installer path coll user? no-main?)
  (unless no-main?
    (do-installer path coll user? #f)
    (when (and (not user?)
               (find-config-tethered-console-bin-dir))
      (do-installer path coll #f #t)))
  (when (find-addon-tethered-console-bin-dir)
    (do-installer path coll #t #t)))

(define (do-installer path collection user? tethered?)
  (define variants (available-mred-variants))
  (define tether-mode (and tethered? (if user? 'addon 'config)))
  ;; add a gracket-text executable that uses the -z flag (preferring a script)
  (for ([vs '((script-3m 3m) (script-cgc cgc) (script-cs cs))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (make-mred-launcher
           #:tether-mode tether-mode
           '("-z")
           (prep-dir
            (mred-program-launcher-path "gracket-text"
                                        #:user? user?
                                        #:tethered? tethered?
                                        #:console? #t))
           `([subsystem . console]
             [single-instance? . #f]
             [relative? . ,(not (or user? tethered?))]))))))
  ;; add a bin/gracket (in addition to lib/gracket)
  (for ([vs '((script-3m 3m) (script-cgc cgc) (script-cs cs))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (make-mred-launcher #:tether-mode tether-mode
                              null
                              (prep-dir
                               (mred-program-launcher-path "GRacket" #:user? user? #:tethered? tethered?))
                              `([exe-name . "GRacket"]
                                [relative? . ,(not (or user? tethered?))]
                                [exe-is-gracket . #t])))))))

(define (prep-dir p)
  (define dir (path-only p))
  (make-directory* dir)
  p)
