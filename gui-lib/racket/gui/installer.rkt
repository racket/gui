#lang racket/base
(require launcher)

(provide installer)

(module private-install-helpers racket/base
  (require setup/dirs
           racket/file
           racket/path)

  (provide dispatch-to-installer-maker
           exists-in-another-layer?
           prep-dir)

  (define (dispatch-to-installer-maker path coll user? no-main? do-installer)
    (cond
      [user?
       (if (find-addon-tethered-console-bin-dir)
           (do-installer path coll #t #t)
           (do-installer path coll #t #f))]
      [else
       (unless no-main?
         (if (find-config-tethered-console-bin-dir)
             (do-installer path coll #f #t)
             (do-installer path coll #f #f)))]))

  (define (exists-in-another-layer? exe-name user? tethered? #:gui? gui?)
    ;; for an untethered main installation, check whether the
    ;; executable exists already in an earlier layer
    (and (not user?)
         (not tethered?)
         (let-values ([(base name dir?) (split-path exe-name)])
           (for/or ([dir (in-list (if gui?
                                      (get-gui-bin-extra-search-dirs)
                                      (get-console-bin-extra-search-dirs)))])
             (file-or-directory-type (build-path dir name) #f)))))

  (define (prep-dir p)
    (define dir (path-only p))
    (make-directory* dir)
    p))

(require (submod "." private-install-helpers))

(define (installer path coll user? no-main?)
  (dispatch-to-installer-maker path coll user? no-main? do-installer))

(define (do-installer path collection user? tethered?)
  (define variants (available-mred-variants))
  (define tether-mode (and tethered? (if user? 'addon 'config)))
  ;; add a gracket-text executable that uses the -z flag (preferring a script)
  (for ([vs '((script-3m 3m) (script-cgc cgc) (script-cs cs))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (define exe-name (mred-program-launcher-path "gracket-text"
                                                       #:user? user?
                                                       #:tethered? tethered?
                                                       #:console? #t))
          (unless (exists-in-another-layer? exe-name user? tethered? #:gui? #f)
            (make-mred-launcher
             #:tether-mode tether-mode
             '("-z")
             (prep-dir exe-name)
             `([subsystem . console]
               [single-instance? . #f]
               [relative? . ,(not (or user? tethered?))])))))))
  ;; add a bin/gracket (in addition to lib/gracket)
  (for ([vs '((script-3m 3m) (script-cgc cgc) (script-cs cs))])
    (let ([v (findf (lambda (v) (memq v variants)) vs)])
      (when v
        (parameterize ([current-launcher-variant v])
          (define exe-name (mred-program-launcher-path "GRacket" #:user? user? #:tethered? tethered?))
          (unless (exists-in-another-layer? exe-name user? tethered? #:gui? #f)
            (make-mred-launcher #:tether-mode tether-mode
                                null
                                (prep-dir exe-name)
                                `([exe-name . "GRacket"]
                                  [relative? . ,(not (or user? tethered?))]
                                  [exe-is-gracket . #t]))))))))
