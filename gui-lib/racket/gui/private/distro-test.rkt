#lang racket/base
(require setup/dirs
         racket/system
         compiler/find-exe)

;; Sanity checks to run in an installer-building context to make sure
;; that things bascially work. This test is in the "-lib" package,
;; instead of the "-test" package, so that it's lightweight to run
;; (without installing lots of other packages)

(define bin-dir (find-gui-bin-dir))
(define console-bin-dir (find-console-bin-dir))

(define (try-exe p)
  (printf "Trying ~a\n" p)
  (let ([o (open-output-bytes)])
    (parameterize ([current-output-port o])
      (system* p "-e" "'hello"))
    ;; For historical reasons, `gracket` still uses `scheme` printing
    (unless (equal? #"hello\n" (get-output-bytes o))
      (error "sanity check failed" p))))

(try-exe (build-path console-bin-dir
                     (let ()
                       (define me (path-replace-suffix (find-exe) #""))
                       (define ending (regexp-match #rx#"(?i:racket)([cs3mg]*)$" me))
                       (define s2 (string-append "gracket-text" (bytes->string/utf-8 (cadr ending))))
                       (if (eq? (system-type) 'windows)
                           (string-append s2 ".exe")
                           s2))))

(unless (eq? (system-type) 'unix) ; may not have a GUI connection on Unix
  (case (system-type)
    [(windows) (try-exe (build-path bin-dir "GRacket.exe"))]
    [(macosx) (try-exe (build-path console-bin-dir "gracket"))]))
