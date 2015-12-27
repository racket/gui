#lang racket/base
(require wxme ;; this is dynamically required
         racket/gui/base
         racket/file
         racket/runtime-path
         racket/port)
(define collection-name "circle-snip")
(define snip-example-name "main.rkt")
(define-runtime-path snip-example.rkt "snip-example.rkt")
(define-runtime-path wxme-circle-snip.rkt "wxme-circle-snip.rkt")
(define new-lib-coll-dir
  (make-temporary-file "scribblings-gui-test-snip-example-~a"
                       'directory))
(dynamic-wind
 void
 (λ ()
   (make-directory (build-path new-lib-coll-dir collection-name))
   (copy-file snip-example.rkt
              (build-path new-lib-coll-dir collection-name snip-example-name))
   (copy-file wxme-circle-snip.rkt
              (build-path new-lib-coll-dir collection-name "wxme-circle-snip.rkt"))
   
   
   (define orig-namespace (current-namespace))
   (parameterize ([current-library-collection-paths
                   (cons new-lib-coll-dir
                         (current-library-collection-paths))])
     (define save-filename (build-path new-lib-coll-dir collection-name "circle.rkt"))
     (define circle-snip-pos #f)
     (define (get-circle-snip-pos) circle-snip-pos)
     (define (set-circle-snip-pos p) (set! circle-snip-pos p))
     (parameterize ([current-namespace (make-base-namespace)])
       (namespace-attach-module orig-namespace 'mred/mred)
       (define circle-snip% (dynamic-require `(lib ,snip-example-name ,collection-name)
                                             'circle-snip%))
       (eval '(require racket/gui/base racket/class racket/format))
       (eval
        `(let ()
           (define circle-snip% ,circle-snip%)
           (define t (new text%))
           (send t insert "#lang racket/base\n")
           (send t insert "(define s ")
           (,set-circle-snip-pos (send t last-position))
           (send t insert (new circle-snip%))
           (send t insert ")\n")
           (send t insert (~s `(provide s)))
           (send t save-file ,save-filename)
           (send t set-filename #f)
           (define t2 (new text%))
           (send t2 set-filename ,save-filename)
           (send t2 load-file)
           (define circle-snip-copy (send t find-snip (,get-circle-snip-pos) 'after))
           (unless (is-a? circle-snip-copy circle-snip%)
             (error 'test-snip-example.rtk "didnt find circle snip.1, found ~s"
                    circle-snip-copy))
           (define gui-loaded (dynamic-require ,save-filename 's))
           (unless (is-a? gui-loaded circle-snip%)
             (error 'test-snip-example.rkt "didnt find circle snip.2, found ~s"
                    gui-loaded)))))
     
     (parameterize ([current-namespace (make-base-namespace)])
       (namespace-attach-module orig-namespace 'mred/mred)
       (define loaded (format "~s" (dynamic-require save-filename 's)))
       (unless (regexp-match #rx"struct:object:circle-snip%" loaded)
         (error 'test-snip-example.rkt "didn't find circle snip.3, found ~s" loaded)))

     (define wxme-text-content
       (parameterize ([current-namespace (make-base-namespace)])
         (eval '(require racket/base wxme))
         (eval
          `(call-with-input-file ,save-filename
             (λ (port)
               (apply
                string
                (for/list ([s (in-input-port-chars (wxme-port->text-port port))])
                  s)))))))
     (unless (regexp-match #rx"[(]circle [0-9.]+[)]" wxme-text-content)
       (error 'test-snip-example.rkt "didn't find circle snip.4 ~s" wxme-text-content))

     (define wxme-content-as-pos
       (parameterize ([current-namespace (make-base-namespace)])
         (eval '(require racket/base wxme))
         (eval
          `(call-with-input-file ,save-filename
             (λ (port)
               (port-count-lines! port)
               (for/or ([s (in-port read-char-or-special
                                    (wxme-port->port port))])
                 (and (syntax? s)
                      (list (syntax-position s)))))))))
     (unless (equal? (list (+ circle-snip-pos 1)) wxme-content-as-pos)
       (error 'test-snip-example.rkt "didn't find circle snip.5 ~s vs ~s"
              wxme-content-as-pos
              circle-snip-pos))))
 
 (λ ()
   (delete-directory/files new-lib-coll-dir)))
