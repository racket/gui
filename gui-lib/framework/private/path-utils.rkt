#lang scheme/unit
  (require "sig.rkt"
           racket/file)
  
  (import framework:autosave/int^)
  (export framework:path-utils^)
  
(define (generate-autosave-name orig-name)
  (define-values (base name dir?)
    (if orig-name
        (split-path orig-name)
        (values (find-system-path 'doc-dir)
                (bytes->path-element #"mredauto")
                #f)))
  (define path
    (let ([base (if (path? base)
                    base
                    (current-directory))])
      (if (relative-path? base)
          (build-path (current-directory) base)
          base)))
  (with-autosave-filesystem-lock
      (λ ()
        (define autosave-toc (get-autosave-toc-content))
        (define autosave-filename
          (let loop ([n 1])
            (define numb (string->bytes/utf-8 (number->string n)))
            (define new-name
              (build-path path
                          (if (eq? (system-type) 'windows)
                              (bytes->path-element
                               (bytes-append (regexp-replace #rx#"\\..*$"
                                                             (path-element->bytes name)
                                                             #"")
                                             #"."
                                             numb))
                              (bytes->path-element
                               (bytes-append #"#"
                                             (path-element->bytes name)
                                             #"#"
                                             numb
                                             #"#")))))
            (define new-name-bytes (path->bytes new-name))
            (cond
              [(or (used-in-autosave-toc? new-name-bytes autosave-toc)
                   (file-exists? new-name))
               (loop (add1 n))]
              [else new-name])))
        (put-autosave-toc-content (cons (list (and orig-name (path->bytes orig-name))
                                              (path->bytes autosave-filename))
                                        autosave-toc))
        autosave-filename)))

(define (used-in-autosave-toc? new-name autosave-toc)
  (for/or ([mapping-entry (in-list autosave-toc)])
    (equal? new-name (list-ref mapping-entry 1))))
  
  (define (generate-backup-name full-name)
    (let-values ([(pre-base name dir?) (split-path full-name)])
      (let ([base (if (path? pre-base)
                      pre-base
                      (current-directory))])
        (let ([name-bytes (path-element->bytes name)])
          (cond
            [(and (eq? (system-type) 'windows)
                  (regexp-match #rx#"(.*)\\.[^.]*" name-bytes))
             =>
             (λ (m)
               (build-path base (bytes->path-element (bytes-append (cadr m) #".bak"))))]
            [(eq? (system-type) 'windows)
             (build-path base (bytes->path-element (bytes-append name-bytes #".bak")))]
            [else
             (build-path base (bytes->path-element (bytes-append name-bytes #"~")))])))))

