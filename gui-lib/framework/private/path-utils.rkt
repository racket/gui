#lang racket/base

(require "sig.rkt"
         "../preferences.rkt"
         racket/unit)

(provide path-utils@)

(define-unit-from-context path-utils@
  framework:path-utils^)
  
(define path-utils:backup-dir
  'path-utils:backup-dir)

(define path-utils:autosave-dir
  'path-utils:autosave-dir)

(define (valid-preference-value? v)
  (or (not v)
      (and (path? v)
           (complete-path? v)
           (directory-exists? v))))

(define (marshall:maybe-path->writable v)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (and v (path->bytes v))))

(define (unmarshall:maybe-bytes->path v)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (and v (bytes->path v))))

(preferences:set-default path-utils:backup-dir #f valid-preference-value?)
(preferences:set-default path-utils:autosave-dir #f valid-preference-value?)
(preferences:set-un/marshall path-utils:backup-dir
                             marshall:maybe-path->writable
                             unmarshall:maybe-bytes->path)
(preferences:set-un/marshall path-utils:autosave-dir
                             marshall:maybe-path->writable
                             unmarshall:maybe-bytes->path)

(define current-backup-dir
  (preferences:get/set path-utils:backup-dir))

(define current-autosave-dir
  (preferences:get/set path-utils:autosave-dir))

; generate-autosave-name : (or/c #f path-string? path-for-some-system?) -> path?
(define (generate-autosave-name maybe-old-path)
  (cond
    [maybe-old-path
     (let*-values ([(base name dir?) (split-path maybe-old-path)]
                   [(base) (cond
                             [(not (path? base))
                              (current-directory)]
                             [(relative-path? base)
                              (build-path (current-directory) base)]
                             [else
                              base])])
       (cond
         [(current-autosave-dir)
          =>
          (λ (dir)
            (make-unique-autosave-name dir (encode-as-path-element base name)))]
         [else
          (make-unique-autosave-name base name)]))]
    [else
     (make-unique-autosave-name (or (current-autosave-dir)
                                    (find-system-path 'doc-dir))
                                (bytes->path-element #"mredauto"))]))


; make-unique-autosave-name : dir-path path-element -> path?
(define (make-unique-autosave-name dir name)
  (define sys
    (system-path-convention-type))
  (let loop ([n 1])
    (let* ([numb (string->bytes/utf-8 (number->string n))]
           [new-name
            (build-path dir
                        (bytes->path-element
                         (case sys
                           [(windows)
                            (bytes-append (regexp-replace #rx#"\\..*$" 
                                                          (path-element->bytes name)
                                                          #"")
                                          #"."
                                          numb)]
                           [else
                            (bytes-append #"#"
                                          (path-element->bytes name)
                                          #"#"
                                          numb
                                          #"#")])))])
      (if (file-exists? new-name)
          (loop (add1 n))
          new-name))))


; path-utils:generate-backup-name : path? -> path?
(define (generate-backup-name full-name)
  (define-values (pre-base name dir?)
    (split-path full-name))
  (define base
    (if (path? pre-base)
        pre-base
        (current-directory)))
  (define name-element
    (let ([name-bytes (path-element->bytes name)]
          [windows? (eq? (system-path-convention-type) 'windows)])
      (bytes->path-element
       (cond
         [(and windows?
               (regexp-match #rx#"(.*)\\.[^.]*" name-bytes))
          =>
          (λ (m)
            (bytes-append (cadr m) #".bak"))]
         [windows?
          (bytes-append name-bytes #".bak")]
         [else
          (bytes-append name-bytes #"~")]))))
  (cond
    [(current-backup-dir)
     =>
     (λ (dir)
       (build-path dir (encode-as-path-element base name-element)))]
    [else
     (build-path base name-element)]))


; make-unique-autosave-name : dir-path path-element -> path-element
; N.B. generate-backup-name may supply a relative directory, but
; we should always use a complete one.
; Using simplify-path does that and ensures no 'up or 'same
; Using ! is not completely robust, but works well enough for Emacs.
(define (encode-as-path-element base-maybe-relative name)
  (bytes->path-element
   (regexp-replace* (case (system-path-convention-type)
                      [(windows) #rx#"\\\\"]
                      [else #rx#"/"])
                    (path->bytes
                     (simplify-path (build-path base-maybe-relative name)))
                    #"!")))
            
#;
(module+ test
  (define dir
    (bytes->path
     (case (system-path-convention-type)
       [(windows) #"C:\\\\Users\\Foo"]
       [else #"/home/foo"])))
  (define elem
    (bytes->path-element #"example.rkt"))
  (build-path dir (encode-as-path-element dir elem))
  (newline)
  (define complete
    (build-path dir elem))
  (define old-backup-dir
    (current-backup-dir))
  (define old-autosave-dir
    (current-autosave-dir))
  (define (high-level-tests)
    (println (generate-autosave-name #f))
    (println (generate-autosave-name elem))
    (println (generate-autosave-name complete))
    (println (generate-backup-name elem))
    (println (generate-backup-name complete)))
  (dynamic-wind
   void
   (λ ()
     (current-backup-dir #f)
     (current-autosave-dir #f)
     (high-level-tests)
     (newline)
     ; nb for the following to work, must disable
     ; directory-exists? check
     (current-backup-dir
      (build-path dir (bytes->path-element #"backup")))
     (current-autosave-dir
      (build-path dir (bytes->path-element #"autosave")))
     (high-level-tests))
   (λ ()
     (current-backup-dir old-backup-dir)
     (current-autosave-dir old-autosave-dir))))
  



