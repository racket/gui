#lang racket/unit

(require "sig.rkt"
         racket/list
         "../preferences.rkt")
  
(import)
(export framework:path-utils^)

;; preferences initialized in main.rkt

(define (make-getter/ensure-exists pref-sym)
  (λ ()
    (let ([maybe-dir (preferences:get pref-sym)])
      (and maybe-dir
           (directory-exists? maybe-dir)
           (memq 'write (file-or-directory-permissions maybe-dir))
           maybe-dir))))

(define current-backup-dir
  (make-getter/ensure-exists 'path-utils:backup-dir))

(define current-autosave-dir
  (make-getter/ensure-exists 'path-utils:autosave-dir))

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
                        (case sys
                          [(windows)
                           (path-replace-extension name
                                                   (bytes-append #"."
                                                                 numb))]
                          [else
                           (bytes->path-element
                            (bytes-append #"#"
                                          (path-element->bytes name)
                                          #"#"
                                          numb
                                          #"#"))]))])
      (if (file-exists? new-name)
          (loop (add1 n))
          new-name))))


;; generate-backup-name : path? -> path?
(define (generate-backup-name full-name)
  (define-values (pre-base old-name dir?)
    (split-path full-name))
  (define base
    (if (path? pre-base)
        pre-base
        (current-directory)))
  (define name-element
    (case (system-path-convention-type)
      [(windows)
       (path-replace-extension old-name #".bak")]
      [else
       (bytes->path-element
        (bytes-append (path-element->bytes old-name) #"~"))]))
  (cond
    [(current-backup-dir)
     =>
     (λ (dir)
       (build-path dir (encode-as-path-element base name-element)))]
    [else
     (build-path base name-element)]))


(define candidate-separators
  `(#"!" #"%" #"_" #"|" #":" #">" #"^" #"$" #"@" #"*" #"?"))

(define separator-regexps
  (map (compose1 byte-regexp regexp-quote) candidate-separators))

; encode-as-path-element : dir-path path-element -> path-element
; N.B. generate-backup-name may supply a relative directory, but
; we should always use a complete one.
; That is handled by simplify+explode-path->bytes.
; Windows has limitations on path lengths. Racket handles MAX_PATH
; by using "\\?\" paths when necessary, but individual elements must
; be shorter than lpMaximumComponentLength.
; We respect this limit (on all platforms, for consistency)
; by replacing some bytes from the middle if necessary.
(define (encode-as-path-element base-maybe-relative name)
  (define illegal-rx
    (case (system-path-convention-type)
      [(windows) #rx#"\\\\"]
      [else #rx#"/"]))
  (define l-bytes
    (simplify+explode-path->bytes (build-path base-maybe-relative name)))
  (define separator-byte
    (or (let ([all-components (apply bytes-append l-bytes)])
          (for/first ([sep (in-list candidate-separators)]
                      [rx (in-list separator-regexps)]
                      #:unless (regexp-match? rx all-components))
            sep))
        #"!"))
  (define legible-name-bytes
    (apply
     bytes-append
     separator-byte
     (add-between
      (for/list ([elem (in-list l-bytes)])
        (regexp-replace* illegal-rx elem separator-byte))
      separator-byte)))
  (define num-legible-bytes
    (bytes-length legible-name-bytes))
  (bytes->path-element
   (cond
     [(< num-legible-bytes
         (lpMaximumComponentLength))
      legible-name-bytes]
     [else
      (define replacement
        (bytes-append separator-byte #"..." separator-byte))
      (define num-excess-bytes
        (+ (- num-legible-bytes
              (lpMaximumComponentLength))
           5 ; extra margin of safety
           (bytes-length replacement)))
      (define num-bytes-to-keep-per-side
        (floor (/ (- num-legible-bytes num-excess-bytes)
                  2)))
      (bytes-append
       (subbytes legible-name-bytes 0 num-bytes-to-keep-per-side)
       replacement
       (subbytes legible-name-bytes (- num-legible-bytes
                                       num-bytes-to-keep-per-side)))])))
     

;; simplify+explode-path->bytes : path? -> (listof bytes?)
;; Useful because path-element->bytes doesn't work on root paths.
;; Using simplify-path ensures no 'up or 'same.
(define (simplify+explode-path->bytes pth)
  (define elems
    (explode-path (simplify-path pth)))
  (cons (path->bytes (car elems))
        (map path-element->bytes (cdr elems))))

;; lpMaximumComponentLength : -> real?
;; Returns the maximum length of an element of a "\\?\" path on Windows.
;; For now, assuming 255, but really this should be
;; "the value returned in the lpMaximumComponentLength parameter
;; of the GetVolumeInformation function".
;; See https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx#maxpath
(define (lpMaximumComponentLength)
  255)


