#lang racket/base

(require rackunit
         (rename-in framework
                    [path-utils:generate-autosave-name
                     generate-autosave-name]
                    [path-utils:generate-backup-name
                     generate-backup-name])
         racket/file
         racket/contract/base
         racket/sequence
         racket/list
         framework/preferences)

;; For uniform comparisons with equal?, all tests use simplified paths 
;; normalized with path->directory-path when applicable.


;; path-base: path? -> path?
;; Returns the directory of the input path (i.e. removes the final element),
;; normalized for comparison with equal?
(define (path-base pth)
  (define-values (base name dir?)
    (split-path pth))
  (path->directory-path base))


;; remove-all-write-permissions: path? -> any
;; Modifies the permissions of the given file/directory so no one can write to it
(define (remove-all-write-permissions pth)
  (define old-permissions-bits
    (file-or-directory-permissions pth 'bits))
  (file-or-directory-permissions
   pth
   (bitwise-and old-permissions-bits
                (bitwise-not user-write-bit)
                (bitwise-not group-write-bit)
                (bitwise-not other-write-bit))))


;; call-with-preference-not-writable: procedure? (-> any) -> any
;; Takes a procedure produced with preferences:get/set and a thunk.
;; Calls the thunk in a context where:
;;   (a) it ensures the preference will have the value it did
;;       when call-with-preference-not-writable was called, which
;;       is assumed to be a path, and
;;   (b) it removes all write permissions from the directory
;;       specified by the preference before calling the thunk.
;; After the thunk returns (or control escapes), call-with-preference-not-writable::
;;   (a) restores the directory's original permissions and
;;   (b) restores the preference to its initial value.
(define (call-with-preference-not-writable get/set-proc thunk)
  (define pth
    (get/set-proc))
  (define old-permissions-bits
    (file-or-directory-permissions pth 'bits))
  (dynamic-wind
   (λ ()
     (get/set-proc pth)
     (remove-all-write-permissions pth))
   thunk
   (λ ()
     (file-or-directory-permissions pth old-permissions-bits)
     (get/set-proc pth))))


;; See framework/private/path-utils for rationale
(define max-path-element-length
  255)


(define-syntax-rule (with-preference-testing-environment body ...)
  (let ([the-prefs-table (make-hash)])
    (parameterize ([preferences:low-level-put-preferences
                    (λ (syms vals)
                      (for ([sym (in-list syms)]
                            [val (in-list vals)])
                        (hash-set! the-prefs-table sym val)))]
                   [preferences:low-level-get-preference
                    (λ (sym [fail void])
                      (hash-ref the-prefs-table sym fail))])
      body ...)))

;                                          
;                                          
;                                          
;                                          
;    ;;                      ;;            
;    ;;                      ;;            
;  ;;;;;;;    ;;;     ;;   ;;;;;;;    ;;   
;    ;;     ;;   ;  ;;  ;    ;;     ;;  ;  
;    ;;     ;    ;   ;       ;;      ;     
;    ;;    ;;;;;;;;   ;;     ;;       ;;   
;    ;;     ;           ;;   ;;         ;; 
;     ;     ;;   ;  ;   ;     ;     ;   ;  
;      ;;;    ;;;    ;;;       ;;;   ;;;   
;                                          
;                                          
;                                          
;                                          
(with-preference-testing-environment
 (define current-backup-dir
   (preferences:get/set 'path-utils:backup-dir))
 (define current-autosave-dir
   (preferences:get/set 'path-utils:autosave-dir))
 (define elem
   (bytes->path-element #"example.rkt"))
 (define current-dir
   (path->directory-path
    (simplify-path (current-directory))))
 (define complete
   ; don't put the complete path in (current-directory) directly
   ; so that we can test that the functions don't just always use (current-directory)
   (build-path current-dir "somewhere" elem))
 (define dir-of-complete
   (path-base complete))
 (define too-long-pth
   (build-path
    current-dir
    (bytes->path-element
     (bytes-append
      (list->bytes
       (for/list ([i (in-range (+ 10 max-path-element-length))]
                  [byte (in-cycle
                         (sequence-filter
                          (compose1 (λ (c)
                                      (or (char-alphabetic? c)
                                          (char-numeric? c)))
                                    integer->char)
                          (in-range 48 123)))])
         byte))
      #".rkt"))))
 ;; Tests with #f for directories
 (current-backup-dir #f)
 (current-autosave-dir #f)
 (with-check-info (['path-utils:backup-dir (current-backup-dir)]
                   ['path-utils:autosave-dir (current-autosave-dir)])
   (check-false (current-backup-dir)
                "backup dir should be #f")
   (check-false (current-autosave-dir)
                "autosave dir should be #f")
   (with-check-info (['|generate- function| (string-info "path-utils:generate-autosave-name")])
     (check-equal? (path-base (simplify-path (generate-autosave-name #f)))
                   (path->directory-path (find-system-path 'doc-dir))
                   "#f orig name should use 'doc-dir")
     (check-equal? (path-base (simplify-path (generate-autosave-name elem)))
                   current-dir
                   "should resolve relative using current directory")
     (check-equal? (path-base (generate-autosave-name complete))
                   dir-of-complete
                   "complete path should use that directory"))
   (with-check-info (['|generate- function| (string-info "path-utils:generate-backup-name")])
     (check-equal? (path-base (simplify-path (generate-backup-name elem)))
                   current-dir
                   "should resolve relative using current directory")
     (check-equal? (path-base (generate-backup-name complete))
                   dir-of-complete
                   "complete path should use that directory")))
 ;; Tests with designated directories
 (define (make-temp-directory/normalize-path fmt-str)
   (path->directory-path
    (simplify-path
     (make-temporary-file fmt-str 'directory))))
 (define backup-dir
   (make-temp-directory/normalize-path "rkt-backup-dir-~a"))
 (define autosave-dir
   (make-temp-directory/normalize-path "rkt-autosave-dir-~a"))
 (dynamic-wind
  void
  (λ () 
    (current-backup-dir backup-dir)
    (current-autosave-dir autosave-dir)
    (define clashing-name
      (build-path current-dir "elsewhere" elem))
    (with-check-info (['path-utils:backup-dir (current-backup-dir)]
                      ['path-utils:autosave-dir (current-autosave-dir)])
      (check-equal? (current-backup-dir)
                    backup-dir
                    "should be using temporary backup dir")
      (check-equal? (current-autosave-dir)
                    autosave-dir
                    "should be using temporary autosave dir")
      (with-check-info (['|generate- function| (string-info "path-utils:generate-autosave-name")])
        (check-equal? (path-base (generate-autosave-name #f))
                      autosave-dir
                      "#f orig name should use autosave dir")
        (check-equal? (path-base (generate-autosave-name elem))
                      autosave-dir
                      "relative path should use autosave dir")
        (check-equal? (path-base (generate-autosave-name complete))
                      autosave-dir
                      "complete path should use autosave dir")
        (check-false
         (equal? (simplify-path (generate-autosave-name complete))
                 (simplify-path (generate-autosave-name clashing-name)))
         "files with the same name in different directories should not collide")
        ; long path element
        (check-not-false
         (< (bytes-length (path-element->bytes
                           (last (explode-path (generate-autosave-name
                                                too-long-pth)))))
            max-path-element-length)
         "excessively long final elements should be shortened")
        ; write permission
        (call-with-preference-not-writable
         current-autosave-dir
         (λ ()
           (test-case
            "autosave dir not writable"
            (check-false (memq 'write (file-or-directory-permissions autosave-dir))
                         "autosave dir should have been made non-writable")
            (check-equal? (path-base (generate-autosave-name complete))
                          dir-of-complete
                          "should fall back when autosave dir not writable"))))
        ; delete
        (delete-directory autosave-dir)
        (check-equal? (path-base (generate-autosave-name complete))
                      dir-of-complete
                      "should fall back when autosave dir deleted"))
      (with-check-info (['|generate- function| (string-info "path-utils:generate-backup-name")])
        (check-equal? (path-base (generate-backup-name elem))
                      backup-dir
                      "relative path should use backup dir")
        (check-equal? (path-base (generate-backup-name complete))
                      backup-dir
                      "complete path should use backup dir")
        (check-false
         (equal? (simplify-path (generate-backup-name complete))
                 (simplify-path (generate-backup-name clashing-name)))
         "files with the same name in different directories should not collide")
        ; long path element
        (check-not-false
         (< (bytes-length (path-element->bytes
                           (last (explode-path (generate-backup-name
                                                too-long-pth)))))
            max-path-element-length)
         "excessively long final elements should be shortened")
        ; write permission
        (call-with-preference-not-writable
         current-backup-dir
         (λ ()
           (test-case
            "backup dir not writable"
            (check-false (memq 'write (file-or-directory-permissions backup-dir))
                         "backup dir should have been made non-writable")
            (check-equal? (path-base (generate-backup-name complete))
                          dir-of-complete
                          "should fall back when backup dir not writable"))))
        ; delete
        (delete-directory backup-dir)
        (check-equal? (path-base (generate-backup-name complete))
                      dir-of-complete
                      "should fall back when backup dir deleted"))))
  (λ ()
    (when (directory-exists? backup-dir)
      (delete-directory backup-dir))
    (when (directory-exists? autosave-dir)
      (delete-directory autosave-dir)))))





