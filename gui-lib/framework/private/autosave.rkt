#lang racket/base

(require racket/class
         racket/unit
         racket/file
         racket/match
         "sig.rkt"
         "text-sig.rkt"
         "../gui-utils.rkt"
         "../preferences.rkt"
         "srcloc-panel.rkt"
         mred/mred-sig
         string-constants)
(provide autosave@)

(define-unit autosave@
  (import mred^
          [prefix exit: framework:exit^]
          [prefix frame: framework:frame^]
          [prefix racket: framework:racket^]
          [prefix editor: framework:editor^]
          [prefix text: framework:text^]
          [prefix finder: framework:finder^]
          [prefix group: framework:group^]
          [prefix canvas: framework:canvas^])
  
  (export framework:autosave^)
  
  (define autosavable<%>
    (interface ()
      do-autosave))
  
  (define objects null)
  
  (define current-toc-path
    (make-parameter
     (build-path (find-system-path 'pref-dir)
                 (case (system-type)
                   [(unix) ".plt-autosave-toc.rktd"]
                   [else "PLT-autosave-toc.rktd"]))))
  (define toc-path (current-toc-path))

  (define (get-autosave-toc-save-filename)
    (define toc-path (current-toc-path))
    (define-values (base name dir) (split-path toc-path))
    (define save-filename-path
      (case (system-type)
        [(unix) ".plt-autosave-toc-save.rktd"]
        [else "PLT-autosave-toc-save.rktd"]))
    (make-directory* base)
    (build-path base save-filename-path))
  
  (define autosave-timer%
    (class timer%
      (inherit start)
      (field [last-name-mapping #f])
      (define/override (notify)
        (when (preferences:get 'framework:autosaving-on?)
          (let-values ([(new-objects new-name-mapping) (rebuild-object-list)])
            (define toc-path (current-toc-path))
            (define autosave-toc-save-filename (get-autosave-toc-save-filename))
            (set! objects new-objects)
            (unless (equal? last-name-mapping new-name-mapping)
              (set! last-name-mapping new-name-mapping)
              (when (file-exists? autosave-toc-save-filename)
                (delete-file autosave-toc-save-filename))
              (when (file-exists? toc-path)
                (copy-file toc-path autosave-toc-save-filename))
              (call-with-output-file toc-path
                (λ (port)
                  (write new-name-mapping port))
                #:exists 'truncate
                #:mode 'text))))
        (cond
          [(null? objects) (set! timer #f)]
          [else
           (let ([seconds (preferences:get 'framework:autosave-delay)])
             (start (* 1000 seconds) #t))]))
      (super-new)
      (let ([seconds (preferences:get 'framework:autosave-delay)])
        (start (* 1000 seconds) #t))))
  
  ;; rebuild-object-list : -> (values (listof (weak-box (is-a?/c editor<%>)))
  ;;                                  (listof (list (union #f string[filename]) string[filename]))
  (define (rebuild-object-list)
    (let loop ([orig-objects objects]
               [name-mapping null]
               [new-objects null])
      (if (null? orig-objects)
          (values new-objects name-mapping)
          (let* ([object-wb (car orig-objects)]
                 [object (weak-box-value object-wb)])
            (if object
                (let* ([new-filename (send object do-autosave)]
                       [tmp-box (box #f)]
                       [filename (send object get-filename tmp-box)])
                  (loop (cdr orig-objects)
                        (if new-filename
                            (cons (list (and (not (unbox tmp-box)) 
                                             filename
                                             (path->bytes filename))
                                        (and new-filename 
                                             (path->bytes new-filename)))
                                  name-mapping)
                            name-mapping)
                        (cons object-wb new-objects)))
                (loop (cdr orig-objects)
                      name-mapping
                      new-objects))))))
  
  (define timer #f)
  ;; when the autosave delay is changed then we
  ;; trigger an autosave right away and let the
  ;; callback trigger the next one at the right interval
  (preferences:add-callback
   'framework:autosave-delay
   (λ (k v)
     (when timer
       (send timer stop)
       (send timer start 0 #t))))
  

  
  (define (register b)
    (unless timer
      (set! timer (make-object autosave-timer%)))
    (set! objects
          (let loop ([objects objects])
            (cond
              [(null? objects) (list (make-weak-box b))]
              [else (let ([weak-box (car objects)])
                      (if (weak-box-value weak-box)
                          (cons weak-box (loop (cdr objects)))
                          (loop (cdr objects))))]))))
  
  ;; restore-autosave-files/gui : -> void?
  ;; opens a frame that lists the autosave files that have changed.
  (define (restore-autosave-files/gui [table #f])
    (cond
      [table (restore-autosave-files/gui/table table)]
      [else
       (define toc-path (current-toc-path))
       (when (file-exists? toc-path)
         ;; Load table from file, and check that the file was not corrupted
         (define raw-read-table
           (with-handlers ([exn:fail? (λ (x) null)])
             (call-with-input-file toc-path read)))
         (define table
           (if (and (list? raw-read-table)
                    (andmap (λ (i)
                              (and (list? i)
                                   (= 2 (length i))
                                   (or (not (car i))
                                       (bytes? (car i)))
                                   (bytes? (cadr i))))
                            raw-read-table))
               (map (λ (ent) (list (if (bytes? (list-ref ent 0))
                                       (bytes->path (list-ref ent 0))
                                       #f)
                                   (bytes->path (list-ref ent 1))))
                    raw-read-table)
               null))
           (restore-autosave-files/gui/table table))]))

  (define (restore-autosave-files/gui/table table)

    ;; main : -> void
    ;; start everything going
    (define (main)
      ;; assume that the autosave file was deleted due to the file being saved
      (define filtered-table (filter (λ (x) (file-exists? (cadr x))) table))
      (unless (null? filtered-table)
        (define dlg
          (new (frame:focus-table-mixin dialog%)
               (label (string-constant recover-autosave-files-frame-title))
               [width 600]))
        (define t
          (new (text:foreground-color-mixin
                (editor:standard-style-list-mixin text:basic%))
               [auto-wrap #t]))
        (define ec (new canvas:color%
                        (parent dlg)
                        (editor t)
                        (line-count 2)
                        (stretchable-height #f)
                        (style '(no-hscroll))))
        (define hp (new-horizontal-panel%
                    [parent dlg]
                    [stretchable-height #f]))
        (define vp (new-vertical-panel%
                    [parent hp]
                    [stretchable-height #f]))
        (define details-parent (new-horizontal-panel% [parent dlg]))
        (send vp set-alignment 'right 'center)
        (make-object grow-box-spacer-pane% hp)
        (send t insert (string-constant autosave-explanation))
        (send t hide-caret #t)
        (send t set-position 0 0)
        (send t lock #t)

        (define only-one? (= 1 (length filtered-table)))
        (for ([table-entry (in-list filtered-table)])
          (add-table-line vp dlg details-parent table-entry only-one?))
        (new button%
          [label (string-constant autosave-done)]
          [parent vp]
          [callback
           (λ (x y)
             (when (send dlg can-close?)
               (send dlg on-close)
               (send dlg show #f)))])
        (send dlg show #t)
        (void)))

    ;; add-table-line : (is-a? area-container<%>)
    ;;                  (or/c #f (is-a?/c top-level-window<%>))
    ;;                  (is-a? area-container<%>)
    ;;               -> (list/c (or/c #f path?) path?)
    ;;               -> void?
    ;; adds in a line to the overview table showing this pair of files.
    (define (add-table-line area-container dlg show-details-parent table-entry show-details-initially/no-details-button?)
      (match-define (list orig-file backup-file) table-entry)
      (define hp (new-horizontal-panel%
                  (parent area-container)
                  (style '(border))
                  (stretchable-height #f)))
      (define vp (new-vertical-panel%
                  (parent hp)))
      (define msg1-panel (new-horizontal-panel%
                          (parent vp)))
      (define msg1-label
        (new message%
             (parent msg1-panel)
             (label (string-constant autosave-original-label:))))
      (define msg1
        (new message%
             (label (if orig-file (path->string orig-file) (string-constant autosave-unknown-filename)))
             (stretchable-width #t)
             (parent msg1-panel)))
      (define msg2-panel (new-horizontal-panel% (parent vp)))
      (define msg2-label (new message%
                              (parent msg2-panel)
                              (label (string-constant autosave-autosave-label:))))
      (define msg2 (new message%
                        (label (path->string backup-file))
                        (stretchable-width #t)
                        (parent msg2-panel)))
      (define (details-callback)
        (show-files table-entry show-details-parent dlg))
      (define details
        (and (not show-details-initially/no-details-button?)
             (new button%
                  [label (string-constant autosave-details)]
                  [parent hp]
                  [callback
                   (λ (x y) (details-callback))])))
      (define delete
        (new button%
          [label (string-constant autosave-delete-button)]
          [parent hp]
          [callback
           (λ (delete y)
             (when (delete-autosave table-entry)
               (disable-line)
               (send msg2 set-label (string-constant autosave-deleted))))]))
      (define recover
        (new button%
             [label (string-constant autosave-recover)]
             [parent hp]
             [callback
              (λ (recover y)
                (let ([filename-result (recover-file dlg table-entry)])
                  (when filename-result
                    (disable-line)
                    (send msg2 set-label (string-constant autosave-recovered!))
                    (send msg1 set-label (gui-utils:quote-literal-label
                                          (path->string filename-result)
                                          #:quote-amp? #f)))))]))
      (define (disable-line)
        (send recover enable #f)
        (when details (send details enable #f))
        (send delete enable #f))
      (define w (max (send msg1-label get-width) (send msg2-label get-width)))
      (send msg1-label min-width w)
      (send msg2-label min-width w)
      (when show-details-initially/no-details-button?
        (details-callback))
      (void))

    ;; delete-autosave : (list (union #f string[filename]) string[filename]) -> boolean
    ;; result indicates if delete occurred
    (define (delete-autosave table-entry)
      (define autosave-file (cadr table-entry))
      (and (gui-utils:get-choice
            (format (string-constant are-you-sure-delete?)
                    autosave-file)
            (string-constant autosave-delete-title)
            (string-constant cancel)
            (string-constant warning)
            #f
            #:dialog-mixin frame:focus-table-mixin)
           (with-handlers ([exn:fail?
                            (λ (exn)
                              (message-box
                               (string-constant warning)
                               (format (string-constant autosave-error-deleting)
                                       autosave-file
                                       (if (exn? exn)
                                           (format "~a" (exn-message exn))
                                           (format "~s" exn)))
                               #:dialog-mixin frame:focus-table-mixin)
                              #f)])
             (delete-file autosave-file)
             #t)))

    ;; show-files : (list (or/c #f path?) path?) (is-a?/c area-container<%>) (is-a?/c dialog%) -> void
    (define (show-files table-entry show-details-parent dlg)
      (match-define (list file1 file2) table-entry)
      (send dlg begin-container-sequence)
      (define had-children? #f)
      (send show-details-parent change-children (λ (x)
                                                  (set! had-children? (not (null? x)))
                                                  '()))
      (when file1
        (add-file-viewer file1 show-details-parent (string-constant autosave-original-label)))
      (add-file-viewer file2 show-details-parent (string-constant autosave-autosave-label))
      (send dlg end-container-sequence)
      (unless had-children?
        (send dlg center)))

    ;; add-file-viewer : path? -> void
    (define (add-file-viewer filename parent label)
      (define vp (make-object vertical-panel% parent))
      (define t (make-object show-files-text%))
      (define msg1 (make-object message% label vp))
      (define msg2 (new message%
                        [label (gui-utils:quote-literal-label
                                (path->string filename)
                                #:quote-amp? #f)]
                        [parent vp]))
      (define ec (make-object canvas:color% vp t))
      (send ec min-height 400)
      (with-handlers ([exn:fail? (λ (exn)
                                   (send t insert "Error loading original file:\n\n")
                                   (define p (send t last-position))
                                   (send t insert (exn-message exn))
                                   (send t set-position 0))])
        (send t load-file filename))
      (send t hide-caret #t)
      (send t lock #t))

    (define show-files-text% (text:foreground-color-mixin
                              text:keymap%))

    (main))

  ;; recover-file : (union #f (is-a?/c toplevel-window<%>))
  ;;                (list (union #f string[filename]) string)
  ;;             -> (union #f string)
  (define (recover-file parent table-entry)
    (let ([orig-name (or (car table-entry)
                         (parameterize ([finder:dialog-parent-parameter parent])
                           (finder:put-file #f #f #f
                                            (string-constant autosave-restore-to-where?))))])
      (and orig-name
           (let ([autosave-name (cadr table-entry)])
             (let ([tmp-name (and (file-exists? orig-name)
                                  (make-temporary-file "autosave-repair~a" orig-name))])
               (when (file-exists? orig-name)
                 (let loop ([i 0])
                   (cond
                     [(= i 100)
                      ;; here we either have a LOT of renamed files or something
                      ;; is going wrong; just give up on the original file
                      (delete-file orig-name)]
                     [else
                      (define-values (base name dir) (split-path orig-name))
                      (define split-name-m (regexp-match #rx#"^(.*)([.][^.]*)$" (path->bytes name)))
                      (define new-suffix (string->bytes/utf-8 (format "~a~a" "-autorec" (if (= i 0) "" i))))
                      (define renamed-candidate
                        (if split-name-m
                            (bytes-append (list-ref split-name-m 0)
                                          new-suffix
                                          #"."
                                          (list-ref split-name-m 1))
                            (bytes-append (path->bytes name) new-suffix)))
                      (define full-candidate (build-path base (bytes->path renamed-candidate)))
                      (cond
                        [(file-exists? full-candidate)
                         (loop (+ i 1))]
                        [else
                         (rename-file-or-directory orig-name full-candidate)])])))
               (copy-file autosave-name orig-name)
               (delete-file autosave-name)
               (when tmp-name
                 (delete-file tmp-name))
               orig-name))))))
