
(module autosave mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred")
           (lib "list.ss")
           (lib "string-constant.ss" "string-constants"))

  (provide autosave@)

  (define autosave@
    (unit/sig framework:autosave^
      (import mred^
	      [exit : framework:exit^]
	      [preferences : framework:preferences^]
              [frame : framework:frame^])
      
      (define autosavable<%>
	(interface ()
	  do-autosave))

      (define objects null)
      
      (define autosave-toc-filename
        (build-path (find-system-path 'pref-dir)
                    (case (system-type)
                      [(windows unix) ".plt-autosave-toc"]
                      [else "PLT-autosave-toc"])))
      
      (define autosave-toc-save-filename
        (build-path (find-system-path 'pref-dir)
                    (case (system-type)
                      [(windows unix) ".plt-autosave-toc-save"]
                      [else "PLT-autosave-toc-save"])))

      (define autosave-timer%
	(class timer%
	  (inherit start)
	  (define/override (notify)
            (when (preferences:get 'framework:autosaving-on?)
              (let-values ([(new-objects new-name-mapping) (rebuild-object-list)])
                (when (file-exists? autosave-toc-save-filename)
                  (delete-file autosave-toc-save-filename))
                (when (file-exists? autosave-toc-filename)
                  (copy-file autosave-toc-filename autosave-toc-save-filename))
                (call-with-output-file autosave-toc-filename
                  (lambda (port)
                    (write new-name-mapping port))
                  'truncate
                  'text)))
            (let ([seconds (preferences:get 'framework:autosave-delay)])
              (start (* 1000 seconds) #t)))
          (super-instantiate ())
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
                      (printf "autosave ~s ~s to ~s\n" object filename new-filename)
                      (loop (cdr orig-objects)
                            (if new-filename
                                (cons (list (and (not (unbox tmp-box)) filename)
                                            new-filename)
                                      name-mapping)
                                name-mapping)
                            (cons object-wb new-objects)))
                    (loop (cdr orig-objects)
                          name-mapping
                          new-objects))))))
      
      (define timer #f)

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
      
      ;; restore-autosave-files/gui : -> (union #f (is-a?/c top-level-window<%>))
      ;; opens a frame that lists the autosave files that have changed.
      (define (restore-autosave-files/gui)
        (and (file-exists? autosave-toc-filename)
             (let* ([table (call-with-input-file autosave-toc-filename read)]
                    ;; assume that the autosave file was deleted due to the file being saved
                    [filtered-table
                     (filter (lambda (x) (file-exists? (cadr x))) table)])
               (and (not (null? filtered-table))
                    (let ([f (make-object frame:basic% (string-constant recover-autosave-files-frame-title))])
                      (for-each (add-table-line (send f get-area-container)) filtered-table)
                      f)))))
      
      ;; add-table-line : (is-a? area-container<%>) 
      ;;               -> (list (union #f string[filename]) string[filename-file-exists?])
      ;;               -> void
      ;; adds in a line to the overview table showing this pair of files.
      (define (add-table-line area-container)
        (lambda (table-entry)
          (let* ([orig-file (car table-entry)]
                 [backup-file (cadr table-entry)]
                 [hp (instantiate horizontal-panel% ()
                       (parent area-container)
                       (style '(border))
                       (stretchable-height #f))]
                 [vp (instantiate vertical-panel% ()
                       (parent hp))]
                 [compare
                  (make-object button% (string-constant autosave-details) hp
                    (lambda (x y)
                      (show-differences table-entry)))]
                 [recover
                  (make-object button% (string-constant autosave-recover) hp
                    (lambda (x y)
                      (recover-file area-container hp table-entry)))]
                 [msg1-panel (instantiate horizontal-panel% ()
                               (parent vp))]
                 [msg1-label (instantiate message% ()
                               (parent msg1-panel)
                               (label (string-constant autosave-original-label)))]
                 [msg1 (instantiate message% ()
                         (label (or orig-file (string-constant autosave-unknown-filename)))
                         (stretchable-width #t)
                         (parent msg1-panel))]
                 [msg2-panel (instantiate horizontal-panel% ()
                               (parent vp))]
                 [msg2-label (instantiate message% ()
                               (parent msg2-panel)
                               (label (string-constant autosave-autosave-label)))]
                 [msg2 (instantiate message% ()
                         (label backup-file)
                         (stretchable-width #t)
                         (parent msg2-panel))])
            (let ([w (max (send msg1-label get-width) (send msg2-label get-width))])
              (send msg1-label min-width w)
              (send msg2-label min-width w))
            (send compare enable orig-file)
            (void))))
      
      (define (show-differences table-entry)
        (void))
      
      (define (recover-file parent child table-entry)
        (void)))))
