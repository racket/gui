
(module autosave mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred")
           (lib "list.ss")
           (lib "string-constant.ss" "string-constants")
           ;(file "/home/robby/cvs/plt/collects/string-constants/string-constant.ss")
           )

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
                      (printf "autosave ~s to ~s\n" filename new-filename)
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
      
      ;; removed-autosave : string[filename] -> void
      ;; cal to indicate to that autosave filename returned from `do-autosave'
      ;; has been deleted (eg, the file was saved, or the user closed the window, etc)
      (define (removed-autosave filename)
        (when (file-exists? autosave-toc-filename)
          (let* ([old-contents (call-with-input-file autosave-toc-filename read)]
                 [new-contents
                  (remove filename
                          old-contents
                          (lambda (filename table-entry)
                            (equal? (cadr table-entry) filename)))])
            (when (file-exists? autosave-toc-save-filename)
              (delete-file autosave-toc-save-filename))
            (copy-file autosave-toc-filename autosave-toc-save-filename)
            (call-with-output-file autosave-toc-filename
              (lambda (port)
                (write new-contents port))
              'truncate
              'text))))
      
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
      
      ;; restore-autosave-files/gui : -> void
      ;; opens a frame that lists the autosave files that have changed.
      (define (restore-autosave-files/gui)
        (when (file-exists? autosave-toc-filename)
          (let* ([table (call-with-input-file autosave-toc-filename read)]
                 ;; assume that the autosave file was deleted due to the file being saved
                 [filtered-table
                  (filter (lambda (x) (file-exists? (cadr x))) table)])
            (unless (null? filtered-table)
              (let ([f (make-object frame:basic% (string-constant recover-autosave-files-frame-title) #f 400 400)])
                (for-each (add-table-line f) filtered-table)
                (send f show #t))))))
      
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
                       (stretchable-height #f))]
                 [vp (instantiate vertical-panel% ()
                       (parent hp))]
                 [compare
                  (make-object button% (string-constant autosave-details) vp
                    (lambda (x y)
                      (show-differences table-entry)))]
                 [recover
                  (make-object button% (string-constant autosave-recover) vp
                    (lambda (x y)
                      (recover-file area-container hp table-entry)))]
                 [msg1 (make-object message% (or orig-file (string-constant autosave-unknown-filename)) vp)]
                 [msg2 (make-object message% (cadr table-entry) vp)])
            (send compare enable orig-file)
            (void))))
      
      (define (show-differences table-entry)
        (void))
      
      (define (recover-file parent child table-entry)
        (void)))))
