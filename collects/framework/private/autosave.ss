
(module autosave mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred"))

  (provide autosave@)

  (define autosave@
    (unit/sig framework:autosave^
      (import mred^
	      [exit : framework:exit^]
	      [preferences : framework:preferences^])
      
      (define autosavable<%>
	(interface ()
	  do-autosave))

      (define objects null)
      
      (define autosave-toc
        (build-path (find-system-path 'pref-dir)
                    (case (system-type)
                      [(windows unix) ".plt-autosave-toc"]
                      [else "PLT-autosave-toc"])))
      
      (define autosave-toc-save
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
                (when (file-exists? autosave-toc-save)
                  (delete-file autosave-toc-save))
                (when (file-exists? autosave-toc)
                  (copy-file autosave-toc autosave-toc-save))
                (call-with-output-file autosave-toc
                  (lambda (port)
                    (write new-name-mapping port))
                  'truncate
                  'text)))
            (let ([seconds (preferences:get 'framework:autosave-delay)])
              (start (* 1000 seconds) #t)))
          (super-instantiate ())
          (let ([seconds (preferences:get 'framework:autosave-delay)])
            (start (* 1000 seconds) #t))))

      (define (restore-autosave-files/gui)
        ...)
      
      (define (rebuild-object-list)
        (let loop ([orig-objects objects]
                   [name-mapping null]
                   [new-objects null])
          (if (null? orig-objects)
              (values new-objects name-mapping)
              (let* ([object-wb (car orig-objects)]
                     [object (weak-box-value object-wb)])
                (if object
                    (let ([new-filename (send object do-autosave)])
                      (loop (cdr orig-objects)
                            (cons (list (send object get-filename)
                                        new-filename))
                            (cons object-wb new-objects)))
                    (loop (cdr orig-objects)
                          name-mapping
                          new-objects))))))
      
      (define timer #f)

      (define (register b)
        (unless (is-a? b editor<%>)
          (error 'autosave:register "expected object implemeting editor<%>, got: ~e" b))
        (unless timer
          (set! timer (make-object autosave-timer%)))
        (set! objects
              (let loop ([objects objects])
                (cond
                  [(null? objects) (list (make-weak-box b))]
                  [else (let ([weak-box (car objects)])
                          (if (weak-box-value weak-box)
                              (cons weak-box (loop (cdr objects)))
                              (loop (cdr objects))))])))))))


