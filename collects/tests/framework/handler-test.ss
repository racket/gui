(let* ([filename "framework-group-test.ss"]
       [tmp-filename (build-path (find-system-path 'temp-dir) filename)])

  (test
   'file-opened
   (lambda (x) (equal? (list filename "MrEd REPL") x))
   (lambda ()
     (send-sexp-to-mred
      `(begin (handler:edit-file ,tmp-filename)
	      (void)))
     (wait-for-frame filename)
     (send-sexp-to-mred
      `(map (lambda (x) (send x get-label)) (get-top-level-windows)))))

  (test
   'file-opened
   (lambda (x) (equal? filename x))
   (lambda ()
     (send-sexp-to-mred
      `(begin (handler:edit-file ,tmp-filename)
	      (void)))
     (wait-for-frame filename)
     (send-sexp-to-mred
      `(let ([f (car (get-top-level-windows))])
	 (send (send f get-editor) get-filename)))))

  (test
   'files-opened-twice
   (lambda (x) (equal? (list filename "MrEd REPL") x))
   (lambda ()
     (send-sexp-to-mred
      `(begin (handler:edit-file ,tmp-filename)
	      (void)))
     (wait-for-frame filename)
     (send-sexp-to-mred
      `(begin (handler:edit-file ,tmp-filename)
	      (void)))
     (wait-for-frame filename)
     (send-sexp-to-mred
      `(map (lambda (x) (send x get-label)) (get-top-level-windows))))))