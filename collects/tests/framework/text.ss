(define (test-creation frame% class name)
  (test
   name
   (lambda (x) #t)
   (lambda ()
     (send-sexp-to-mred
      `(let* ([% (class-asi ,frame%
		   (override
		    [get-editor% (lambda () ,class)]))]
	      [f (make-object % "test text")])
	 
	 (let loop ([f f][l " "])
	   (printf "~a~a ~a~n" l f (send f get-label))
	   (when (is-a? f area-container<%>)
	     (for-each (lambda (c)
			 (loop c (string-append " " l)))
		       (send f get-children))))
	 (send f show #t)
	 (sleep/yield 1)
	 (printf "focus: ~a~n" (get-top-level-focus-window))))
      (wait-for-frame "test text")
      (send-sexp-to-mred `(test:keystroke #\a))
      (wait-for `(string=? "a" (send (send (get-top-level-focus-window) get-editor) get-text)))
      (send-sexp-to-mred
       `(send (get-top-level-focus-window) show #f)))))

(test-creation 'frame:text%
	       '(text:basic-mixin (editor:basic-mixin text%))
	       'text:basic-mixin-creation)
(test-creation 'frame:text%
	       'text:basic%
	       'text:basic-creation)

(define (return-args class)
  `(class ,class ()
     (sequence
       (super-init void))))
(test-creation 'frame:text%
	       (return-args '(text:return-mixin text:basic%))
	       'text:return-mixin-creation)
(test-creation 'frame:text%
	       (return-args 'text:return%)
	       'text:return-creation)

(test-creation 'frame:text%
	       '(editor:file-mixin text:basic%)
	       'editor:file-mixin-creation)
(test-creation 'frame:text%
	       'text:file%
	       'text:file-creation)
(test-creation 'frame:text%
	       '(text:clever-file-format-mixin text:file%)
	       'text:clever-file-format-mixin-creation)
(test-creation 'frame:text%
	       'text:clever-file-format%
	       'text:clever-file-format-creation)
(test-creation 'frame:text%
	       '(editor:backup-autosave-mixin text:clever-file-format%)
	       'editor:backup-autosave-mixin-creation)
(test-creation 'frame:text%
	       'text:backup-autosave%
	       'text:backup-autosave-creation)
(test-creation 'frame:text%
	       '(text:searching-mixin text:backup-autosave%)
	       'text:searching-mixin-creation)
(test-creation 'frame:text%
	       'text:searching%
	       'text:searching-creation)
(test-creation 'frame:text-info%
	       '(text:info-mixin (editor:info-mixin text:searching%))
	       'text:info-mixin-creation)
(test-creation 'frame:text-info%
	       'text:info%
	       'text:info-creation)