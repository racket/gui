(define (test-creation frame class name)
  (test
   name
   (lambda (x) #t)
   (lambda ()
     (send-sexp-to-mred
      `(let* ([% (class-asi ,frame
		   (override
		     [get-editor%
		      (lambda ()
			,class)]))]
	      [f (make-object % "test pasteboard")])
	 (send f show #t)))
      (wait-for-frame "test pasteboard")
      (send-sexp-to-mred
       `(send (get-top-level-focus-window) show #f)))))

(test-creation 'frame:editor%
	       '(editor:basic-mixin pasteboard%)
	       'editor:basic-mixin-creation)
(test-creation 'frame:editor%
	       'pasteboard:basic%
	       'pasteboard:basic-creation)

(test-creation 'frame:editor%
	       '(editor:file-mixin pasteboard:basic%)
	       'editor:file-mixin-creation)
(test-creation 'frame:editor%
	       'pasteboard:file%
	       'pasteboard:file-creation)

(test-creation 'frame:editor%
	       '(editor:backup-autosave-mixin pasteboard:file%)
	       'editor:backup-autosave-mixin-creation)
(test-creation 'frame:editor%
	       'pasteboard:backup-autosave%
	       'pasteboard:backup-autosave-creation)

(test-creation 'frame:pasteboard-info%
	       '(editor:info-mixin pasteboard:backup-autosave%)
	       'editor:info-mixin-creation)
(test-creation 'frame:pasteboard-info%
	       'pasteboard:info%
	       'pasteboard:info-creation)
