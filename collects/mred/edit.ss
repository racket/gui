
;; Simple editor implementation; provides new-text-frame
;; and new-pasteboard-frame

(module edit mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (provide new-text-frame
	   new-pasteboard-frame)

  (define (new-text-frame file) (new-frame text% file))
  (define (new-pasteboard-frame file) (new-frame pasteboard% file))

  (define (new-frame editor% file)
    (define f (make-object frame% "MrEdIt" #f 620 450))
    (define c (make-object editor-canvas% f))
    (define e (make-object editor%))
    (define mb (make-object menu-bar% f))

    (define file-menu (make-object menu% "File" mb))
    (define edit-menu (make-object menu% "Edit" mb))
    (define font-menu (make-object menu% "Font" mb))

    (make-object menu-item% "New Text Frame" file-menu
		 (lambda (item event) 
		   (new-text-frame #f))
		 #\N)
    (make-object menu-item% "New Pasteboard Frame" file-menu
		 (lambda (item event) 
		   (new-pasteboard-frame #f)))

    (make-object menu-item% "Open..." file-menu
		 (lambda (item event) 
		   (send e load-file ""))
		 #\O)
    (make-object menu-item% "Save As..." file-menu
		 (lambda (item event) 
		   (send e save-file ""))
		 #\S)
    (when (eq? editor% text%)
      (make-object menu-item% "Save As Text..." file-menu
		   (lambda (item event) 
		     (send e save-file "" 'text))))
    (make-object separator-menu-item% file-menu)
    (make-object menu-item% "Print..." file-menu
		 (lambda (item event)
		   (send e print))
		 #\P)
    (make-object separator-menu-item% file-menu)
    (make-object menu-item% "Close" file-menu
		 (lambda (item event)
		   (send f show #f))
		 #\Q)

    (append-editor-operation-menu-items edit-menu #f)
    (when (eq? editor% text%)
      (make-object separator-menu-item% edit-menu)
      (make-object checkable-menu-item% "Wrap Lines" edit-menu
		   (lambda (item event)
		     (send e auto-wrap (send item is-checked?)))))

    (append-editor-font-menu-items font-menu)
    (let ([m (make-object menu% "Smoothing" font-menu)])
      (let ([mk (lambda (name v)
		  (make-object menu-item% name m
			       (lambda (i e)
				 (let* ([o (send f get-edit-target-object)])
				   (and o
					(o . is-a? . editor<%>)
					(send o change-style 
					      (make-object style-delta% 'change-smoothing v)))))))])
	(mk "Default" 'default)
	(mk "Partly Smoothed" 'partly-smoothed)
	(mk "Smoothed" 'smoothed)
	(mk "Not Smoothed" 'unsmoothed)))

    ((current-text-keymap-initializer) (send e get-keymap))
    (send c set-editor e)

    (when file
      (if (regexp-match "[.]((gif)|(bmp)|(jpg)|(xbm)|(xpm))$" file)
	  (send e insert (make-object image-snip% file))
	  (send e load-file file)))
    
    (send f show #t)
    f))
