#!/bin/sh

string=? ; exec mred -qr $0

(require-library "pretty.ss")
(require-library "function.ss")

(require-library "errortrace.ss" "errortrace")

(require-library "standard-menus-items.ss" "framework")

(define build-id
  (case-lambda
   [(name post) (build-id name post "")]
   [(item post pre)
    (let* ([name (an-item->name item pre)]
	   [name-string (symbol->string name)]
	   [answer (string->symbol (string-append name-string post))])
      answer)]))

(define menu-name->id
  (lambda (name-string)
    (let ([file-menu? (string=? (substring name-string 0 9) "file-menu")]
	  [edit-menu? (string=? (substring name-string 0 9) "edit-menu")]
	  [windows-menu? (string=? (substring name-string 0 9) "windows-m")]
	  [help-menu? (string=? (substring name-string 0 9) "help-menu")])
      `(,(cond
	   [file-menu? 'get-file-menu]
	   [edit-menu? 'get-edit-menu]
	   [windows-menu? 'get-windows-menu]
	   [help-menu? 'get-help-menu]
	   [else (printf "WARNING: defaulting item to file-menu ~s~n" name-string)
		 'get-file-menu])))))

(define (an-item->names item)
  (list (an-item->name item)
	(build-id item "-item" "get-")
	(build-id item "-string")
	(build-id item "-help-string")
	(build-id item "-on-demand")))

(define build-fill-in-item-clause
  (lambda (item)
    (let ([help-string (an-item-help-string item)]
	  [proc (an-item-proc item)])
      `(public 
	 ,@(map (lambda (x y) `[,x ,y])
		(an-item->names item)
		(list proc
		      `(lambda () ,(build-id item "-item"))
		      `(lambda () "")
		      `(lambda () ,help-string)
		      (an-item-on-demand item)))))))

(define build-fill-in-clause
  (lambda (->name -procedure)
    (lambda (obj)
      `(public
	 [,(->name obj)
	  ,(case (-procedure obj)
	     [(nothing) '(lambda (menu) (void))]
	     [(separator) '(lambda (menu) (make-object separator-menu-item% menu))])]))))

(define build-fill-in-between-clause
  (build-fill-in-clause
   between->name
   between-procedure))
(define build-fill-in-before/after-clause
  (build-fill-in-clause
   before/after->name
   before/after-procedure))

(define (build-item-menu-clause item)
  (let* ([name (an-item->name item)]
	 [name-string (symbol->string name)]
	 [menu-before-string (an-item-menu-string-before item)]
	 [menu-after-string (an-item-menu-string-after item)]
	 [key (an-item-key item)]
	 [join (lambda (base-text suffix-text special-text)
		 `(let ([special ,special-text]
			[base ,base-text]
			[suffix ,suffix-text])
		    (if (string=? special "")
			(string-append base suffix)
			(string-append base " " special suffix))))])
    `(private
       [,(build-id item "-item")
	(and ,name
	     (make-object (class (get-menu-item%) args
			    (rename [super-on-demand on-demand])
			    (override
			     [on-demand
			      (lambda ()
				(,(build-id item "-on-demand") this)
				(super-on-demand))])
			    (sequence (apply super-init args)))
	       ,(join menu-before-string menu-after-string
		      `(,(build-id item "-string")))
	       ,(menu-name->id name-string)
               (let ([,name (lambda (item evt) (,name item evt))])
                 ,name)
	       ,key
	       (,(build-id item "-help-string"))))])))

(define build-menu-clause
  (lambda (->name -menu)
    (lambda (between/after)
      `(sequence
	 (,(->name between/after)
	  ,(menu-name->get-menu (-menu between/after)))))))

(define build-between-menu-clause
  (build-menu-clause between->name between-menu))
(define build-before/after-menu-clause
  (build-menu-clause before/after->name before/after-menu))

(define menu-name->get-menu
  (lambda (menu-name)
    `(,(string->symbol
	(string-append
	 "get-"
	 (symbol->string
	  menu-name))))))

(define build-between-menu-clause
  (lambda (between)
    `(sequence
       (,(between->name between)
	,(menu-name->get-menu (between-menu between))))))

(define (build-generic-clause x) '(sequence (void)))
(define (build-fill-in-generic-clause generic)
  `(public
    [,(generic-name generic)
     ,(generic-initializer generic)]))

(define (build-generic-override-clause x)
  `(rename [,(string->symbol (format "super-~a" (generic-override-name x)))
	    ,(generic-override-name x)]))
(define (build-fill-in-generic-override-clause generic)
  `(override
    [,(generic-override-name generic)
     ,(generic-override-initializer generic)]))


(define standard-menus.ss-filename (build-path (collection-path "framework") "standard-menus.ss"))
(printf "writing to ~a~n" standard-menus.ss-filename)  

(call-with-output-file standard-menus.ss-filename
  (lambda (port)
    (pretty-print
     `(define standard-menus<%>
	(interface (basic<%>)
	  ,@(apply append (map
			   (lambda (x)
			     (cond
			       [(an-item? x) (an-item->names x)]
			       [(between? x) (list (between->name x))]
			       [(or (after? x) (before? x))
				(list (before/after->name x))]
			       [(generic-override? x) null]
			       [(generic? x) (list (generic-name x))])) 
			   items))))
     port)

    (newline port)

    (pretty-print
     `(define standard-menus-mixin
	(mixin (basic<%>) (standard-menus<%>) args
          (inherit on-menu-char on-traverse-char)
	  (private
	    [remove-prefs-callback
	     (preferences:add-callback
	      'framework:menu-bindings
	      (lambda (p v)
		(let ([mb (get-menu-bar)])
		  (let loop ([menu (get-menu-bar)])
		    (cond
                      [(is-a? menu menu-item-container<%>)
                       (for-each loop (send menu get-items))]
                      [(is-a? menu selectable-menu-item<%>)
                       (when (is-a? menu menu:can-restore<%>)
                         (if v
                             (send menu restore-keybinding)
                             (send menu set-shortcut #f)))])))))])
          
	  (inherit get-menu-bar show can-close? get-edit-target-object)
	  (sequence (apply super-init args))
	  ,@(append 
	     (map (lambda (x)
		    (cond
		      [(between? x) (build-fill-in-between-clause x)]
		      [(or (after? x) (before? x))
		       (build-fill-in-before/after-clause x)]
		      [(an-item? x) (build-fill-in-item-clause x)]
		      [(generic? x) (build-fill-in-generic-clause x)]
		      [(generic-override? x) (build-fill-in-generic-override-clause x)]
		      [else (printf "~a~n" x)]))
		  items)
	     (map (lambda (x)
		    (cond
		      [(between? x) (build-between-menu-clause x)]
		      [(an-item? x) (build-item-menu-clause x)]
		      [(or (after? x) (before? x))
		       (build-before/after-menu-clause x)]
		      [(generic? x) (build-generic-clause x)]
		      [(generic-override? x) (build-generic-override-clause x)]))
		  items)
	     (list `(sequence (reorder-menus this))))))
     port))
  'text
  'truncate)
