#!/bin/sh

string=? ; exec mred -mgaqvf $0

(require-library "pretty.ss")

(load-relative "standard-menus-items.ss")

(define build-id
  (lambda (name post)
    (let* ([name-string (symbol->string name)]
	   [answer (string->symbol (string-append name-string post))])
      answer)))

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
  (let ([name (an-item->name item)])
    (list name (build-id name "-string") (build-id name "-help-string"))))

(define build-fill-in-item-clause
  (lambda (item)
    (let ([help-string (an-item-help-string item)]
	  [proc (an-item-proc item)])
      `(public 
	 ,@(map (lambda (x y) `[,x ,y])
		(an-item->names item)
		(list proc `(lambda () "") `(lambda () ,help-string)))))))

(define build-fill-in-between/after-clause
  (lambda (->name -procedure)
    (lambda (obj)
      `(public
	 [,(->name obj)
	  ,(case (-procedure obj)
	     [(nothing) '(lambda (menu) (void))]
	     [(separator) '(lambda (menu) (make-object separator-menu-item% menu))])]))))

(define build-fill-in-between-clause (build-fill-in-between/after-clause between->name between-procedure))
(define build-fill-in-after-clause (build-fill-in-between/after-clause after->name after-procedure))

(define build-item-menu-clause
  (lambda (item)
    (let* ([name (an-item->name item)]
	   [name-string (symbol->string name)]
	   [menu-before-string (an-item-menu-string-before item)]
	   [menu-after-string (an-item-menu-string-after item)]
	   [key (an-item-key item)]
	   [join '(lambda (base special suffix)
		    (if (string=? special "")
			(string-append base suffix)
			(string-append base " " special suffix)))])
      `(public
	 [,(build-id name "-menu")
	  (and ,name
	       (make-object
		   (get-menu-item%)
		 (,join ,menu-before-string
			(,(build-id name "-string"))
			,menu-after-string)
		 ,(menu-name->id name-string)
		 ,name
		 ,key
		 (,(build-id name "-help-string"))))]))))

(define build-between/after-menu-clause
  (lambda (->name -menu)
    (lambda (between/after)
      `(sequence
	 (,(->name between/after)
	  ,(menu-name->get-menu (-menu between/after)))))))

(define build-between-menu-clause (build-between/after-menu-clause between->name between-menu))
(define build-after-menu-clause (build-between/after-menu-clause after->name after-menu))

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
  `(public [,(generic-name generic)
	    ,(generic-initializer generic)]))

(call-with-output-file "standard-menus.ss"
  (lambda (port)
    (pretty-print
     `(define standard-menus<%>
	(interface (basic<%>)
	  ,@(apply append (map
			   (lambda (x)
			     (cond
			       [(an-item? x) (an-item->names x)]
			       [(between? x) (list (between->name x))]
			       [(after? x) (list (after->name x))]
			       [(generic? x) (list (generic-name x))])) 
			   items))))
     port)

    (newline port)

    (pretty-print
     `(define standard-menus-mixin
	(mixin (basic<%>) (standard-menus<%>) args
	  (inherit get-menu-bar on-close show)
	  (sequence (apply super-init args))
	  ,@(append 
	     (map (lambda (x)
		    (cond
		      [(between? x) (build-fill-in-between-clause x)]
		      [(after? x) (build-fill-in-after-clause x)]
		      [(an-item? x) (build-fill-in-item-clause x)]
		      [(generic? x) (build-fill-in-generic-clause x)]
		      [else (printf "~a~n" x)]))
		  items)
	     (map (lambda (x)
		    (cond
		      [(between? x) (build-between-menu-clause x)]
		      [(an-item? x) (build-item-menu-clause x)]
		      [(after? x) (build-after-menu-clause x)]
		      [(generic? x) (build-generic-clause x)]))
		  items))))
     port))
  'truncate)
