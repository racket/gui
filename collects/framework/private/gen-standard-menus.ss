(module gen-standard-menus mzscheme
  (require (lib "pretty.ss"))
  (require (lib "list.ss"))
  (require (lib "standard-menus-items.ss" "framework" "private"))
  
  ;; build-before-super-item-clause : an-item -> sexp
  ;; calculates a `public' class expression
  (define build-before-super-item-clause
    (lambda (item)
      `(public
         [,(an-item->callback-name item)
          ,(or (an-item-proc item) `(lambda (x y) (void)))]
         [,(an-item->get-item-name item)
          (lambda () ,(an-item->item-name item))]
         [,(an-item->string-name item)
          (lambda () "")]
         [,(an-item->help-string-name item)
          (lambda () ,(an-item-help-string item))]
         [,(an-item->on-demand-name item)
          ,(an-item-on-demand item)]
         [,(an-item->create-menu-item-name item)
          (lambda () ,(not (not (an-item-proc item))))])))
  
  (define build-before-super-clause
    (lambda (->name -procedure)
      (lambda (obj)
        `(public
           [,(->name obj)
            ,(case (-procedure obj)
               [(nothing) '(lambda (menu) (void))]
               [(separator) '(lambda (menu) (make-object separator-menu-item% menu))])]))))
  
  (define build-before-super-between-clause
    (build-before-super-clause
     between->name
     between-procedure))
  (define build-before-super-before/after-clause
    (build-before-super-clause
     before/after->name
     before/after-procedure))
  
  (define (build-after-super-item-clause item)
    (let* ([callback-name (an-item->callback-name item)]
           [create-menu-item-name (an-item->create-menu-item-name item)]
           [callback-name-string (symbol->string callback-name)]
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
      `(private-field
        [,(an-item->item-name item)
         (and (,create-menu-item-name)
              (make-object (class100 (get-menu-item%) args
                             (rename [super-on-demand on-demand])
                             (override
                               [on-demand
                                (lambda ()
                                  (,(an-item->on-demand-name item) this)
                                  (super-on-demand))])
                             (sequence
                               (apply super-init args)))
                ,(join menu-before-string menu-after-string
                       `(,(an-item->string-name item)))
                ,(menu-item-menu-name item)
                (let ([,callback-name (lambda (item evt) (,callback-name item evt))])
                  ,callback-name)
                ,key
                (,(an-item->help-string-name item))))])))
  
  (define build-after-super-clause
    (lambda (->name)
      (lambda (between/after)
        `(sequence
           (,(->name between/after)
            (,(menu-name->get-menu-name between/after)))))))
  
  (define build-after-super-between-clause (build-after-super-clause between->name))
  (define build-after-super-before/after-clause (build-after-super-clause before/after->name))
  
  (define (build-after-super-generic-clause x) 
    (cond
      [(generic-private-field? x)
       `(private-field
         [,(generic-name x)
          ,(generic-initializer x)])]
      [(generic-override? x)
       `(rename [,(string->symbol (format "super-~a" (generic-name x)))
                 ,(generic-name x)])]
      [(generic-method? x)
       `(sequence (void))]))
  (define (build-before-super-generic-clause generic)
    (cond
      [(generic-private-field? generic)
       `(sequence (void))]
      [(generic-override? generic)
       `(override
          [,(generic-name generic)
           ,(generic-initializer generic)])]
      [(generic-method? generic)
       `(public
          [,(generic-name generic)
           ,(generic-initializer generic)])]))
  
  
  (define standard-menus.ss-filename (build-path (collection-path "framework" "private") "standard-menus.ss"))
  (printf "writing to ~a~n" standard-menus.ss-filename)  
  
  (call-with-output-file standard-menus.ss-filename
    (lambda (port)
      (pretty-print
       `(define standard-menus<%>
          (interface (basic<%>)
            ,@(apply append (map
                             (lambda (x)
                               (cond
                                 [(an-item? x) 
                                  (list 
                                   (an-item->callback-name x)
                                   (an-item->get-item-name x)
                                   (an-item->string-name x)
                                   (an-item->help-string-name x)
                                   (an-item->on-demand-name x)
                                   (an-item->create-menu-item-name x))]
                                 [(between? x) (list (between->name x))]
                                 [(or (after? x) (before? x))
                                  (list (before/after->name x))]
                                 [(generic? x) 
                                  (if (generic-method? x)
                                      (list (generic-name x))
                                      null)])) 
                             items))))
       port)
      
      (newline port)
      
      (pretty-print
       `(define standard-menus-mixin
          (mixin (basic<%>) (standard-menus<%>) args
            (inherit on-menu-char on-traverse-char)
            (private-field
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
            ,@(map (lambda (x)
                     (cond
                       [(between? x) (build-before-super-between-clause x)]
                       [(or (after? x) (before? x)) (build-before-super-before/after-clause x)]
                       [(an-item? x) (build-before-super-item-clause x)]
                       [(generic? x) (build-before-super-generic-clause x)]
                       [else (printf "~a~n" x)]))
                   items)
            (sequence (apply super-init args))
            ,@(map (lambda (x)
                     (cond
                       [(between? x) (build-after-super-between-clause x)]
                       [(an-item? x) (build-after-super-item-clause x)]
                       [(or (after? x) (before? x)) (build-after-super-before/after-clause x)]
                       [(generic? x) (build-after-super-generic-clause x)]))
                   items)
            (sequence (reorder-menus this))))
       port))
    'text
    'truncate))
