(module standard-menus-items mzscheme
  (provide
   (struct generic (name initializer documentation))
   (struct generic-override (name initializer documentation))
   
   (struct before/after (menu name procedure))
   (struct before ())
   (struct after ())
   
   (struct between (menu before after procedure))
   before/after->name
   between->name
   
   an-item->name
   items
   edit-menu:do
   edit-menu:do-on-demand
   edit-menu:edit-target-on-demand
   edit-menu:edit-target-on-demand)
  
  (define-struct generic (name initializer documentation))
  (define-struct generic-override (name initializer documentation))
  
  (define-struct before/after (menu name procedure))
  (define-struct (before struct:before/after) ())
  (define-struct (after struct:before/after) ())
  (define (before/after->name before/after)
    (string->symbol (format "~a:~a-~a"
                            (before/after-menu before/after)
                            (if (before? before/after)
                                "before"
                                "after")
                            (before/after-name before/after))))
  
  (define-struct between (menu before after procedure))
  (define (between->name between)
    (string->symbol (format "~a:between-~a-and-~a"
                            (between-menu between)
                            (between-before between)
                            (between-after between))))
  
  (define-struct an-item (menu-name
                          item-name
                          help-string
                          proc
                          key
                          menu-string-before
                          menu-string-after
                          on-demand))
  (define an-item->name
    (case-lambda
     [(item) (an-item->name item "")]
     [(item middle)
      (string->symbol (format "~a:~a~a"
                              (an-item-menu-name item)
                              middle
                              (an-item-item-name item)))]))
  
  (define (edit-menu:do const)
    `(lambda (menu evt)
       (let ([edit (get-edit-target-object)])
         (when (and edit
                    (is-a? edit editor<%>))
           (send edit do-edit-operation ',const)))
       #t))
  
  (define (edit-menu:can-do-on-demand const)
    `(lambda (item)
       (let* ([editor (get-edit-target-object)]
              [enable?
               (and editor
                    (is-a? editor editor<%>)
                    (send editor can-do-edit-operation? ',const))])
         (send item enable enable?))))
  
  (define edit-menu:edit-target-on-demand
    `(lambda (item)
       (send item enable
             (let ([target (get-edit-target-object)])
               (and target (is-a? target editor<%>))))))
  
  (define items
    (list (make-generic-override
           'on-close '(lambda ()
                        (remove-prefs-callback)
                        (super-on-close))
           '("@return : void"
             "Removes the preferences callbacks for the menu items"))
          (make-generic 'get-menu% '(lambda () menu%)
                        '("The result of this method is used as the class"
                          "for creating the result of these methods:"
                          "@ilink frame:standard-menus get-file-menu %"
                          ", "
                          "@ilink frame:standard-menus get-edit-menu %"
                          ", "
                          "@ilink frame:standard-menus get-help-menu %"
                          ". "
                          ""
                          "@return : (derived-from \\iscmclass{menu})"
                          ""
                          "defaultly returns"
                          "@link menu"))
          (make-generic 'get-menu-item% '(lambda () menu:can-restore-menu-item%)
                        '("The result of this method is used as the class for creating"
                          "the menu items in this class (see "
                          "@link frame:standard-menus"
                          "for a list)."
                          ""
                          "@return : (derived-from \\iscmclass{menu-item})"
                          ""
                          "defaultly returns"
                          "@link menu:can-restore-menu-item %"
                          "."))
          (make-generic 'get-checkable-menu-item% '(lambda () menu:can-restore-checkable-menu-item%)
                        '("The result of this method is used as the class for creating"
                          "checkable menu items in this class (see "
                          "@link frame:standard-menus"
                          "for a list)."
                          ""
                          "@return : (derived-from \\iscmclass{checkable-menu-item})"
                          ""
                          "defaultly returns"
                          "@link menu:can-restore-checkable-menu-item %"
                          "."))
          
          (make-generic 'get-file-menu
                        '(let ([m (make-object (get-menu%)
                                    (if (eq? (system-type) 'windows)
                                        "&File" "F&ile")
                                    (get-menu-bar))])
                           (lambda () m))
                        '("Returns the file menu"
                          "See also"
                          "@ilink frame:standard-menus get-menu\\%"
                          ""
                          "@return : (instance (derived-from \\iscmclass{menu}))"))
          (make-generic 'get-edit-menu
                        '(let ([m (make-object (get-menu%) "&Edit" (get-menu-bar))])
                           (lambda () m))
                        
                        '("Returns the edit menu"
                          "See also"
                          "@ilink frame:standard-menus get-menu\\%"
                          ""
                          "@return : (instance (derived-from \\iscmclass{menu}))"))
          (make-generic 'get-help-menu
                        '(let ([m (make-object (get-menu%) "&Help" (get-menu-bar))])
                           (lambda () m))
                        
                        '("Returns the help menu"
                          "See also"
                          "@ilink frame:standard-menus get-menu\\%"
                          ""
                          "@return : (instance (derived-from \\iscmclass{menu}))"))
          
          (make-an-item 'file-menu 'new "Open a new file"
                        '(lambda (item control) (handler:edit-file #f) #t)
                        #\n "&New" ""
                        'void)
          (make-between 'file-menu 'new 'open 'nothing)
          (make-an-item 'file-menu 'open "Open a file from disk"
                        '(lambda (item control) (handler:open-file) #t)
                        #\o "&Open" "..."
                        'void)
          (make-between 'file-menu 'open 'revert 'nothing)
          (make-an-item 'file-menu 'revert 
                        "Revert this file to the copy on disk"
                        #f #f "&Revert" ""
                        'void)
          (make-between 'file-menu 'revert 'save 'nothing)
          (make-an-item 'file-menu 'save
                        "Save this file to disk"
                        #f #\s "&Save" ""
                        'void)
          (make-an-item 'file-menu 'save-as
                        "Prompt for a filename and save this file to disk"
                        #f #f "Save" " &As..."
                        'void)
          (make-between 'file-menu 'save-as 'print 'separator)
          (make-an-item 'file-menu 'print
                        "Print this file"
                        #f #\p "&Print" "..."
                        'void)
          (make-between 'file-menu 'print 'close 'separator)
          (make-an-item 'file-menu 'close
                        "Close this file"
                        '(lambda (item control) (when (can-close?) (on-close) (show #f)) #t)
                        #\w "&Close" ""
                        'void)
          (make-between 'file-menu 'close 'quit 'nothing)
          (make-an-item 'file-menu 'quit
                        "Quit"
                        '(lambda (item control) (parameterize ([exit:frame-exiting this]) (exit:exit)))
                        #\q
                        '(if (eq? (system-type) 'windows) "E&xit" "Quit")
                        ""
                        'void)
          (make-after 'file-menu 'quit 'nothing)
          
          (make-an-item 'edit-menu 'undo "Undo the most recent action" 
                        (edit-menu:do  'undo)
                        #\z "&Undo" ""
                        (edit-menu:can-do-on-demand 'undo))
          (make-an-item 'edit-menu 'redo "Redo the most recent undo" 
                        (edit-menu:do 'redo)
                        #\y "&Redo" ""
                        (edit-menu:can-do-on-demand 'redo))
          (make-between 'edit-menu 'redo 'cut 'separator)
          (make-an-item 'edit-menu 'cut "Cut the selection" 
                        (edit-menu:do 'cut)
                        #\x "Cu&t" ""
                        (edit-menu:can-do-on-demand 'cut))
          (make-between 'edit-menu 'cut 'copy 'nothing)
          (make-an-item 'edit-menu 'copy "Copy the selection"
                        (edit-menu:do 'copy)
                        #\c "&Copy" ""
                        (edit-menu:can-do-on-demand 'copy))
          (make-between 'edit-menu 'copy 'paste 'nothing)
          (make-an-item 'edit-menu 'paste "Paste the most recent copy or cut over the selection"
                        (edit-menu:do 'paste)
                        #\v "&Paste" ""
                        (edit-menu:can-do-on-demand 'paste))
          (make-between 'edit-menu 'paste 'clear 'nothing)
          (make-an-item 'edit-menu 'clear "Clear the selection without affecting paste" 
                        (edit-menu:do 'clear)
                        #f
                        '(if (eq? (system-type) 'macos)
                             "Clear"
                             "&Delete")
                        ""
                        (edit-menu:can-do-on-demand 'clear))
          (make-between 'edit-menu 'clear 'select-all 'nothing)
          (make-an-item 'edit-menu 'select-all "Select the entire document"
                        (edit-menu:do 'select-all)
                        #\a "Select A&ll" ""
                        (edit-menu:can-do-on-demand 'select-all))
          (make-between 'edit-menu 'select-all 'find 'separator)
          (make-an-item 'edit-menu 'find "Search for a string in the window" #f
                        #\f "Find" "..."
                        edit-menu:edit-target-on-demand)
          (make-an-item 'edit-menu 'find-again "Search for the same string as before" #f
                        #\g "Find Again" ""
                        edit-menu:edit-target-on-demand)
          (make-an-item 'edit-menu 'replace-and-find-again 
                        "Replace the current text and search for the same string as before"
                        #f #\h "Replace && Find Again" ""
                        edit-menu:edit-target-on-demand)
          (make-between 'edit-menu 'find 'preferences 'separator)
          (make-an-item 'edit-menu 'preferences "Configure the preferences"
                        '(lambda (item control) (preferences:show-dialog) #t)
                        #f "Preferences..." ""
                        'void)
          (make-after 'edit-menu 'preferences 'nothing)
          
          (make-before 'help-menu 'about 'nothing)
          (make-an-item 'help-menu 'about "Learn something about this application"
                        #f
                        #f
                        "About "
                        "..."
                        'void)
          (make-after 'help-menu 'about 'nothing))))