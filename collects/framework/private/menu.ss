(module menu mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   "sig"
	   "../macro.ss"
	   (lib "mred-sig.ss" "mred"))
  
  (provide menu@)
  
  (define menu@
    (unit/sig framework:menu^
      (import mred^
              [preferences : framework:preferences^])
      
      (define can-restore<%>
        (interface (selectable-menu-item<%>)
          restore-keybinding))
      
      (define can-restore-mixin
        (mixin (selectable-menu-item<%>) (can-restore<%>) args
          (inherit set-shortcut get-shortcut)
          (private-field
           [saved-shortcut 'not-yet])
          (public
            [restore-keybinding
             (lambda ()
               (unless (eq? saved-shortcut 'not-yet)
                 (set-shortcut saved-shortcut)))])
          (sequence
            (apply super-init args)
            (set! saved-shortcut (get-shortcut))
            (unless (preferences:get 'framework:menu-bindings)
              (set-shortcut #f)))))
      
      (define can-restore-menu-item% (can-restore-mixin menu-item%))
      (define can-restore-checkable-menu-item% (can-restore-mixin checkable-menu-item%)))))