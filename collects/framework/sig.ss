(require-library "refer.ss")
(require-library "cores.ss")
(require-library "match.ss")
(require-library "dates.ss")
(require-library "functios.ss")
(require-library "macro.ss")

(define-signature framework:frame^
  (empty<%>
   standard-menus<%>
   empty-standard-menus<%>
   edit<%>
   searchable<%>
   pasteboard<%>
   info<%>
   info-file<%>

   make-empty%
   make-standard-menus%
   make-edit%
   make-searchable%
   make-info%
   make-file%

   empty%
   standard-menus%
   edit%
   searchable%
   info%
   info-file%
   pasteboard%
   pasteboard-info%
   pasteboard-info-file%))

(define-signature framework:version^
  (add-spec
   version))

(define-signature mred:panel^
  (make-edit%
   edit<%>
   horizontal-edit%
   vertical-edit%))

(define-signature framework:exn^
  ((struct exn ())
   (struct exn:unknown-preference ())
   (struct exn:during-preferences ())
   (struct exn:url ())))

(define-signature framework:application^
  (current-app-name))

(define-signature framework:preferences^
  (get
   add-callback
   set
   set-default
   set-un/marshall

   save
   read
   restore-defaults

   add-panel
   show-dialog
   hide-dialog))

(define-signature framework:autosave^
  (register))

(define-signature framework:exit^
  (insert-callback
   remove-callback
   run-callbacks
   exit))

(define-signature framework:gui-utils^
  (cursor-delay
   show-busy-cursor
   delay-action
   local-busy-cursor
   unsaved-warning
   read-snips/chars-from-buffer
   open-input-buffer))

(define-signature framework:path-utils^
  (generate-autosave-name 
   generate-backup-name))

(define-signature framework:finder^
  (filter-match?
   dialog-parent-parameter
   common-put-file 
   common-get-file 
   std-put-file 
   std-get-file 
   common-get-file-list
   current-find-file-directory
   get-file
   put-file))

(define framework:editor^
  (editor:basic<%>
   editor:info<%>
   editor:autosave<%>
   
   editor:make-basic%
   editor:make-info%
   editor:make-file%
   editor:make-backup-autosave%))

(define-signature framework:text^
  (text:basic<%>
   text:searching<%>
   
   text:make-basic%
   text:make-return%
   text:make-searching%
   text:make-clever-file-format%
   text:make-scheme%
   
   text:basic% 
   text:return%
   text:searching%
   text:info%
   text:clever-file-format%
   text:file%
   text:backup-autosave%
   text:scheme%))

(define-signature framework:pasteboard%
  (pasteboard:basic%
   pasteboard:info%
   pasteboard:file%
   pasteboard:backup-autosave%))


(define-signature framework:edit^
  (make-std-buffer%
   make-pasteboard%
   make-info-buffer%
   make-info-edit%
   make-file-buffer%
   make-searching-edit%
   make-backup-autosave-buffer%
   make-return-edit%

   media-edit%
   info-edit%
   searching-edit%
   clever-file-format-edit%
   file-edit%
   backup-autosave-edit%
   edit%
   return-edit%

   pasteboard%
   info-pasteboard%
   file-pasteboard%
   backup-autosave-pasteboard% 
   
   make-snip%
   snip%
   media-snip%))

(define-signature framework:canvas^
  (make-wide-snip-canvas%
   wide-snip-canvas%))

(define-signature framework:frame^
  (empty<%>
   standard-menus<%>
   empty-standard-menus<%>
   edit<%>
   searchable<%>
   pasteboard<%>
   info<%>
   info-file<%>

   make-empty%
   make-standard-menus%
   make-edit%
   make-searchable%
   make-pasteboard%
   make-info%
   make-file%
   
   empty%
   standard-menus%
   edit%
   searchable%
   info%
   info-file%
   pasteboard%
   pasteboard-info%
   pasteboard-info-file%))

(define-signature mred:group^
  (frame-group%
   the-frame-group))

(define-signature framework:handler^
  (handler? handler-name handler-extension handler-handler
   format-handlers
   insert-format-handler
   find-format-handler 
   find-named-format-handler 
   edit-file
   open-url
   open-file))

(define-signature framework:icon^
  (get

   get-paren-highlight-bitmap
   get-autowrap-bitmap
   get-reset-console-bitmap

   get-lock-bitmap
   get-lock-mdc
   get-unlock-bitmap
   get-unlock-mdc
   get-anchor-bitmap
   get-anchor-mdc
   
   get-gc-on-dc
   get-gc-off-dc
   get-gc-width
   get-gc-height))

(define-signature mred:keymap^
  (keyerr
   set-keymap-error-handler
   shifted-key-list
   set-keymap-implied-shifts
   make-meta-prefix-list
   send-map-function-meta

   setup-global-keymap
   setup-global-search-keymap
   setup-global-file-keymap

   global-keymap
   global-search-keymap
   global-file-keymap))

(define-signature framework:match-cache^
  (%))

(define-signature mred:menu^
  (max-manual-menu-id
   generate-menu-id
   make-menu%
   menu%
   make-menu-bar%
   menu-bar%))

(define-signature mred:project^
  (project-frame-group%
   make-project-frame%
   project-frame%))

(define-signature framework:scheme-paren^
  (paren-pairs
   quote-pairs
   comments
   forward-match
   backward-match
   balanced?
   backward-containing-sexp))

(define-signature framework:scheme-mode^
  (wordbreak-map
   init-wordbreak-map
   style-list
   keymap
   setup-keymap
   make-text%
   text<%>
   text%))

(define-signature framework:paren^
  (balanced? 
   forward-match 
   backward-match
   skip-whitespace))


(define-signature mred:hyper-edit^
  ((struct hypertag (name position))
   (struct hyperlink (anchor-start anchor-end url-string))
   hyper-buffer-data%
   hyper-data-class
   make-hyper-edit%
   hyper-edit%))

(define-signature mred:hyper-dialog^
  (hyper-tag-dialog%
   hyper-get-current-tags))

(define-signature mred:hyper-frame^
  (hyper-frame-group
   make-hyper-canvas%
   hyper-canvas%
   make-hyper-basic-frame%
   hyper-basic-frame%
   make-hyper-view-frame%
   hyper-view-frame%
   make-hyper-make-frame%
   hyper-make-frame%
   open-hyper-view
   open-hyper-make
   hyper-text-require))

(define-signature mred^
  ((unit constants : mred:constants^)
   (open mred:version^)
   (open mred:exn-external^)
   (open mred:connections^) (open mred:container^) (open mred:preferences^)
   (open mred:autoload^) (open mred:autosave^) (open mred:exit^)
   (open mred:gui-utils^) (open mred:console^) (open mred:path-utils^)
   (open mred:finder^)
   (open mred:find-string^) (open mred:edit^) (open mred:canvas^)
   (open mred:frame^) (open mred:editor-frame^)
   (open mred:group^) (open mred:handler^) (open mred:icon^) (open mred:keymap^)
   (open mred:match-cache^) (open mred:menu^) (open mred:mode^) 
   (open mred:panel^) (open mred:paren^) (open mred:project^)
   (open mred:scheme-paren^) (open mred:scheme-mode^) 
   (open mred:hyper-edit^) (open mred:hyper-dialog^) (open mred:hyper-frame^)
   (open mred:testable-window^)
   (unit test : mred:self-test-export^)
   (open mred:url^)
   (open mred:graph^)
   (open mred:application^)
   (open mred:control^)))

