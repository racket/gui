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

(define-signature mred:graph^
  (node-snip%
   make-node-snip%
   graph-pasteboard%
   make-graph-pasteboard%))

(define-signature mred:connections^
  (connections-frame%
   connections-dialog-box%
   connections-media-edit%
   connections-media-pasteboard%
   connections-media-canvas%
   connections-panel%

   make-connections-frame%
   make-connections-media-buffer%
   make-connections-media-canvas%
   make-connections-panel%))

(define-signature mred:version^
  (add-version-spec
   version))

(define-signature mred:html^
  (html-convert))

(define-signature mred:panel^
  (make-edit-panel%
   horizontal-edit-panel%
   vertical-edit-panel%))

(define-signature mred:url^
  ((struct url (scheme host port path params query fragment))
   unixpath->path
   get-pure-port			; url [x list (str)] -> in-port
   get-impure-port			; url [x list (str)] -> in-port
   display-pure-port			; in-port -> ()
   purify-port				; in-port -> list (mime-header)
   netscape/string->url			; (string -> url)
   string->url				; str -> url
   url->string
   call/input-url			; url x (url -> in-port) x
					; (in-port -> ())
					; [x list (str)] -> ()
   combine-url/relative))		; url x str -> url

(define-signature framework:exn^
  ((struct exn ())
   (struct exn:unknown-preference ())
   (struct exn:during-preferences ())
   (struct exn:url ())))

(define-signature mred:hyper-loader^
  (open-hyper-make
   open-hyper-view
   hyper-text-require))

(define-signature framework:application^
  (current-app-name))

(define-signature mred:exn-external^
  (exn? exn:unknown-preference? exn:during-preferences? exn:url?))

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

(define-signature mred:exit^
  (insert-callback
   remove-callback
   run-callbacks
   exit))

(define-signature mred:gui-utils^
  (get-font-from-user
   get-colour-from-user
   get-text-from-user
   message-box
   cursor-delay
   show-busy-cursor
   delay-action
   local-busy-cursor
   get-choice
   unsaved-warning
   read-snips/chars-from-buffer
   open-input-buffer
   print-paper-names
   get-single-choice))

(define-signature mred:console^
  (credits-proc
   credits
   copyright-string
   welcome-message

   separator-snip%

   console-max-save-previous-exprs
   
   show-interactions-history

   make-scheme-mode-edit%
   scheme-mode-edit%
   
   make-console-edit%
   console-edit%

   transparent-io-edit%
   make-transparent-io-edit%

   make-console-frame%
   console-frame%))

(define-signature mred:path-utils^
  (generate-autosave-name 
   generate-backup-name))

(define-signature mred:finder^
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

(define-signature mred:find-string^
  (make-find-frame%
   find-frame%
   find-string))

(define-signature mred:edit^
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

(define-signature mred:canvas^
  (make-wrapping-canvas%
   wrapping-canvas%

   make-one-line-canvas%
   one-line-canvas%

   make-frame-title-canvas%
   frame-title-canvas%
   
   make-wide-snip-canvas%
   wide-snip-canvas%

   number-control%))

(define-signature mred:frame^
  (frame-width
   frame-height

   make-simple-frame%
   make-menu-frame%
   make-standard-menus-frame%
   make-searchable-frame%

   make-info-frame%
   make-edit-info-frame%

   make-file-frame%

   make-pasteboard-frame%
   make-pasteboard-file-frame%
   make-pasteboard-info-frame%

   empty-frame%
   menu-frame%
   standard-menus-frame%
   simple-menu-frame%
   searchable-frame%
   info-frame%
   info-file-frame%
   pasteboard-frame%
   pasteboard-info-frame%
   pasteboard-info-file-frame%))

(define-signature mred:editor-frame^
  (make-editor-frame%
   editor-frame%
   make-status-frame%))

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

(define-signature mred:scheme-mode^
  (scheme-mode-allow-console-eval
   scheme-mode-tabify-on-return?
   scheme-mode-match-round-to-square?
   scheme-media-wordbreak-map
   scheme-init-wordbreak-map
   setup-global-scheme-mode-keymap
   setup-global-scheme-interaction-mode-keymap
   global-scheme-mode-keymap
   global-scheme-interaction-mode-keymap
   make-scheme-mode%
   make-scheme-interaction-mode%
   scheme-mode%
   scheme-interaction-mode%
   scheme-mode-style-list))

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

