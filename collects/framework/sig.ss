
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
   editor<%>
   searchable<%>
   pasteboard<%>
   info<%>
   info-file<%>

   make-empty%
   make-standard-menus%
   make-editor%
   make-searchable%
   make-info%
   make-file%

   empty%
   standard-menus%
   editor%
   searchable%
   info%
   info-file%
   pasteboard%
   pasteboard-info%
   pasteboard-info-file%))

(define-signature framework:version^
  (add-spec
   version))

(define-signature framework:panel^
  (make-edit%
   edit<%>
   horizontal-edit%
   vertical-edit%))

(define-signature framework:exn^
  ((struct exn ())
   (struct unknown-preference ())
   (struct during-preferences ())))

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
   get-choice
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

(define-signature framework:editor^
  (editor:basic<%>
   editor:info<%>
   editor:autosave<%>
   
   editor:make-basic%
   editor:make-info%
   editor:make-file%
   editor:make-backup-autosave%))

(define-signature framework:pasteboard^
  (basic%
   file%
   clever-file-format%
   backup-autosave%
   info%))

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

(define-signature framework:group^
  (frame-group%
   the-frame-group))

(define-signature framework:handler^
  (handler? handler-name handler-extension handler-handler
   format-handlers
   insert-format-handler
   find-format-handler 
   find-named-format-handler 
   edit-file
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

(define-signature framework:keymap^
  (shifted-key-list

   keyerr
   set-keymap-error-handler
   set-keymap-implied-shifts
   make-meta-prefix-list
   send-map-function-meta

   setup-global
   setup-search
   setup-file

   global
   search
   file))

(define-signature framework:match-cache^
  (%))



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

(define-signature framework^
  ([unit application : framework:application^]
   [unit version : framework:version^]
   [unit exn : framework:exn^]
   [unit exit : framework:exit^]
   [unit preferences : framework:preferences^]
   [unit autosave : framework:autosave^]
   [unit handler : framework:handler^] 
   [unit keymap : framework:keymap^]
   [unit match-cache : framework:match-cache^]
   [unit paren : framework:paren^]
   [unit scheme-paren : framework:scheme-paren^]
   [unit path-utils : framework:path-utils^]
   [unit icon : framework:icon^]

   [unit editor : framework:editor^]
   [unit pasteboard : framework:pasteboard^]
   [unit text : framework:text^]

   [unit gui-utils : framework:gui-utils^]

   [unit finder : framework:finder^]

   [unit group : framework:group^]

   [unit canvas : framework:canvas^]

   [unit panel : framework:panel^]

   [unit frame : framework:frame^]
   [unit scheme-mode : framework:scheme-mode^]))