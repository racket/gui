;; language specification
(compile-allow-cond-fallthrough #t)

(require-library "refer.ss")
(require-library "macro.ss")
(require-library "cores.ss")
(require-library "dates.ss")
(require-library "match.ss")
(require-library "dates.ss")
(require-library "functios.ss")
(require-library "macro.ss")
(require-relative-library "macro.ss")

(require-relative-library "mred-interfacess.ss")
(require-relative-library "tests.ss")

(define-signature framework:version^
  (add-spec
   version))

(define-signature framework:panel^
  (single-mixin
   single<%>
   single-window<%>
   single-window-mixin
   single%
   single-pane%
   
   editor-mixin
   editor<%>
   horizontal-editor%
   vertical-editor%))

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
  (insert-on-callback
   insert-can?-callback
   can-exit?
   on-exit
   exit))

(define-signature framework:gui-utils^
  (next-untitled-name
   cursor-delay
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
  (dialog-parent-parameter
   default-extension
   common-put-file 
   common-get-file 
   std-put-file 
   std-get-file 
   common-get-file-list
   current-find-file-directory
   get-file
   put-file))

(define-signature framework:editor^
  (basic<%>
   keymap<%>
   info<%>
   file<%>
   backup-autosave<%>
   
   basic-mixin
   keymap-mixin
   info-mixin
   file-mixin
   backup-autosave-mixin))

(define-signature framework:pasteboard^
  (basic%
   keymap%
   file%
   backup-autosave%
   info%))

(define-signature framework:text^
  (basic<%>
   searching<%>
   return<%>
   info<%>
   clever-file-format<%>
   
   basic-mixin
   searching-mixin
   return-mixin
   info-mixin
   clever-file-format-mixin
   
   basic% 
   keymap%
   return%
   file%
   clever-file-format%
   backup-autosave%
   searching%
   info%))

(define-signature framework:pasteboard%
  (pasteboard:basic%
   pasteboard:info%
   pasteboard:file%
   pasteboard:backup-autosave%))


(define-signature framework:canvas^
  (basic-mixin
   basic<%>
   basic%
   wide-snip-mixin
   wide-snip<%>
   wide-snip%))

(define-signature framework:frame^
  (reorder-menus

   basic<%>
   basic-mixin

   standard-menus<%>
   standard-menus-mixin

   editor<%>
   editor-mixin

   text<%>
   text-mixin

   pasteboard<%>
   pasteboard-mixin

   searchable<%>
   searchable-mixin

   info<%>
   info-mixin

   text-info<%>
   text-info-mixin

   pasteboard-info<%>
   pasteboard-info-mixin

   file<%>
   file-mixin

   basic%
   standard-menus%
   editor%

   text%
   searchable%
   text-info%
   text-info-file%
   pasteboard%
   pasteboard-info%
   pasteboard-info-file%))

(define-signature framework:group^
  (%
   get-the-frame-group))

(define-signature framework:handler^
  (handler? handler-name handler-extension handler-handler
   insert-format-handler
   find-format-handler 
   find-named-format-handler 
   edit-file
   open-file))

(define-signature framework:icon^
  (get

   get-paren-highlight-bitmap
   get-autowrap-bitmap

   get-lock-bitmap
   get-unlock-bitmap
   get-anchor-bitmap
   
   get-gc-on-bitmap
   get-gc-off-bitmap))

(define-signature framework:keymap^
  (send-map-function-meta
   make-meta-prefix-list

   setup-global
   setup-search
   setup-file

   get-global
   get-search
   get-file))

(define-signature framework:match-cache^
  (%))

(define-signature framework:scheme-paren^
  (get-comments
   get-paren-pairs
   get-quote-pairs
   forward-match
   backward-match
   balanced?
   backward-containing-sexp))

(define-signature framework:scheme^
  (get-wordbreak-map
   init-wordbreak-map
   get-style-list
   get-keymap
   setup-keymap
   text-mixin
   text<%>
   text%))

(define-signature framework:paren^
  (balanced? 
   forward-match 
   backward-match
   skip-whitespace))

(define-signature framework:main^ ())

(define-signature frameworkc^
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
   [unit scheme : framework:scheme^]
   [unit main : framework:main^]))

(define-signature framework^
  ([unit test : framework:test^]
   (open frameworkc^)))