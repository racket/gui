(module pasteboard mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   "sig.ss"
	   "../macro.ss"
	   (lib "mred-sig.ss" "mred"))

  (provide pasteboard@)

  (define pasteboard@
    (unit/sig framework:pasteboard^
      (import mred^
	      [editor : framework:editor^])

      (rename [-keymap% keymap%])

      (define basic% (editor:basic-mixin pasteboard%))
      (define -keymap% (editor:keymap-mixin basic%))
      (define file% (editor:file-mixin -keymap%))
      (define backup-autosave% (editor:backup-autosave-mixin file%))
      (define info% (editor:info-mixin backup-autosave%)))))
