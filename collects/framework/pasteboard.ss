(unit/sig framework:pasteboard^
  (import mred-interfaces^
	  [editor : framework:editor^])

  (define basic% (editor:basic-mixin pasteboard%))
  (define keymap% (editor:keymap-mixin basic%))
  (define file% (editor:file-mixin keymap%))
  (define backup-autosave% (editor:backup-autosave-mixin file%))
  (define info% (editor:info-mixin backup-autosave%)))