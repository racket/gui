(dunit/sig framework:pasteboard^
  (import mred-interfaces^
	  [editor : framework:editor^])

  (define basic% (editor:basic-mixin pasteboard%))
  (define file% (editor:file-mixin basic%))
  (define backup-autosave% (editor:backup-autosave-mixin file%))
  (define info% (editor:info-mixin backup-autosave%)))