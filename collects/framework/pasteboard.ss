(unit/sig framework:pasteboard^
  (import mred^
	  [editor : framework:editor^])

  (define basic% (editor:make-basic% pasteboard%))
  (define file% (editor:make-file% basic%))
  (define clever-file-format% (editor:make-clever-file-format% file%))
  (define backup-autosave% (editor:make-backup-autosave% clever-file-format%))
  (define info% (editor:make-info% searching%)))