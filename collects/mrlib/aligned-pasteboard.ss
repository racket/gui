(module aligned-pasteboard mzscheme
  (require
    "private/aligned-pasteboard/aligned-pasteboard.ss"
    "private/aligned-pasteboard/aligned-editor-container.ss"
    "private/aligned-pasteboard/interface.ss")
  (provide
    vertical-pasteboard%
    horizontal-pasteboard%
    aligned-editor-snip%
    aligned-editor-canvas%
    aligned-pasteboard<%>
    aligned-pasteboard-parent<%>
    stretchable-snip<%>
    ;stretchable-text-snip%
    ;stretchable-text-snip-mixin
    ))
