#lang scribble/doc
@(require "common.rkt" (for-label mrlib/arrow-toggle-snip))

@title{Arrow Toggle Snip}

@defmodule[mrlib/arrow-toggle-snip]

@defclass[arrow-toggle-snip% snip% ()]{

Represents a toggle control, displayed as a right-facing arrow (off or
``closed'') or a downward-facing arrow (on or ``open'').

The size of the arrow is determined by the style (and font) applied to
the snip. The arrow is drawn inscribed in a square resting on the
baseline, but the snip reports its size (usually) as the same as a
capital @litchar{X}; this means that the snip should look good next to
text (in the same style) no matter whether base-aligned or
top-aligned.

@defconstructor[([callback (-> boolean? any) void])]{

The @racket[on-up] and @racket[on-down] callbacks are called when the
snip is toggled.
}

@defmethod[(get-toggle-state) boolean?]{

Get the control's state.
}

@defmethod[(set-toggle-state [v boolean?]) void?]{

Sets the control's state. If the new state is different from the
previous state, the appropriate callback is called.
}

}
