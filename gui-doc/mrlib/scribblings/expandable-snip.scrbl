#lang scribble/doc
@(require "common.rkt" (for-label mrlib/expandable-snip))

@title{Expandable Snip}

@defmodule[mrlib/expandable-snip]

@defclass[expandable-snip% editor-snip% ()]{

An expandable snip allows the user to toggle between two
views---``open'' and ``closed''---implemented by two text
editors. Typically the closed view is a concise summary and the open
view contains more detailed information. The syntax browser snip is an
example of an expandable snip.

@defconstructor/auto-super[([layout (or/c 'append 'replace) 'append]
                            [closed-editor (is-a?/c text%) (new text%)]
                            [open-editor (is-a?/c text%) (new text%)]
                            [open/close-callback
                             (-> (is-a?/c expandable-snip%) boolean? any)
                             void])]{

The @racket[open/close-callback] is called when the snip state is
toggled. It is called with the expandable snip object and a boolean
that indicates whether the new state is open.

In closed mode, the toggle arrow and @racket[closed-editor] are
displayed adjacent on a single line. In open mode, the layout is
controlled by the @racket[layout] argument as follows:
@itemlist[

@item{@racket['append] --- The first line is unchanged (that is, it
contains both toggle arrow and @racket[closed-editor]), and
@racket[open-editor] is displayed on the second line.}

@item{@racket['replace] --- The toggle arrow and the
@racket[open-editor] are displayed on a single line. The
@racket[closed-editor] is not displayed.}

]
}

@defmethod[(get-open-editor) (is-a?/c text%)]{

Get the editor for the open mode.
}

@defmethod[(get-closed-editor) (is-a?/c text%)]{

Gets the editor for the closed mode.
}
}
