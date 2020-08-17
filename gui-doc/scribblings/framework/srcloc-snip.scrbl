#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Srcloc Snips}

@defclass[srcloc-snip:snip% editor-snip% ()]{
This snip implements clickable links to @racket[srcloc] locations.

The snip is initialized with an appropriate editor, into which a
representation for the link can be inserted.  When the reprenstation
has been inserted, the @racket[activate-link] method needs to be
called to activate the link.
     
@defconstructor[([srcloc srcloc?])]{
 The @racket[srcloc] field specifies where the link points.
}

@defmethod[#:mode public (activate-link) void?]{
 This makes the content of the snip's editor clickable, such that
 clicking highlights the position of the srcloc.
}
}

@(include-previously-extracted "main-extracts.rkt" #rx"^srcloc-snip:")

