#lang scribble/doc
@(require "common.rkt" (for-label racket/gui/dynamic))

@title{Dynamic Loading}

@defmodule[racket/gui/dynamic]{The @racketmodname[racket/gui/dynamic]
library provides functions for dynamically accessing the
@racketmodname[racket/gui/base] library, instead of directly requiring
@racketmodname[racket/gui] or @racketmodname[racket/gui/base].}

@defproc[(gui-available?) boolean?]{

Returns @racket[#t] if dynamic access to the GUI bindings is
available. The bindings are available if
@racketmodname[racket/gui/base] has been loaded, instantiated, and
attached to the namespace in which @racketmodname[racket/gui/dynamic] was
instantiated.}


@defproc[(gui-dynamic-require [sym symbol?]) any]{

Like @racket[dynamic-require], but specifically to access exports of
@racketmodname[racket/gui/base], and only when @racket[(gui-available?)]
returns true.

The @racket[gui-dynamic-require] function is intended primarily for
use under a @racket[(gui-available?)] conditional. It can also be used
as a shorthand for @racket[dynamic-require] with
@racket['racket/gui/base], but only after ensuring that the bindings
are available. One way to make @racketmodname[racket/gui/base]
bindings available, so that @racket[(gui-available?)] returns true, is
through @racket[dynamic-require]:

@racketblock[
(dynamic-require 'racket/gui/base #f)
]

Unlike @racket[require], using @racket[dynamic-require] delays the
instantiation of @racketmodname[racket/gui/base] until the run-time
call of @racket[dynamic-require]. With @racketmodname[racket/gui/base]
so declared, @racket[gui-dynamic-require] can be used to access
bindings:

@racketblock[
(define window (new (gui-dynamic-require 'frame%)
                    [label "Frame"]))
(send window show #t)
]}
