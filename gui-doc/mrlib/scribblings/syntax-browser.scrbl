#lang scribble/doc
@(require "common.rkt" (for-label mrlib/image-core))

@title{Syntax Browser}

@defmodule[mrlib/syntax-browser]

@defproc[(render-syntax/snip [stx syntax?]) (is-a?/c snip%)]{
 Constructs a @racket[snip%] object that displays information
 about @racket[stx].
}

@defproc[(render-syntax/window [stx syntax?]) void?]{ Uses
 @racket[render-syntax/snip]'s result, together with a frame
 and editor-canvas to show @racket[stx].
}

@defthing[snip-class (is-a?/c snip-class%)]{
 The snipclass used by the result of @racket[render-syntax/snip].
}