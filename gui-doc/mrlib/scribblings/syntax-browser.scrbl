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

@defthing[render-syntax-focused-syntax-color-style-name string?]{
 The name of the style that controls the color of the focused
 portion of the syntax object that's being displayed.
 See also @racket[render-syntax-subtitle-color-style-name].

 Defaults to a style that's derived from the basic style with
 the foreground color @racket["forestgreen"].
}

@defthing[render-syntax-subtitle-color-style-name string?]{
 Like @racket[render-syntax-focused-syntax-color-style-name] but
 for the subheadings when the syntax browser details are shown.

 Defaults to a style that's derived from the basic style with
 the foreground color @racket["navy"].
}
