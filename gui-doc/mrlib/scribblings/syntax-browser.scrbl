#lang scribble/doc
@(require "common.rkt" (for-label mrlib/image-core))

@title{Syntax Browser}

@defmodule[mrlib/syntax-browser]

@defproc[(render-syntax/snip [stx syntax?]
                             [#:summary-width summary-width (or/c #f (integer-in 3 #f) +inf.0) 32])
         (is-a?/c snip%)]{
 Constructs a @racket[snip%] object that displays information
 about @racket[stx].

 The @racket[summary-width] parameter controls the width (in
 characters) of the syntax object that is shown before the
 triangle is turned down. If it is @racket[#f], the value of
 the @racket[print-syntax-width] parameter is used.

 @history[#:changed "1.59" @list{Added @racket[summary-width] argument and changed default width to 32.}]
}

@defproc[(render-syntax/window [stx syntax?]
                               [#:summary-width summary-width (or/c #f (integer-in 3 #f) +inf.0) 32])
         void?]{
 Uses @racket[render-syntax/snip]'s result, together with a frame
 and editor-canvas to show @racket[stx].

 @history[#:changed "1.59" @list{Added @racket[summary-width] argument and changed default width to 32.}]
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
