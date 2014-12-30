#lang scribble/doc
@(require "common.rkt" (for-label mrlib/image-core))

@title{Image Core}

@defmodule[mrlib/image-core]

This library is the core part of the @racketmodname[2htdp/image #:indirect] library that DrRacket
links into the namespace of all languages that it runs. This ensures that minimal
support for these images are the same in all languages, specifically including
support for printing the images and constructing the core data structures making
up an image.

@defproc[(render-image [image image?]
                       [dc (is-a?/c dc<%>)]
                       [dx real?]
                       [dy real?])
         void?]{
  Draws @racket[image] in @racket[dc] at the position (@racket[dx],@racket[dy]).
}

@defproc[(image? [v any/c]) boolean?]{
 Recognizes the images that library handles.
}

@defproc[(un/cache-image [image image?] [b any/c]) image?]{
  Returns an image that either caches its drawing in the 
  snip @method[snip% draw] method or doesn't, depending on @racket[b].
  
  Not all @racket[image?] values have special caching capabilities;
  in those cases, this returns a copy of the value if it is a 
  @racket[snip%]; otherwise it returns the value itself (if it 
  isn't a @racket[snip%]).
}

@defproc[(compute-image-cache [image image?]) void?]{
  When the image has a bitmap-cache (which it does by default,
  although @racket[un/cache-image] can disable it), this function
  fills in the bitmap, doing the work to draw image into the bitmap.
  
  Ordinarily, the image's bitmap cache is computed the first time
  the image is actually rendered.
}

@defproc[(definitely-same-image? [i1 image?] [i2 image?]) boolean?]{
  Returns @racket[#t] if @racket[i1] and @racket[i2] draw identically
  and @racket[#f] if they may draw the same or may draw differently. 
  
  This test is intended to be cheaper than a full equality comparison.
  It is also used by the implementation of @racket[equal?] on images
  to short-circuit the full check. (The full check draws the two images
  and then compares the resulting bitmaps.)
}
