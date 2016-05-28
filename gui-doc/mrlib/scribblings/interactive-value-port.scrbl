#lang scribble/doc
@(require "common.rkt" (for-label mrlib/interactive-value-port scheme/pretty))

@title{Interactive Value Port}

@defmodule[mrlib/interactive-value-port]


@defproc[(set-interactive-display-handler
          [port output-port?]
          [#:snip-handler snip-handler
           (or/c #f (-> (is-a?/c snip%) output-port? any))
           #f])
         void?]{

 Sets @racket[port]'s display handler (via
 @racket[port-display-handler]) so that when it encounters
 these values:
 @itemize[@item{syntax objects}
          @item{snips}]

 it uses @racket[write-special] to send snips to the port
 and uses @racketmodname[mrlib/syntax-browser] to turn
 syntax object into snips and then uses
 @racket[write-special] with the result to send it to the
 port. Otherwise, it behaves like the default handler.

 If @racket[snip-handler] is not @racket[#f], then
 @racket[set-interactive-display-handler] passes any snips
 to it (not those it creates by
 @racketmodname[mrlib/syntax-browser]) instead of calling 
 @racket[write-special].

 To show values embedded in lists and other compound object,
 it uses @racket[pretty-display].
}


@defproc[(set-interactive-write-handler
          [port output-port?]
          [#:snip-handler snip-handler
           (or/c #f (-> (is-a?/c snip%) output-port? any))
           #f])
         void?]{

Like @racket[set-interactive-display-handler], but sets the
@racket[port-write-handler] and uses @racket[pretty-write].}


@defproc[(set-interactive-print-handler
          [port output-port?]
          [#:snip-handler snip-handler
           (or/c #f (-> (is-a?/c snip%) output-port? any))
           #f])
         void?]{

Like @racket[set-interactive-display-handler], but sets the
@racket[port-print-handler] and uses @racket[pretty-print].}
