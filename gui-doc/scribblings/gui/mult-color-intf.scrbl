#lang scribble/doc
@(require "common.rkt")

@definterface/title[mult-color<%> ()]{

A @racket[mult-color<%>] object is used to scale the RGB values of a
 @racket[color%] object. A @racket[mult-color<%>] object exist only
 within a @racket[style-delta%] object.

See also @method[style-delta% get-foreground-mult] and
 @method[style-delta% get-background-mult].



@defmethod[(get [r (box/c real?)]
                [g (box/c real?)]
                [b (box/c real?)]
                [a (or/c (box/c real?) #f) #f])
           void?]{

Gets all of the scaling values.

@boxisfill[@racket[r] @elem{the scaling value for the red component of the color}]
@boxisfill[@racket[g] @elem{the scaling value for the green component of the color}]
@boxisfill[@racket[b] @elem{the scaling value for the blue component of the color}]
@boxisfillnull[@racket[a] @elem{the scaling value for the alpha component of the color}]

@history[#:changed "1.63" @elem{Added the @racket[a] optional argument.}]}

@defmethod[(get-a)
           real?]{

Gets the multiplicative scaling value for the alpha component of the color.

@history[#:added "1.63"]}


@defmethod[(get-b)
           real?]{

Gets the multiplicative scaling value for the blue component of the color.

}

@defmethod[(get-g)
           real?]{

Gets the multiplicative scaling value for the green component of the color.

}

@defmethod[(get-r)
           real?]{

Gets the multiplicative scaling value for the red component of the color.

}

@defmethod[(set [r real?]
                [g real?]
                [b real?]
                [a real? 1.0])
           void?]{

Sets all of the scaling values.

@history[#:changed "1.63" @elem{Added the @racket[a] optional argument.}]}

@defmethod[(set-a [v real?])
           void?]{

Sets the multiplicative scaling value for the alpha component of the color.

@history[#:added "1.63"]}

@defmethod[(set-b [v real?])
           void?]{

Sets the multiplicative scaling value for the blue component of the color.

}

@defmethod[(set-g [v real?])
           void?]{

Sets the multiplicative scaling value for the green component of the
color.

}

@defmethod[(set-r [v real?])
           void?]{

Sets the additive value for the red component of the color.

}}
