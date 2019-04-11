#lang scribble/doc
@(require "common.rkt")

@title[#:style 'hidden]

The bindings documented in this section are also provided by the
@racketmodname[racket/gui/base] library.

@declare-exporting[racket/gui/event
                   racket/gui/base
                   racket/gui
                   #:use-sources (mred)]

@defmodule*/no-declare[(racket/gui/event)]

@defclass[event% object% ()]{

An @racket[event%] object contains information about a control,
keyboard, mouse, or scroll event. See also
@racket[control-event%], 
@racket[key-event%],
@racket[mouse-event%], and
@racket[scroll-event%].


@defconstructor[([time-stamp exact-integer? 0])]{

See @method[event% get-time-stamp] for information about
 @racket[time-stamp].

}

@defmethod[(get-time-stamp)
           exact-integer?]{

Returns the time, in milliseconds, when the event occurred. This time
 is compatible with times reported by Racket's
 @racket[current-milliseconds] procedure.

}

@defmethod[(set-time-stamp [time exact-integer?])
           void?]{

Set the time, in milliseconds, when the event occurred. See also
 Racket's @racket[current-milliseconds].

If the supplied value is outside the platform-specific range of time
 values, @|MismatchExn|.

}

 @history[#:changed "7.3.0.1" @elem{Added @racketmodname[racket/gui/event]
            that also exports @racket[event%] and subclasses.}]}
