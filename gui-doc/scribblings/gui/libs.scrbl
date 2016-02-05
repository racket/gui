#lang scribble/doc
@(require "common.rkt")

@(define draw-doc '(lib "scribblings/draw/draw.scrbl"))

@title[#:tag "libs"]{Platform Dependencies}

See @secref[#:doc draw-doc "libs"] in @other-manual[draw-doc] for
information on platform library dependencies for
@racketmodname[racket/draw]. On Unix, GTK+ 3 is used if its libraries
can be found and the @indexed-envvar{PLT_GTK2} environment is not
defined. Otherwise, GTK+ 2 is used. The following additional system
libraries must be installed for @racketmodname[racket/gui/base] in
either case:

@itemlist[
 @item{@filepath{libgdk-3.0[.0]} (GTK+ 3) or @filepath{libgdk-x11-2.0[.0]} (GTK+ 2)}
 @item{@filepath{libgdk_pixbuf-2.0[.0]} (GTK+ 2)}
 @item{@filepath{libgtk-3.0[.0]} (GTK+ 3) or @filepath{libgtk-x11-2.0[.0]} (GTK+ 2)}
 @item{@filepath{libgio-2.0[.0]} --- optional, for detecting interface scaling}
 @item{@filepath{libGL[.1]} --- optional, for OpenGL support}
 @item{@filepath{libunique-1.0[.0]} --- optional, for single-instance support (GTK+ 2)}
]
