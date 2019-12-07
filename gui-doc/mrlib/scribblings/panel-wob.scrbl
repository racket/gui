#lang scribble/doc
@(require "common.rkt" (for-label mrlib/panel-wob))

@title{White on Black Panel Predicate}

@defmodule[mrlib/panel-wob]

@defproc[(white-on-black-panel-scheme?) boolean?]{
 Determines if the foreground color of the panel background
 is lighter than the background color. If they appear to
 be the same, @racket[white-on-black-panel-scheme?] returns
 @racket[#f].

 This predicate is intended to determine if the underlying
 operating system is in a ``dark'' mode.
}
