#lang scribble/doc
@(require "common.rkt"
          (for-label racket/gui/dynamic racket/pretty racket/gui/base setup/dirs))

@title{Init Libraries}

@defmodule*/no-declare[(racket/gui/init)]{The
@racketmodname[racket/gui/init] library is the default start-up
library for GRacket. It re-exports the @racketmodname[racket/init] and
@racketmodname[racket/gui/base] libraries, and it sets
@racket[current-load] to use @racket[text-editor-load-handler].}

@defmodule*/no-declare[(racket/gui/interactive)]{
 Similar to @racketmodname[racket/interactive], but for
 GRacket. This library can be changed by modifying 
 @racket['gui-interactive-file] in the
 @filepath{config.rktd} file in @racket[(find-config-dir)].
 Additionally, if the file @filepath{gui-interactive.rkt}
 exists in @racket[(find-system-path 'addon-dir)], it is run
 rather than the installation wide graphical interactive
 module.

 This library runs the 
 @racket[(find-graphical-system-path 'init-file)] file in
 the users home directory if it exists, rather than their 
 @racket[(find-system-path 'init-file)]. Unlike 
 @racketmodname[racket/interactive], this library does not
 start @racketmodname[xrepl].

 @history[#:added "1.27"]}
