#lang scribble/doc
@(require "common.rkt")

@defclass/title[horizontal-panel% panel% ()]{

A horizontal panel arranges its subwindows in a single row. See also
 @racket[panel%].

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (or/c 'border 'deleted
                                      'hscroll 'auto-hscroll 'hide-hscroll
                                      'vscroll 'auto-vscroll 'hide-vscroll)) null]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 0]
                 [horiz-margin spacing-integer? 0]
                 [border spacing-integer? 0]
                 [spacing spacing-integer? 0]
                 [alignment (list/c (or/c 'left 'center 'right)
                                    (or/c 'top 'center 'bottom))
                            '(left center)]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

The @racket[style] flags are the same as for @racket[panel%].

@WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaContKWs[] @AreaKWs[]
}

@defmethod[(set-orientation [horizontal? boolean?]) void?]{
  Sets the orientation of the panel, switching it between
  the behavior of the @racket[vertical-panel%] and that of
  the @racket[horizontal-panel%].
}

@defmethod[(get-orientation) boolean?]{
  Initially returns @racket[#t], but if
  @method[horizontal-panel% set-orientation] is called,
  this method returns whatever the last value passed to it was.
}
}

