#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/message}}

@defclass/title[message% object% (control<%>)]{

A message control is a static line of text or a static bitmap. The
 text or bitmap corresponds to the message's label (see
@method[message% set-label]).


@defconstructor[([label (or/c label-string? (is-a?/c bitmap%)
                              (or/c 'app 'caution 'stop))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (or/c 'deleted)) null]
                 [font (is-a?/c font%) normal-control-font]
                 [color (or/c #f string? (is-a?/c color%)) #f]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 2]
                 [horiz-margin spacing-integer? 2]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f]
                 [auto-resize any/c #f])]{

Creates a string or bitmap message initially showing @racket[label].
 @bitmaplabeluse[label] An @indexed-racket['app],
 @indexed-racket['caution], or @indexed-racket['stop] symbol for
 @racket[label] indicates an icon; @racket['app] is the application
 icon (Windows and Mac OS) or a generic ``info'' icon (X),
 @racket['caution] is a caution-sign icon, and @racket['stop] is a
 stop-sign icon.

@labelsimplestripped[@racket[label] @elem{message}]

@DeletedStyleNote[@racket[style] @racket[parent]]{message}

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]

The @racket[color] argument determines the color of the text label.  It
has no effect on symbol and bitmap labels.  If it is @racket[#f], the
system default text color is used.  If it is a string, then the color
is looked up in @racket[the-color-database].

If @racket[auto-resize] is not @racket[#f], then automatic resizing is
initially enabled (see @method[message% auto-resize]), and the
@racket[message%] object's @tech{graphical minimum size} is as small as
possible.

@history[
  #:changed "1.58" @elem{Added the @racket[color] argument.}
]

}

@defmethod*[([(auto-resize) boolean?]
             [(auto-resize [on? any/c]) void?])]{

Reports or sets whether the @racket[message%]'s @method[area<%> min-width] and
@method[area<%> min-height] are automatically set when the label is changed
via @method[message% set-label].

}

@defmethod[#:mode override
           (set-label [label (or/c label-string? (is-a?/c bitmap%))])
           void?]{

The same as @xmethod[window<%> set-label] when @racket[label] is a
 string.

Otherwise, sets the bitmap label for a bitmap message.
 @bitmaplabeluseisbm[label] @|bitmapiforiglabel|

}

@defmethod*[([(set-color [color (is-a?/c color%)]) void?]
             [(set-color [color-name string?]) void?])]{
  Sets the label's text color.  This method has no effect if the label
  is a symbol or a bitmap.

  @history[#:added "1.58"]
}

@defmethod[(get-color) (or/c #f (is-a?/c color%))]{
  Returns the current user-specified label color or @racket[#f] if the
  system default is used.

  @history[#:added "1.58"]
}

}
