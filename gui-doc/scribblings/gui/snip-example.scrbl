#lang scribble/doc
@(require scribble/bnf "common.rkt")

@title[#:tag "snip-example"]{Implementing New Snips}

To support new kinds of content within an editor, derive a subclass of @racket[snip%]
to implement a new kind of @tech{snip}.
In deriving a new snip implementation, the following methods of @racket[snip%]
must be overridden to
create a useful snip:

@itemize[

 @item{@method[snip% get-extent]} 

 @item{@method[snip% draw]} 

 @item{@method[snip% copy]} 

 @item{@method[snip% resize] if the snip can be resized by the user}

 @item{@method[snip% partial-offset] if the snip can contain more than
       one @techlink{item}}

 @item{@method[snip% split] if the snip can contain more than one @techlink{item}}

 @item{@method[snip% size-cache-invalid] if the snip caches the result to @method[snip% get-extent]} 

 @item{@method[snip% get-text] (not required)}

 @item{@method[snip% find-scroll-step], @method[snip%
       get-num-scroll-steps], and @method[snip%
       get-scroll-step-offset] if the snip can contain more than one
       scroll position}

 @item{@method[snip% set-unmodified] if the snip's internal state can
       be modified by the user, and call @method[snip-admin% modified]
       in the snip's administrator when the state changes the first
       time}

]

If a snip can contain more than one @techlink{item}, then the snip's @techlink{count}
 must be maintained as well.

To define a class of snips that can be saved or cut-and-pasted (see also
@|snipclassdiscuss|):

@itemize[

 @item{Create an instance of @racket[snip-class%], implementing the
       @method[snip-class% read] method. Export the
       @racket[snip-class%] instance as @racket[snip-class] from a
       module, and use a classname of the form @racket["(lib ...)"] as
       described in @|snipclassdiscuss|.}

 @item{For each instance of the snip class, set the snip's class object 
       with @method[snip% set-snipclass].}

 @item{Override the @method[snip% copy] method.}

 @item{Override the @method[snip% write] method.}

]

In deriving a new @racket[snip-class%] class:

@itemize[

 @item{Set the classname using @method[snip-class% set-classname].}

 @item{Set the version using 
       @method[snip-class% set-version].} 

 @item{Install the class into the list returned by
       @racket[get-the-snip-class-list] using the
       @method[snip-class-list<%> add] method. Note that if the same
       name is inserted into the same class list multiple times, all
       but the first insertion is ignored.}

]

To define a class of snips that read specially with
@racket[open-input-text-editor]:

@itemize[

 @item{Make your @racket[snip%] class implement @racket[readable-snip<%>].}

 @item{Implement the @method[readable-snip<%> read-special] method.}

]

As an example, the following module implements a snip that draws a
circle. Clicking on the snip causes the circle to grow. To enable
copying an instance of the snip from one program/eventspace to
another, the module should be @filepath{main.rkt} a
@filepath{circle-snip} directory that is installed as a
@filepath{circle-snip} package.

@codeblock{
#lang racket/base
(require racket/class
         racket/snip
         racket/format)

(provide circle-snip%
         (rename-out [circle-snip-class snip-class]))

(define circle-snip%
  (class snip%
    (inherit set-snipclass
             get-flags set-flags
             get-admin)
    (init-field [size 20.0])

    (super-new)
    (set-snipclass circle-snip-class)
    (send (get-the-snip-class-list) add circle-snip-class)
    (set-flags (cons 'handles-events (get-flags)))
    
    (define/override (get-extent dc x y	 	 	 	 
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (maybe-set-box! w (+ 2.0 size))
      (maybe-set-box! h (+ 2.0 size))
      (maybe-set-box! descent 1.0)
      (maybe-set-box! space 1.0)
      (maybe-set-box! lspace 1.0)
      (maybe-set-box! rspace 1.0))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-ellipse (+ x 1.0) (+ y 1.0) size size))

    (define/override (copy)
      (new circle-snip% [size size]))

    (define/override (write f)
      (send f put size))

    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down?)
        (set! size (+ 1.0 size))
        (define admin (get-admin))
        (when admin
          (send admin resized this #t))))))

(define circle-snip-class%
  (class snip-class%
    (inherit set-classname)

    (super-new)
    (set-classname (~s '(lib "main.rkt" "circle-snip")))

    (define/override (read f)
      (define size-b (box 0.0))
      (send f get size-b)
      (new circle-snip% [size (unbox size-b)]))))

(define circle-snip-class (new circle-snip-class%))
}
