#lang scribble/doc
@(require scribble/bnf
          racket/runtime-path
          (for-label wxme)
          "common.rkt")

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
@filepath{circle-snip} package. The snip also has a @racketmodname[wxme]
reader implementation following it that must be installed as
the file @filepath{wxme-circle-snip.rkt} in the @filepath{circle-snip}
directory.

@(begin
   (define-runtime-path snip-example.rkt "snip-example.rkt")
   (define-runtime-path wxme-circle-snip.rkt "wxme-circle-snip.rkt")
   (define (put-code filename)
     (apply
      typeset-code
      #:context #'here
      (call-with-input-file filename
        (λ (port)
          (for/list ([l (in-lines port)])
            (format "~a\n" l))))))
   (put-code snip-example.rkt))

This is the @filepath{wxme-circle-snip.rkt} file:

@(put-code wxme-circle-snip.rkt))

