;; mysterxe.ss

(require-library "xml.ss" "xml")

(unit/sig mysterx:mysterx^
  (import 
   mzlib:function^
   mzlib:string^
   [mxprims : mysterx:prims^]
   [xml : xml^])	

  (define com-invoke mxprims:com-invoke)
  (define com-set-property! mxprims:com-set-property!)
  (define com-get-property mxprims:com-get-property)
  (define com-methods mxprims:com-methods)
  (define com-get-properties mxprims:com-get-properties)
  (define com-set-properties mxprims:com-set-properties)
  (define com-events mxprims:com-events)
  (define com-method-type mxprims:com-method-type)
  (define com-get-property-type mxprims:com-get-property-type)
  (define com-set-property-type mxprims:com-set-property-type)
  (define com-event-type mxprims:com-event-type)
  (define com-object-type mxprims:com-object-type)
  (define com-is-a? mxprims:com-is-a?)
  (define com-help mxprims:com-help)
  (define com-register-event-handler mxprims:com-register-event-handler)
  (define com-unregister-event-handler mxprims:com-unregister-event-handler)
  (define com-all-coclasses mxprims:com-all-coclasses)
  (define com-all-controls mxprims:com-all-controls)
  (define coclass->html mxprims:coclass->html)
  (define cocreate-instance mxprims:cocreate-instance)
  (define com-object-eq? mxprims:com-object-eq?)
  (define com-object? mxprims:com-object?)
  (define com-omit mxprims:com-omit)

  (define html-sem (make-semaphore 1))   ; protects HTML insertions
  (define html-wait (lambda () (semaphore-wait html-sem)))
  (define html-post (lambda () (semaphore-post html-sem)))

  (define (xexp->string xexp)
	  (lambda (xexp)
	    (parameterize ([xml:empty-tag-shorthand #f])
	      (let* ([port (open-output-string)]
        	     [xml (xml:xexpr->xml xexp)])
	        (xml:write-xml/content xml port)
        	(get-output-string port)))))

  (define mx-element%
    (class object% (document dhtml-element)
	   
	   (private
	    [elt dhtml-element]
	    [doc document])

	   (public
	    [insert-html
	     (lambda (s)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:element-insert-html elt s))
		html-post))]
	    [append-html
	     (lambda (s)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:element-append-html elt s))
		html-post))]
	    [replace-html
	     (lambda (s)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:element-replace-html elt s))
		html-post))]
	    [insert-text 
	     (lambda (s)
	       (mxprims:element-insert-text elt s))]
	    [append-text
	     (lambda (s)
	       (mxprims:element-append-text elt s))]
	    [insert-object 
	     (opt-lambda (object width height [size 'pixels])
	       (dynamic-wind
		html-wait
		(lambda () 
		  (let ([old-objects (mxprims:document-objects doc)])
		    (mxprims:element-insert-html 
		     elt 
		     (coclass->html object width height size))
		       (let* ([new-objects (mxprims:document-objects doc)]
			      [obj (car (remove* old-objects new-objects
						 com-object-eq?))])
			 (mxprims:com-register-object obj)
			 obj)))
		html-post))]
	    [append-object 
	     (opt-lambda (object width height [size 'pixels])
	       (dynamic-wind
		html-wait
		(lambda ()
		  (let* ([old-objects (mxprims:document-objects doc)])
		    (mxprims:element-append-html 
		     elt 
		     (coclass->html object width height size))
		       (let* ([new-objects (mxprims:document-objects doc)]
			      [obj (car (remove* old-objects
						 new-objects
						 com-object-eq?))])
			 (mxprims:com-register-object obj)
			 obj)))
		html-post))]
	    [attribute
	     (lambda (s)
	       (mxprims:element-attribute elt s))]
	    [set-attribute!
	     (lambda (a v)
	       (mxprims:element-set-attribute! elt a v))]
	    [click
	     (lambda ()
	       (mxprims:element-click elt))]
	    [tag
	     (lambda ()
	       (mxprims:element-tag elt))]
	    [font-family
	     (lambda ()
	       (mxprims:element-font-family elt))]
	    [set-font-family!
	     (lambda (s)
	       (mxprims:element-set-font-family! elt s))]
	    [font-style
	     (lambda ()
	       (mxprims:element-font-style elt))]
	    [set-font-style!
	     (lambda (s)
	       (mxprims:element-set-font-style! elt s))]
	    [font-variant
	     (lambda ()
	       (mxprims:element-font-variant elt))]
	    [set-font-variant!
	     (lambda (s)
	       (mxprims:element-set-font-variant! elt s))]
	    [font-weight
	     (lambda ()
	       (mxprims:element-font-weight elt))]
	    [set-font-weight!
	     (lambda (s)
	       (mxprims:element-set-font-weight! elt s))]
	    [font
	     (lambda ()
	       (mxprims:element-font elt))]
	    [set-font!
	     (lambda (s)
	       (mxprims:element-set-font! elt s))]
	    [background
	     (lambda ()
	       (mxprims:element-background elt))]
	    [set-background!
	     (lambda (s)
	       (mxprims:element-set-background! elt s))]
	    [background-image
	     (lambda ()
	       (mxprims:element-background-image elt))]
	    [set-background-image!
	     (lambda (s)
	       (mxprims:element-set-background-image! elt s))]
	    [background-repeat
	     (lambda ()
	       (mxprims:element-background-repeat elt))]
	    [set-background-repeat!
	     (lambda (s)
	       (mxprims:element-set-background-repeat! elt s))]
	    [background-position
	     (lambda ()
	       (mxprims:element-background-position elt))]
	    [set-background-position!
	     (lambda (s)
	       (mxprims:element-set-background-position! elt s))]
	    [text-decoration
	     (lambda ()
	       (mxprims:element-text-decoration elt))]
	    [set-text-decoration!
	     (lambda (s)
	       (mxprims:element-set-text-decoration! elt s))]
	    [text-transform
	     (lambda ()
	       (mxprims:element-text-transform elt))]
	    [set-text-transform!
	     (lambda (s)
	       (mxprims:element-set-text-transform! elt s))]
	    [text-align
	     (lambda ()
	       (mxprims:element-text-align elt))]
	    [set-text-align!
	     (lambda (s)
	       (mxprims:element-set-text-align! elt s))]
	    [margin
	     (lambda ()
	       (mxprims:element-margin elt))]
	    [set-margin!
	     (lambda (s)
	       (mxprims:element-set-margin! elt s))]
	    [padding
	     (lambda ()
	       (mxprims:element-padding elt))]
	    [set-padding!
	     (lambda (s)
	       (mxprims:element-set-padding! elt s))]
	    [border
	     (lambda ()
	       (mxprims:element-border elt))]
	    [set-border!
	     (lambda (s)
	       (mxprims:element-set-border! elt s))]
	    [border-top
	     (lambda ()
	       (mxprims:element-border-top elt))]
	    [set-border-top!
	     (lambda (s)
	       (mxprims:element-set-border-top! elt s))]
	    [border-bottom
	     (lambda ()
	       (mxprims:element-border-bottom elt))]
	    [set-border-bottom!
	     (lambda (s)
	       (mxprims:element-set-border-bottom! elt s))]
	    [border-left
	     (lambda ()
	       (mxprims:element-border-left elt))]
	    [set-border-left!
	     (lambda (s)
	       (mxprims:element-set-border-left! elt s))]
	    [border-right
	     (lambda ()
	       (mxprims:element-border-right elt))]
	    [set-border-right!
	     (lambda (s)
	       (mxprims:element-set-border-right! elt s))]
	    [border-color
	     (lambda ()
	       (mxprims:element-border-color elt))]
	    [set-border-color!
	     (lambda (s)
	       (mxprims:element-set-border-color! elt s))]
	    [border-width
	     (lambda ()
	       (mxprims:element-border-width elt))]
	    [set-border-width!
	     (lambda (s)
	       (mxprims:element-set-border-width! elt s))]
	    [border-style
	     (lambda ()
	       (mxprims:element-border-style elt))]
	    [set-border-style!
	     (lambda (s)
	       (mxprims:element-set-border-style! elt s))]
	    [border-top-style
	     (lambda ()
	       (mxprims:element-border-top-style elt))]
	    [set-border-top-style!
	     (lambda (s)
	       (mxprims:element-set-border-top-style! elt s))]
	    [border-bottom-style
	     (lambda ()
	       (mxprims:element-border-bottom-style elt))]
	    [set-border-bottom-style!
	     (lambda (s)
	       (mxprims:element-set-border-bottom-style! elt s))]
	    [border-left-style
	     (lambda ()
	       (mxprims:element-border-left-style elt))]
	    [set-border-left-style!
	     (lambda (s)
	       (mxprims:element-set-border-left-style! elt s))]
	    [border-right-style
	     (lambda ()
	       (mxprims:element-border-right-style elt))]
	    [set-border-right-style!
	     (lambda (s)
	       (mxprims:element-set-border-right-style! elt s))]
	    [style-float
	     (lambda ()
	       (mxprims:element-style-float elt))]
	    [set-style-float!
	     (lambda (s)
	       (mxprims:element-set-style-float! elt s))]
	    [clear
	     (lambda ()
	       (mxprims:element-clear elt))]
	    [set-clear!
	     (lambda (s)
	       (mxprims:element-set-clear! elt s))]
	    [display
	     (lambda ()
	       (mxprims:element-display elt))]
	    [set-display!
	     (lambda (s)
	       (mxprims:element-set-display! elt s))]
	    [visibility
	     (lambda ()
	       (mxprims:element-visibility elt))]
	    [set-visibility!
	     (lambda (s)
	       (mxprims:element-set-visibility! elt s))]
	    [list-style-type
	     (lambda ()
	       (mxprims:element-list-style-type elt))]
	    [set-list-style-type!
	     (lambda (s)
	       (mxprims:element-set-list-style-type! elt s))]
	    [list-style-position
	     (lambda ()
	       (mxprims:element-list-style-position elt))]
	    [set-list-style-position!
	     (lambda (s)
	       (mxprims:element-set-list-style-position! elt s))]
	    [list-style-image
	     (lambda ()
	       (mxprims:element-list-style-image elt))]
	    [set-list-style-image!
	     (lambda (s)
	       (mxprims:element-set-list-style-image! elt s))]
	    [list-style
	     (lambda ()
	       (mxprims:element-list-style elt))]
	    [set-list-style!
	     (lambda (s)
	       (mxprims:element-set-list-style! elt s))]
	    [whitespace
	     (lambda ()
	       (mxprims:element-whitespace elt))]
	    [set-whitespace!
	     (lambda (s)
	       (mxprims:element-set-whitespace! elt s))]
	    [position
	     (lambda ()
	       (mxprims:element-position elt))]
	    [overflow
	     (lambda ()
	       (mxprims:element-overflow elt))]
	    [set-overflow!
	     (lambda (s)
	       (mxprims:element-set-overflow! elt s))]
	    [pagebreak-before
	     (lambda ()
	       (mxprims:element-pagebreak-before elt))]
	    [set-pagebreak-before!
	     (lambda (s)
	       (mxprims:element-set-pagebreak-before! elt s))]
	    [pagebreak-after
	     (lambda ()
	       (mxprims:element-pagebreak-after elt))]
	    [set-pagebreak-after!
	     (lambda (s)
	       (mxprims:element-set-pagebreak-after! elt s))]
	    [css-text
	     (lambda ()
	       (mxprims:element-css-text elt))]
	    [set-css-text!
	     (lambda (s)
	       (mxprims:element-set-css-text! elt s))]
	    [cursor
	     (lambda ()
	       (mxprims:element-cursor elt))]
	    [set-cursor!
	     (lambda (s)
	       (mxprims:element-set-cursor! elt s))]
	    [clip
	     (lambda ()
	       (mxprims:element-clip elt))]
	    [set-clip!
	     (lambda (s)
	       (mxprims:element-set-clip! elt s))]
	    [filter
	     (lambda ()
	       (mxprims:element-filter elt))]
	    [set-filter!
	     (lambda (s)
	       (mxprims:element-set-filter! elt s))]
	    [style-string
	     (lambda ()
	       (mxprims:element-style-string elt))]
	    [text-decoration-none
	     (lambda ()
	       (mxprims:element-text-decoration-none elt))]
	    [set-text-decoration-none!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-none! elt s))]
	    [text-decoration-underline
	     (lambda ()
	       (mxprims:element-text-decoration-underline elt))]
	    [set-text-decoration-underline!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-underline! elt s))]
	    [text-decoration-overline
	     (lambda ()
	       (mxprims:element-text-decoration-overline elt))]
	    [set-text-decoration-overline!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-overline! elt s))]
	    [text-decoration-linethrough
	     (lambda ()
	       (mxprims:element-text-decoration-linethrough elt))]
	    [set-text-decoration-linethrough!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-linethrough! elt s))]
	    [text-decoration-blink
	     (lambda ()
	       (mxprims:element-text-decoration-blink elt))]
	    [set-text-decoration-blink!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-blink! elt s))]
	    [pixel-top
	     (lambda ()
	       (mxprims:element-pixel-top elt))]
	    [set-pixel-top!
	     (lambda (s)
	       (mxprims:element-set-pixel-top! elt s))]
	    [pixel-left
	     (lambda ()
	       (mxprims:element-pixel-left elt))]
	    [set-pixel-left!
	     (lambda (s)
	       (mxprims:element-set-pixel-left! elt s))]
	    [pixel-width
	     (lambda ()
	       (mxprims:element-pixel-width elt))]
	    [set-pixel-width!
	     (lambda (s)
	       (mxprims:element-set-pixel-width! elt s))]
	    [pixel-height
	     (lambda ()
	       (mxprims:element-pixel-height elt))]
	    [set-pixel-height!
	     (lambda (s)
	       (mxprims:element-set-pixel-height! elt s))]
	    [pos-top
	     (lambda ()
	       (mxprims:element-pos-top elt))]
	    [set-pos-top!
	     (lambda (s)
	       (mxprims:element-set-pos-top! elt s))]
	    [pos-left
	     (lambda ()
	       (mxprims:element-pos-left elt))]
	    [set-pos-left!
	     (lambda (s)
	       (mxprims:element-set-pos-left! elt s))]
	    [pos-width
	     (lambda ()
	       (mxprims:element-pos-width elt))]
	    [set-pos-width!
	     (lambda (s)
	       (mxprims:element-set-pos-width! elt s))]
	    [pos-height
	     (lambda ()
	       (mxprims:element-pos-height elt))]
	    [set-pos-height!
	     (lambda (s)
	       (mxprims:element-set-pos-height! elt s))]
	    [font-size
	     (lambda ()
	       (mxprims:element-font-size elt))]
	    [set-font-size!
	     (lambda (s)
	       (mxprims:element-set-font-size! elt s))]
	    [color
	     (lambda ()
	       (mxprims:element-color elt))]
	    [set-color!
	     (lambda (s)
	       (mxprims:element-set-color! elt s))]
	    [background-color
	     (lambda ()
	       (mxprims:element-background-color elt))]
	    [set-background-color!
	     (lambda (s)
	       (mxprims:element-set-background-color! elt s))]
	    [background-position-x
	     (lambda ()
	       (mxprims:element-background-position-x elt))]
	    [set-background-position-x!
	     (lambda (s)
	       (mxprims:element-set-background-position-x! elt s))]
	    [background-position-y
	     (lambda ()
	       (mxprims:element-background-position-y elt))]
	    [set-background-position-y!
	     (lambda (s)
	       (mxprims:element-set-background-position-y! elt s))]
	    [word-spacing
	     (lambda ()
	       (mxprims:element-word-spacing elt))]
	    [set-word-spacing!
	     (lambda (s)
	       (mxprims:element-set-word-spacing! elt s))]
	    [letter-spacing
	     (lambda ()
	       (mxprims:element-letter-spacing elt))]
	    [set-letter-spacing!
	     (lambda (s)
	       (mxprims:element-set-letter-spacing! elt s))]
	    [vertical-align
	     (lambda ()
	       (mxprims:element-vertical-align elt))]
	    [set-vertical-align!
	     (lambda (s)
	       (mxprims:element-set-vertical-align! elt s))]
	    [text-indent
	     (lambda ()
	       (mxprims:element-text-indent elt))]
	    [set-text-indent!
	     (lambda (s)
	       (mxprims:element-set-text-indent! elt s))]
	    [line-height
	     (lambda ()
	       (mxprims:element-line-height elt))]
	    [set-line-height!
	     (lambda (s)
	       (mxprims:element-set-line-height! elt s))]
	    [margin-top
	     (lambda ()
	       (mxprims:element-margin-top elt))]
	    [set-margin-top!
	     (lambda (s)
	       (mxprims:element-set-margin-top! elt s))]
	    [margin-bottom
	     (lambda ()
	       (mxprims:element-margin-bottom elt))]
	    [set-margin-bottom!
	     (lambda (s)
	       (mxprims:element-set-margin-bottom! elt s))]
	    [margin-left
	     (lambda ()
	       (mxprims:element-margin-left elt))]
	    [set-margin-left!
	     (lambda (s)
	       (mxprims:element-set-margin-left! elt s))]
	    [margin-right
	     (lambda ()
	       (mxprims:element-margin-right elt))]
	    [set-margin-right!
	     (lambda (s)
	       (mxprims:element-set-margin-right! elt s))]
	    [padding-top
	     (lambda ()
	       (mxprims:element-padding-top elt))]
	    [set-padding-top!
	     (lambda (s)
	       (mxprims:element-set-padding-top! elt s))]
	    [padding-bottom
	     (lambda ()
	       (mxprims:element-padding-bottom elt))]
	    [set-padding-bottom!
	     (lambda (s)
	       (mxprims:element-set-padding-bottom! elt s))]
	    [padding-left
	     (lambda ()
	       (mxprims:element-padding-left elt))]
	    [set-padding-left!
	     (lambda (s)
	       (mxprims:element-set-padding-left! elt s))]
	    [padding-right
	     (lambda ()
	       (mxprims:element-padding-right elt))]
	    [set-padding-right!
	     (lambda (s)
	       (mxprims:element-set-padding-right! elt s))]
	    [border-top-color
	     (lambda ()
	       (mxprims:element-border-top-color elt))]
	    [set-border-top-color!
	     (lambda (s)
	       (mxprims:element-set-border-top-color! elt s))]
	    [border-bottom-color
	     (lambda ()
	       (mxprims:element-border-bottom-color elt))]
	    [set-border-bottom-color!
	     (lambda (s)
	       (mxprims:element-set-border-bottom-color! elt s))]
	    [border-left-color
	     (lambda ()
	       (mxprims:element-border-left-color elt))]
	    [set-border-left-color!
	     (lambda (s)
	       (mxprims:element-set-border-left-color! elt s))]
	    [border-right-color
	     (lambda ()
	       (mxprims:element-border-right-color elt))]
	    [set-border-right-color!
	     (lambda (s)
	       (mxprims:element-set-border-right-color! elt s))]
	    [border-top-width
	     (lambda ()
	       (mxprims:element-border-top-width elt))]
	    [set-border-top-width!
	     (lambda (s)
	       (mxprims:element-set-border-top-width! elt s))]
	    [border-bottom-width
	     (lambda ()
	       (mxprims:element-border-bottom-width elt))]
	    [set-border-bottom-width!
	     (lambda (s)
	       (mxprims:element-set-border-bottom-width! elt s))]
	    [border-left-width
	     (lambda ()
	       (mxprims:element-border-left-width elt))]
	    [set-border-left-width!
	     (lambda (s)
	       (mxprims:element-set-border-left-width! elt s))]
	    [border-right-width
	     (lambda ()
	       (mxprims:element-border-right-width elt))]
	    [set-border-right-width!
	     (lambda (s)
	       (mxprims:element-set-border-right-width! elt s))]
	    [width
	     (lambda ()
	       (mxprims:element-width elt))]
	    [set-width!
	     (lambda (s)
	       (mxprims:element-set-width! elt s))]
	    [height
	     (lambda ()
	       (mxprims:element-height elt))]
	    [set-height!
	     (lambda (s)
	       (mxprims:element-set-height! elt s))]
	    [top
	     (lambda ()
	       (mxprims:element-top elt))]
	    [set-top!
	     (lambda (s)
	       (mxprims:element-set-top! elt s))]
	    [left
	     (lambda ()
	       (mxprims:element-left elt))]
	    [set-left!
	     (lambda (s)
	       (mxprims:element-set-left! elt s))]
	    [z-index
	     (lambda ()
	       (mxprims:element-z-index elt))]
	    [set-z-index!
	     (lambda (s)
	       (mxprims:element-set-z-index! elt s))])

	   (sequence (super-init))))

  (define mx-event%
    (class object% (dhtml-event)

	   (private
	    [event dhtml-event])

	   (public

            ; predicates

	    [keypress? (lambda () (mxprims:event-keypress? event))]
	    [keydown? (lambda () (mxprims:event-keydown? event))]
	    [keyup? (lambda () (mxprims:event-keyup? event))] 
	    [mousedown? (lambda () (mxprims:event-mousedown? event))] 
	    [mousemove? (lambda () (mxprims:event-mousemove? event))] 
	    [mouseover? (lambda () (mxprims:event-mouseover? event))] 
	    [mouseout? (lambda () (mxprims:event-mouseout? event))] 
	    [mouseup? (lambda () (mxprims:event-mouseup? event))] 
	    [click? (lambda () (mxprims:event-click? event))] 
	    [dblclick? (lambda () (mxprims:event-dblclick? event))] 
	    [error? (lambda () (mxprims:event-error? event))]
	    
            ; attributes

	    [tag (lambda () (mxprims:event-tag event))]
	    [id (lambda () (mxprims:event-id event))]
	    [from-tag (lambda () (mxprims:event-from-tag event))]
	    [from-id (lambda () (mxprims:event-id event))]
	    [to-tag (lambda () (mxprims:event-to-tag event))]
	    [to-id (lambda () (mxprims:event-to-id event))]
	    [keycode (lambda () (mxprims:event-keycode event))]
	    [shiftkey (lambda () (mxprims:event-shiftkey event))]
	    [ctrlkey (lambda () (mxprims:event-ctrlkey event))]
	    [altkey (lambda () (mxprims:event-altkey event))]
	    [x (lambda () (mxprims:event-x event))]
	    [y (lambda () (mxprims:event-y event))])

	   (sequence (super-init))))


  (define mx-document%
    (class object%

	   ((label "MysterX")
	    (width 'default)
	    (height 'default)
	    (x 'default)
	    (y 'default)
	    (style-options null))

	   (private
	    [doc (mxprims:make-document label width height x y style-options)]
	    [thread-sem (make-semaphore 1)]   ; protects *handler-threads*
	    [thread-wait (lambda () (semaphore-wait thread-sem))]
	    [thread-post (lambda () (semaphore-post thread-sem))]
	    [handler-sem (make-semaphore 1)]  ; protects *handler-table* and its contained hash tables
	    [handler-wait (lambda () (semaphore-wait handler-sem))]
	    [handler-post (lambda () (semaphore-post handler-sem))]
	    [handler-table (make-hash-table)]
	    [handler-thread #f]
	    [block-until-event 
	     (lambda () (mxprims:block-until-event doc))]
	    [make-event-key 
	     (lambda (tag id) ; string x string -> symbol
	       (let ([new-tag (string-copy tag)]
		     [new-id (string-copy id)])
		 (string-uppercase! new-tag)
		    (string-uppercase! new-id)
		    (string->symbol
		     (string-append new-tag "@" new-id))))])

	   (public
	    [show 
	     (lambda (b) 
	       (mxprims:document-show doc b))]
	    [find-element
	     (lambda (tag id)
	       (make-object mx-element% doc (mxprims:document-find-element doc tag id)))]
	    [objects
	     (lambda () 
	       (mxprims:document-objects doc))]
	    [insert-html 
	     (lambda (html-string)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:document-insert-html doc html-string))
		html-post))]
	    [append-html 
	     (lambda (html-string)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:document-append-html doc html-string))
		html-post))]
	    [replace-html 
	     (lambda (html-string)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:document-replace-html doc html-string))
		html-post))]
	    [register-event-handler
	     (lambda (elt fn)
	       (dynamic-wind
		handler-wait
		(lambda () 
		  (let* ([tag (send elt tag)]
			 [id (send elt attribute "id")]) 
		    (let ([key (make-event-key tag id)])
		      (hash-table-remove! handler-table key)
			 (hash-table-put! handler-table key fn))))
		handler-post))]
	    [unregister-event-handler
	     (lambda (elt)
	       (dynamic-wind
		handler-wait
		(lambda () 
		  (let* ([tag (send elt tag)]
			 [id (send elt attribute "id")])
		    (let ([key (make-event-key tag id)])
		      (hash-table-remove! handler-table key))))
		handler-post))]
	    [insert-object 
	     (opt-lambda (object width height [size 'pixels])
	       (dynamic-wind 
		html-wait
		(lambda ()
		  (mxprims:document-insert-html 
		   doc 
		   (coclass->html object width height size))
		  (car (mxprims:document-objects doc)))
		html-post))]
	    [append-object 
	     (opt-lambda (object width height [size 'pixels])
	       (dynamic-wind
		html-wait
		(lambda ()
		  (mxprims:document-append-html 
		   doc 
		   (coclass->html object width height size))
		  (car (last-pair (mxprims:document-objects doc))))
		html-post))]
	    [handle-events 
	     (lambda ()
	       (dynamic-wind
		thread-wait 
                (lambda ()	; no-op if existing handler-thread
		  (unless handler-thread
			  (dynamic-wind
			   handler-wait
			   (lambda ()
			     (let* ([handler-thunk
				     (lambda ()
				       (let loop ()
					 (block-until-event)
				         (let* ([prim-event
						 (with-handlers
						  ([void 
						    (lambda (e) 
						      (printf "~a~n" (exn-message e))
						      (loop))])
						    (mxprims:get-event doc))]
						[event (make-object mx-event% prim-event)]
						[tag (send event tag)]
						[id (send event id)]
						[key (make-event-key tag id)]
						[handler (hash-table-get handler-table key void)])
					   (unless (void? handler)
						   (handler event))
					   (loop))))])
			       (set! handler-thread (thread handler-thunk))))
			   handler-post)))
		thread-post))]
	    [stop-handling-events 
	     (lambda ()
	       (dynamic-wind
		thread-wait
		(lambda () 
		  (when handler-thread
			(kill-thread handler-thread))
		  (set! handler-thread #f))
		thread-post))])

	   (sequence 
	     (super-init))))
	     
  (thread	
   (lambda () 
     (let loop ()
       (mxprims:process-win-events)
	  (sleep)
	  (loop))))

  (let ([old-exit-handler (exit-handler)])
    (exit-handler 
     (lambda (arg)
       (for-each 
	(lambda (obj) 
	  (let ([val (cdr obj)])
	    (cond
	     [(com-object? val)
	      (mxprims:com-release-object (cdr obj))]
	     
             ; rely on GC to release interfaces in documents, elements
	     ; not entirely reliable, since collector is conservative
	     
	     [(or (is-a? val mx-document%)
		  (is-a? val mx-element%))
	      (undefine (car obj))])))
	(make-global-value-list))
       (collect-garbage)
       (mxprims:release-type-table)
       (mxprims:com-terminate)
       (old-exit-handler arg)))))



