(module seqcontract mzscheme
  (require (lib "class.ss"))
  (provide es-contract-mixin)
  
  (require-for-syntax (lib "stx.ss" "syntax")
                      (lib "boundmap.ss" "syntax"))

  (define-syntax (sequence-contract-mixin stx)
    (syntax-case stx (state-machine)
      [(_ (state-machine
           [name exp (method-name states ...) ...] ...)
          clauses ...)
       (and (andmap identifier? (syntax->list (syntax (name ...))))
            (andmap (lambda (x) (andmap identifier? (syntax->list x)))
                    (syntax->list (syntax ((method-name ...) ...))))
            (andmap (lambda (xs) 
                      (andmap (lambda (x) (andmap identifier? (syntax->list x)))
                              (syntax->list xs)))
                    (syntax->list (syntax (((states ...) ...) ...)))))
       (let ()
         (define state-names (syntax->list (syntax (name ...))))
         (define predicate-names (generate-temporaries (syntax (name ...))))
         
         (define state-name->predicate-name
           (let ([mapping (make-bound-identifier-mapping)])
             (for-each (lambda (state-name predicate-name)
                         (bound-identifier-mapping-put! mapping state-name predicate-name))
                       state-names
                       predicate-names)
             (lambda (state-name)
               (bound-identifier-mapping-get mapping state-name))))
         
         (define-struct state-desc (method-name predicate-name state-name result-predicates) (make-inspector))
         
         ;; -> mapping[state-name-symbol -o> state-desc]
         (define (build-table)
           (let ([mapping (new-mapping)])
             (for-each 
              (lambda (state-name-stx predicate-name-stx method-names state-namess)
                (for-each
                 (lambda (method-name state-names-stx)
                   (extend-mapping/at-end
                    mapping
                    method-name
                    (make-state-desc method-name
                                     predicate-name-stx
                                     state-name-stx
                                     (syntax->list state-names-stx))))
                 (syntax->list method-names)
                 (syntax->list state-namess)))
              (syntax->list (syntax (name ...)))
              predicate-names
              (syntax->list (syntax ((method-name ...) ...)))
              (syntax->list (syntax (((states ...) ...) ...))))
             mapping))
         
         (define (build-overriding-method mapping state-descs)
           (with-syntax ([method-name (state-desc-method-name (car state-descs))]
                         [super-method-name (build-super-name (state-desc-method-name (car state-descs)))]
                         [(predicate-name ...) (map state-desc-predicate-name state-descs)]
                         [(state-name ...) (map state-desc-state-name state-descs)]
                         [((result-predicate-state ...) ...)
                          (map state-desc-result-predicates state-descs)]
                         [((result-predicate-name ...) ...)
                          (map 
                           (lambda (state-desc)
                             (map state-name->predicate-name
                                  (state-desc-result-predicates state-desc)))
                           state-descs)])
             (syntax
              (begin
                (rename [super-method-name method-name])
                (define/override (method-name . x)
                  (cond
                    [(predicate-name)
                     (super-method-name . x)
                     (unless (or (result-predicate-name) ...)
                       (sequence-contract-violation 
                        'positive 
                        "expected one of states ~s after calling ~s in state ~s"
                        '(result-predicate-state ...)
                        'method-name
                        'state-name))]
                    ...
                    [else
                     (sequence-contract-violation 
                      'negative
                      "method ~s cannot be called, except in states ~s"
                      'method-name
                      '(state-name ...))]))))))
         
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;
         ;; finite mapping code
         ;;
         
         (define (new-mapping) (make-hash-table))
         (define (set-mapping mapping key-stx val)
           (hash-table-put! mapping (syntax-e key-stx) val))
         (define get-mapping
           (case-lambda
             [(mapping key-stx) (get-mapping mapping key-stx (lambda () (error 'get-mapping "ack!")))]
             [(mapping key-stx fail)
              (hash-table-get mapping (syntax-e key-stx) fail)]))
         (define (extend-mapping/at-end mapping key-stx ele)
           (set-mapping mapping key-stx 
                        (append
                         (get-mapping mapping key-stx (lambda () null))
                         (list ele))))
         (define (mapping-map f mapping)
           (hash-table-map mapping f))
         
         ;;
         ;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         (define (build-super-name name)
           (datum->syntax-object
            name
            (string->symbol
             (format 
              "super-~a"
              (syntax-object->datum name)))))
         
         (define table (build-table))
         (with-syntax ([(predicate-names ...) predicate-names]
                       [(overriding-method ...) (mapping-map
                                                 (lambda (k vs) (build-overriding-method table vs))
                                                 table)]
                       
                       ;; syntax system stuff for super-instantiate, super-make-object, and this
                       [this (datum->syntax-object (syntax _) 'this stx)]
                       [super-instantiate (datum->syntax-object (syntax _) 'super-instantiate stx)]
                       [super-make-object (datum->syntax-object (syntax _) 'super-make-object stx)]
                       [super-new (datum->syntax-object (syntax _) 'super-new stx)])
           
           (syntax
            (lambda (%)
              (class*/names (this super-instantiate super-make-object super-new) % ()
                (define/private predicate-names (lambda () exp)) ...
                overriding-method ... 
                clauses ...)))))]))

  (define (sequence-contract-violation dir fmt . args)
    (apply error
           'sequence-contract-violation 
           (string-append (format "~a: " dir) fmt)
           args))

  (define es-contract-mixin 
    (sequence-contract-mixin
     (state-machine
      [in-edit-sequence
       (in-edit-sequence?)
       (begin-edit-sequence in-edit-sequence)
       (end-edit-sequence in-edit-sequence out-of-edit-sequence)]
      [out-of-edit-sequence
       (not (in-edit-sequence?))
       (begin-edit-sequence in-edit-sequence)])
     
     (inherit in-edit-sequence?)
     (super-new)))

#|

  (define (test t)
    (send t begin-edit-sequence)
    (send t end-edit-sequence)
    (send t end-edit-sequence))
  
  (test (new text%))
  (test (new (es-contract-mixin text%)))
  
Matthew writes:

> Underscore tends to mean "internal". Many variants of
> Insert(), for example, call the main _Insert() method.

So, double check the methods to make sure that a flag check
in an underscore method means the flag is checked in the
non-underscore methods.

At Sun, 29 Jun 2003 09:26:02 -0500, Robby Findler wrote:
> Is there some kind of invariant or ordering on these
> flags? That is, if a method only checks the flowLocked flag,
> is that effectively the same as checking the flowLocked flag
> or the writeLocked flag or something like that?

Yes: readLocked => flowLocked, and flowLocked => writeLocked.

Matthew

(define lock-contract-mixin
  (sequence-contract-mixin
   (state-machine
    [(and (locked-for-flow?)
          (locations-computed?))
     ;; everything except CheckRecalc
     ...]
    
    [(locked-for-flow?)
     ]
    [(locked-for-write?)
     ]
    [(locked-for-read?)
     ]
    [#t ;; unlocked
     
     ;; flowLocked in wx_mpriv
     set-position ; _SetPosition
     CheckRecalc  (only if graphicMaybeInvalid)
     set-autowrap-bitmap ; SetAutowrapBitmap
     Redraw
     BeginPrint
     EndPrint
     HasPrintPage
     print-to-dc ; PrintToDC
     
     ;; flowlocked in wx_media.cxx
     scroll-to-position ; ScrollToPosition
     move-position ; MovePosition
     split-snip ; SplitSnip
     ReallyCanEdit
     set-line-spacing ; SetLineSpacing
     set-max-width ; SetMaxWidth
     set-min-width ; SetMinWidth
     set-min-height ; SetMinHeight
     set-max-height ; SetMaxHeight
     set-tabs ; SetTabs
     scroll-to ; ScrollTo
     resized ; Resized ;; uses the flag, but not to abort
     
     ;; methods that consider
     ;; the readLocked variable,
     ;; at the C level; they just
     ;; return if it is set.
     ReallyCanEdit
     get-text ; GetText
     get-character ; GetCharacter
     find-wordbreak ; FindWorkbreak
     save-file ; SaveFile
     write-to-file ; WriteToFile
     _FindPositionInSnip
     find-position ; FindPosition
     scroll-line-location ; ScrollLineLocation
     num-scroll-lines ; NumScrollLines
     find-scroll-line ; FindScrollLine
     style-has-changed ; StyleHasChanged ;; maybe need to expand this to include style lists?
     FindFirstVisiblePosition
     FindLastVisiblePosition
     CheckRecalc
     
     ;; methods that consider the writeLocked variable,
     ;; at the C level
     _ChangeStyle
     _Insert
     _Delete
     ReallyCanEdit -- only when op != wxEDIT_COPY
     InsertPort
     read-from-file ; ReadFromFile
     set-style-list ; SetStyleList
     Recounted
     
     ;; in wx_mpbrd.cxx
     insert ; Insert
     delete ; Delete
     erase ; Erase
     delete ; Delete ;; -- with arg
     remove ; Remove
     move-to ; MoveTo
     move ; Move, also with arg
     change-style ; _ChangeStyle
     set-before ;SetBefore
     set-after ;SetAfter
     ;ReallyCanEdit -- only when op != wxEDIT_COPY
     ;Refresh has wierd code checking writeLocked -- what does < 0 mean?
     do-paste ; DoPaste
     paste ; Paste
     insert-port ; InsertPort
     insert-file ; InsertFile
     read-from-file ; ReadFromFile
     ; BeginEditSequence ;; -- wierd flag check
     ; EndEditSequence ;; -- wierd flag check, like BeginEditSequence
     
     ])
   (inherit locked-for-flow?
            locked-for-write?
            locked-for-read?)
   (super-new)))
  
  |#
  )
