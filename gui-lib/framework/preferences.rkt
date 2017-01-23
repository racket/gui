#lang at-exp racket/base
#|

There are three attributes for each preference:

  - default set, or not
  - marshalling function set, or not
  - initialization still okay, or not

the state transitions / contracts are:

  get(true, _, _) -> (true, _, false)
  get(false, _, _) -> error default not yet set

  set is just like get.

  set-default(false, _, true) -> set-default(true, _, true)
  set-default(true, _, _) -> error default already set
  set-default(_, _, false) -> initialization not okay anymore  /* cannot happen, I think */

  set-un/marshall(true, false, true) -> (true, true, true)
  .. otherwise error

  for all syms:
   prefs-snapshot(_, _, _) -> (_, _, false)

|#

(require scribble/srcdoc
         racket/contract/base racket/file)
(require/doc racket/base
             scribble/manual
             scribble/example
             (for-label racket/serialize))

(define-struct (exn:unknown-preference exn) ())

;; these two names are for consistency
(define exn:make-unknown-preference make-exn:unknown-preference)
(define exn:struct:unknown-preference struct:exn:unknown-preference)

(define preferences:low-level-put-preferences (make-parameter put-preferences))
(define preferences:low-level-get-preference  (make-parameter get-preference))

(define (add-pref-prefix p) (string->symbol (format "plt:framework-pref:~a" p)))

;; preferences : hash-table[sym -o> any]
;;   the current values of the preferences
;; marshall-unmarshall : sym -o> un/marshall
;; callbacks : sym -o> (listof (sym TST -> boolean))
;; defaults : hash-table[sym -o> default]
(struct preferences:layer (preferences marshall-unmarshall callbacks defaults prev))

(define (preferences:new-layer prev)
  (preferences:layer (make-hasheq) (make-hasheq) (make-hasheq) (make-hasheq) prev))
(define preferences:current-layer (make-parameter (preferences:new-layer #f)))

(define (find-layer pref)
  (let loop ([pref-state (preferences:current-layer)])
    (and pref-state
         (cond
           [(hash-has-key? (preferences:layer-defaults pref-state) pref)
            pref-state]
           [(hash-has-key? (preferences:layer-callbacks pref-state) pref)
            pref-state]
           [else
            (loop (preferences:layer-prev pref-state))]))))

(define (preferences:default-set? pref)
  (define layer (find-layer pref))
  (and layer
       (hash-has-key? (preferences:layer-defaults layer) pref)))

;; type un/marshall = (make-un/marshall (any -> prinable) (printable -> any))
(define-struct un/marshall (marshall unmarshall))

;; type pref = (make-pref any)
(define-struct pref (value))

;; type default  = (make-default any (-> any bool) (listof symbol) (listof (-> any any)))
(define-struct default (value checker aliases rewrite-aliases))

;; pref-callback : (make-pref-callback (union (weak-box (sym tst -> void)) (sym tst -> void)))
;; this is used as a wrapped to deal with the problem that different procedures might be eq?.
(define-struct pref-callback (cb) #:transparent)

;; used to detect missing hash entries
(define none (gensym 'none))

;; get : symbol -> any
;; return the current value of the preference `p'
;; exported
(define (preferences:get p)
  (define pref-state (find-layer p))
  (when (or (not pref-state)
            (not (hash-has-key? (preferences:layer-defaults pref-state) p)))
    (raise-unknown-preference-error
     'preferences:get
     "tried to get a preference but no default set for ~e"
     p))
  (define preferences (preferences:layer-preferences pref-state))
  (define v (hash-ref preferences p none))
  (cond
    ;; first time reading this, check the file & unmarshall value, if
    ;; it's not there, use the default
    [(eq? v none)
     (define defaults (preferences:layer-defaults pref-state))
     ;; try to read the preference from the preferences file
     (define marshalled-v (read-pref-from-file (hash-ref defaults p) p))
     (define default-info (hash-ref defaults p))
     (define the-default-value (default-value default-info))
     (define v (if (eq? marshalled-v none)
                   ;; no value read, take the default value
                   the-default-value
                   ;; found a saved value, unmarshall it
                   (unmarshall-pref pref-state p marshalled-v
                                    (default-checker default-info)
                                    the-default-value)))
     ;; set the value in the preferences table for easier reference
     ;; and so we know it has been read from the disk
     ;; (and thus setting the marshaller after this is no good)
     (hash-set! preferences p v)
     v]
    ;; oth. it is found, so we can just return it
    [else v]))

;; read-pref-from-file : symbol -> (or/c any none)
;; reads the preference saved in the low-level preferences
;; file, first checking 'p' and then checking the aliases (in order)
(define (read-pref-from-file defaults p)
  (let loop ([syms (cons p (default-aliases defaults))]
             [rewriters (cons values (default-rewrite-aliases defaults))])
    (cond
      [(null? syms) none]
      [else
       (let/ec k
         ((car rewriters)
          ((preferences:low-level-get-preference)
           (add-pref-prefix (car syms))
           (lambda () (k (loop (cdr syms) (cdr rewriters)))))))])))

;; set : symbol any -> void
;; updates the preference
;; exported
(define (preferences:set p value) (multi-set (list p) (list value)))

;; set : symbol any -> void
;; updates the preference
;; exported
(define (multi-set ps values)
  (dynamic-wind
   (λ ()
     (call-pref-save-callbacks #t))
   (λ ()
     (for ([p (in-list ps)]
           [value (in-list values)])
       (define pref-state (find-layer p))
       (cond
         [pref-state
          (define default (hash-ref (preferences:layer-defaults pref-state) p))
          (define checker? (default-checker default))
          (unless (checker? value)
            (error 'preferences:set
                   (string-append
                    "new value doesn't satisfy preferences:set-default predicate\n"
                    "  pref symbol: ~e\n"
                    "  given: ~e\n"
                    "  predicate: ~e")
                   p value checker?))
          (check-callbacks pref-state p value)
          (hash-set! (preferences:layer-preferences pref-state) p value)]
         [else
          (raise-unknown-preference-error
           'preferences:set
           (string-append
            "cannot set preference before setting default"
            "  pref symbol: ~e\n"
            "  given: ~e")
           p
           value)]))
     ((preferences:low-level-put-preferences)
      (map add-pref-prefix ps)
      (for/list ([p (in-list ps)]
                 [value (in-list values)])
        (marshall-pref p value)))
     (void))
   (λ ()
     (call-pref-save-callbacks #f))))

(define pref-save-callbacks '())

(define (preferences:get/set sym)
  (case-lambda
    [() (preferences:get sym)]
    [(v) (preferences:set sym v)]))

(define (preferences:register-save-callback f)
  (define key (gensym))
  (set! pref-save-callbacks (cons (list key f) pref-save-callbacks))
  key)

(define (preferences:unregister-save-callback k)
  (set! pref-save-callbacks
        (let loop ([callbacks pref-save-callbacks])
          (cond
            [(null? callbacks) '()]
            [else
             (let ([cb (car callbacks)])
               (if (eq? (list-ref cb 0) k)
                   (cdr callbacks)
                   (cons cb (loop (cdr callbacks)))))]))))

(define (call-pref-save-callbacks b)
  (for ([cb (in-list pref-save-callbacks)])
    ((list-ref cb 1) b)))

(define (raise-unknown-preference-error sym fmt . args)
  (raise (exn:make-unknown-preference
          (string-append (format "~a: " sym) (apply format fmt args))
          (current-continuation-marks))))

;; add-callback : sym (-> void) -> void
(define (preferences:add-callback p callback [weak? #f])
  (define pref-state (or (find-layer p) (preferences:current-layer)))
  (define callbacks (preferences:layer-callbacks pref-state))
  (define new-cb
    (make-pref-callback (if weak?
                            (impersonator-ephemeron callback)
                            callback)))
  (hash-set! callbacks
             p
             (append
              (hash-ref callbacks p '())
              (list new-cb)))
  (λ ()
    (hash-set!
     callbacks
     p
     (let loop ([callbacks (hash-ref callbacks p '())])
       (cond
         [(null? callbacks) '()]
         [else
          (let ([callback (car callbacks)])
            (cond
              [(eq? callback new-cb)
               (loop (cdr callbacks))]
              [else
               (cons (car callbacks) (loop (cdr callbacks)))]))])))))

;; check-callbacks : pref-state sym val -> void
(define (check-callbacks pref-state p value)
  (define callbacks (preferences:layer-callbacks pref-state))
  (define new-callbacks
    (let loop ([callbacks (hash-ref callbacks p '())])
      (cond
        [(null? callbacks) null]
        [else
         (define callback (car callbacks))
         (define cb (pref-callback-cb callback))
         (cond
           [(ephemeron? cb)
            (define v (ephemeron-value cb))
            (cond
              [v
               (v p value)
               (cons callback (loop (cdr callbacks)))]
              [else
               (loop (cdr callbacks))])]
           [else
            (cb p value)
            (cons callback (loop (cdr callbacks)))])])))
  (if (null? new-callbacks)
      (hash-remove! callbacks p)
      (hash-set! callbacks p new-callbacks)))

(define (preferences:set-un/marshall p marshall unmarshall)
  (define pref-state (find-layer p))
  (cond
    [pref-state
     (define marshall-unmarshall (preferences:layer-marshall-unmarshall pref-state))
     (define pref-un/marshall-set? (hash-ref marshall-unmarshall p #f))
     (define pref-can-init? (not (hash-has-key? (preferences:layer-preferences pref-state) p)))
     (cond
       [(and (not pref-un/marshall-set?) pref-can-init?)
        (hash-set! marshall-unmarshall p (make-un/marshall marshall unmarshall))]
       [pref-un/marshall-set?
        (error 'preferences:set-un/marshall
               "already set un/marshall for ~e"
               p)]
       [(not pref-can-init?)
        (error 'preferences:set-un/marshall "the preference ~e cannot be configured any more" p)])]
    [else
     (error 'preferences:set-un/marshall
            "must call preferences:set-default for ~s before calling set-un/marshall for ~s"
            p p)]))

;; set-default : (sym TST (TST -> boolean) -> void
(define (preferences:set-default p default-value checker
                                 #:aliases [aliases '()]
                                 #:rewrite-aliases [rewrite-aliases (map (λ (x) values) aliases)])
  (define pref-state (or (find-layer p) (preferences:current-layer)))
  (define defaults (preferences:layer-defaults pref-state))
  (when (hash-has-key? defaults p)
    (error 'preferences:set-default
           (string-append
            "preferences default already set\n"
            "  pref symbol: ~e\n"
            "  default: ~e\n"
            "  checker: ~e")
           p default-value checker))
  (unless (checker default-value)
    (error 'preferences:set-default
           (string-append
            "checker doesn't match default\n"
            "  pref symbol: ~e\n"
            "  default: ~e\n"
            "  checker: ~e")
           p default-value checker))
  (unless (= (length aliases) (length rewrite-aliases))
    (error 'preferences:set-default
           (string-append
            "expected equal length lists for the #:aliases"
            " and #:rewrite-aliases arguments, got ~e and ~e")
           aliases rewrite-aliases))
  (hash-set! defaults p (make-default default-value checker aliases rewrite-aliases)))

;; marshall-pref : symbol any -> (list symbol printable)
(define (marshall-pref p value)
  (define pref-state (find-layer p))
  (let/ec k
    (define marshaller
      (un/marshall-marshall
       (hash-ref (preferences:layer-marshall-unmarshall pref-state)
                 p
                 (λ () (k value)))))
    (marshaller value)))

;; unmarshall-pref : pref-state symbol marshalled (any -> bool) any -> any
;; unmarshalls a preference read from the disk
(define (unmarshall-pref pref-state p data the-checker the-default-value)
  (define marshall-unmarshall (preferences:layer-marshall-unmarshall pref-state))
  (define un/marshall (hash-ref marshall-unmarshall p #f))
  (define result
    (if un/marshall
        ((un/marshall-unmarshall un/marshall) data)
        data))
  (if (the-checker result)
      result
      the-default-value))

;; copy-pref-value : sym any -> any
;; uses the marshalling code to copy a preference. If there
;; is not marshaller set, then no copying happens.
(define (copy-pref-value p value)
  (let/ec k
    (define pref-state (find-layer p))
    (define marshall-unmarshall (preferences:layer-marshall-unmarshall pref-state))
    (define un/marshaller (hash-ref marshall-unmarshall p (λ () (k value))))
    (define default (hash-ref (preferences:layer-defaults pref-state) p))
    (define marsh (un/marshall-marshall un/marshaller))
    (define unmarsh (un/marshall-unmarshall un/marshaller))
    (define marshalled (marsh value))
    (define copy (unmarsh marshalled))
    (if ((default-checker default) copy)
        copy
        value)))

(define (preferences:restore-defaults)
  (let loop ([prefs-state (preferences:current-layer)])
    (when prefs-state
      (for ([(p def) (in-hash (preferences:layer-defaults prefs-state))])
        (preferences:set p (default-value def)))
      (loop (preferences:layer-prev prefs-state)))))

(define-struct preferences:snapshot (x))
(define (preferences:get-prefs-snapshot)
  (make-preferences:snapshot
   (let loop ([prefs-state (preferences:current-layer)]
              [sofar '()])
     (cond
       [prefs-state
        (loop (preferences:layer-prev prefs-state)
              (for/fold ([sofar sofar])
                        ([(k def) (in-hash (preferences:layer-defaults prefs-state))])
                (cons (cons k (copy-pref-value k (preferences:get k)))
                      sofar)))]
       [else sofar]))))

(define (preferences:restore-prefs-snapshot snapshot)
  (multi-set (map car (preferences:snapshot-x snapshot))
             (map cdr (preferences:snapshot-x snapshot)))
  (void))

(begin-for-doc
  (define pref-layer-eval (make-base-eval))
  (pref-layer-eval
   '(begin
      (require framework/preferences)
      (let ([the-prefs-table (make-hash)])
        (preferences:low-level-put-preferences
         (λ (syms vals)
           (for ([sym (in-list syms)]
                 [val (in-list vals)])
             (hash-set! the-prefs-table sym val))))
        (preferences:low-level-get-preference
         (λ (sym [fail void])
           (hash-ref the-prefs-table sym fail)))))))

(provide/doc
 (proc-doc/names
  preferences:get
  (symbol? . -> . any/c)
  (symbol)
  @{See also @racket[preferences:set-default].

        @racket[preferences:get] returns the value for the preference
        @racket[symbol]. It raises an exception matching
        @racket[exn:unknown-preference?]
        if the preference's default has not been set.})

 (proc-doc/names
  preferences:set
  (symbol? any/c . -> . void?)
  (symbol value)
  @{Sets the preference
    @racket[symbol] to @racket[value]. It should be called when the
    user requests a change to a preference.

    @racket[preferences:set] immediately writes the preference value to disk.
    It raises an exception matching
    @racket[exn:unknown-preference?]
    if the preference's default has not been set

    See also @racket[preferences:set-default].})

 (proc-doc/names
  preferences:get/set
  (-> symbol? (case-> (-> any/c) (-> any/c void?)))
  (pref)
  @{Returns a procedure that when applied to zero arguments retrieves the
    current value of the preference named @racket[pref] and when
    applied to one argument updates the preference named @racket[pref].

    @history[#:added "1.18"]{}})

 (proc-doc/names
  preferences:add-callback
  (->* (symbol? (-> symbol? any/c any))
       (boolean?)
       (-> void?))
  ((p f)
   ((weak? #f)))
  @{This function adds a callback which is called with a symbol naming a
    preference and its value, when the preference changes.
    @racket[preferences:add-callback] returns a thunk, which when
    invoked, removes the callback from this preference.

    If @racket[weak?] is true, the preferences system will only hold on to
    the callback
    @tech[#:key "weak references"
          #:doc '(lib "scribblings/reference/reference.scrbl")]{weakly}.

    The callbacks will be called in the order in which they were added.

    If you are adding a callback for a preference that requires
    marshalling and unmarshalling, you must set the marshalling and
    unmarshalling functions by calling
    @racket[preferences:set-un/marshall] before adding a callback.

    The result thunk removes the callback from the same @tech{preferences layer}
    that @racket[p] was in when @racket[preferences:add-callback] was
    originally called.

    This function raises an exception matching
    @racket[exn:unknown-preference?]
    if the preference default has not been set via
    @racket[preferences:set-default].})
 (proc-doc/names
  preferences:set-default
  (->* (symbol? any/c (any/c . -> . any))
       (#:aliases (listof symbol?)
        #:rewrite-aliases (listof (-> any/c any)))
      void?)
  ((symbol value test)
   ((aliases '()) (rewrite-aliases (map (lambda (x) values) aliases))))
  @{This function must be called every time your application starts up, before
    any call to @racket[preferences:get] or @racket[preferences:set]
    (for any given preference).

    If you use @racket[preferences:set-un/marshall],
    you must call this function before calling it.

    This sets the default value of the preference @racket[symbol] to
    @racket[value]. If the user has chosen a different setting,
    (reflected via a call to @racket[preferences:set], possibly
    in a different run of your program),
    the user's setting will take precedence over the default value.

    The @racket[test] argument is used as a safeguard. That function is
    called to determine if a preference read in from a file is a valid
    preference. If @racket[test] returns @racket[#t], then the preference is
    treated as valid. If @racket[test] returns @racket[#f] then the default is
    used.

    The @racket[aliases] and @racket[rewrite-aliases] arguments aids
    in renaming preferences. If @racket[aliases] is present, it is
    expected to be a list of symbols that correspond to old versions
    of the preferences. It defaults to @racket['()]. If @racket[rewrite-aliases]
    is present, it is used to adjust the old values of the preferences
    when they are present in the saved file.

    @history[#:changed "1.23" @list{Allow @racket[preferences:set-default]
               to be called even after a snapshot has been grabbed.}]
 })

 (proc-doc/names
  preferences:default-set?
  (-> symbol? boolean?)
  (pref)
  @{Returns @racket[#t] if @racket[pref] has been passed to
            @racket[preferences:set-default], @racket[#f]
            otherwise})

 (proc-doc/names
  preferences:set-un/marshall
  (symbol? (any/c . -> . printable/c) (printable/c . -> . any/c) . -> . void?)
  (symbol marshall unmarshall)
  @{@racket[preferences:set-un/marshall] is used to specify marshalling and
    unmarshalling functions for the preference
    @racket[symbol]. @racket[marshall] will be called when the users saves their
    preferences to turn the preference value for @racket[symbol] into a
    printable value. @racket[unmarshall] will be called when the user's
    preferences are read from the file to transform the printable value
    into its internal representation. If @racket[preferences:set-un/marshall]
    is never called for a particular preference, the values of that
    preference are assumed to be printable.

    If the unmarshalling function returns a value that does not meet the
    guard passed to @racket[preferences:set-default]
    for this preference, the default value is used.

    The @racket[marshall] function might be called with any value returned
    from @racket[read] and it must not raise an error
    (although it can return arbitrary results if it gets bad input). This might
    happen when the preferences file becomes corrupted, or is edited
    by hand.

    @racket[preferences:set-un/marshall] must be called before calling
    @racket[preferences:get],@racket[preferences:set].

    See also @racket[serialize] and @racket[deserialize].
   })

 (proc-doc/names
  preferences:restore-defaults
  (-> void?)
  ()
  @{@racket[(preferences:restore-defaults)] restores the users' configuration
    to the default preferences.})

 (proc-doc/names
  preferences:register-save-callback
  (-> (-> boolean? any) symbol?)
  (callback)
  @{Registers @racket[callback] to run twice for each call
 to @racket[preferences:set]---once before the preferences
 file is written, with @racket[#t], and once after it is
 written, with @racket[#f]. Registration returns a key for
 use with @racket[preferences:unregister-save-callback].
 Caveats: @itemize{
  @item{The callback occurs on whichever
   thread happened to call @racket[preferences:set].
  }
  @item{
   Pre- and post-write notifications are not necessarily
   paired; unregistration may cancel the post-write
   notification before it occurs.}}})

 (proc-doc/names
  preferences:unregister-save-callback
  (-> symbol? void?)
  (key)
  @{Unregisters the save callback associated with @racket[key].})

 (proc-doc/names
  exn:make-unknown-preference
  (string? continuation-mark-set? . -> . exn:unknown-preference?)
  (message continuation-marks)
  @{Creates an unknown preference exception.})

 (proc-doc/names
  exn:unknown-preference?
  (any/c . -> . boolean?)
  (exn)
  @{Determines if a value is an unknown preference exn.})

 (thing-doc
  exn:struct:unknown-preference
  struct-type?
  @{The struct type for the unknown preference exn.})


 (parameter-doc
  preferences:low-level-put-preferences
  (parameter/c (-> (listof symbol?) (listof any/c) any))
  put-preferences
  @{This parameter's value is called to save preference the preferences file.
    Its interface should be just like mzlib's @racket[put-preferences].

    The default value calls @racket[put-preferences] and, if there is an error,
    then starts using a hash-table to save the preferences instead.
    See also @racket[]})

 (parameter-doc
  preferences:low-level-get-preference
  (parameter/c (->* (symbol?) [(-> any)] any))
  get-preference
  @{This parameter's value is called to get a preference from the preferences
    file. Its interface should be just like @racket[get-preference].

    The default value calls @racket[get-preferences] and, if there is an error,
    then starts using a hash-table to save the preferences instead.})

 (proc-doc/names
  preferences:snapshot?
  (-> any/c boolean?)
  (arg)
  @{Determines if its argument is a preferences snapshot.

    See also @racket[preferences:get-prefs-snapshot] and
    @racket[preferences:restore-prefs-snapshot].})
 (proc-doc/names
  preferences:restore-prefs-snapshot
  (-> preferences:snapshot? void?)
  (snapshot)
  @{Restores the preferences saved in @racket[snapshot], updating
    all of the preferences values to the ones they had at the time
    that @racket[preferences:get-prefs-snapshot] was called.

    See also @racket[preferences:get-prefs-snapshot].})

 (proc-doc/names
  preferences:get-prefs-snapshot
  (-> preferences:snapshot?)
  ()
  @{Caches all of the current values of the known preferences and returns them.
    For any preference that has marshalling and unmarshalling set
    (see @racket[preferences:set-un/marshall]), the preference value is
    copied by passing it through the marshalling and unmarshalling process.
    Other values are not copied, but references to them are instead saved.

    See also @racket[preferences:restore-prefs-snapshot].})

 (proc-doc/names
  preferences:new-layer
  (-> (or/c #f preferences:layer?) preferences:layer?)
  (previous-preferences-layer)
  @{Creates a @tech{preferences layer} that extends @racket[previous-preferences-layer].

  @history[#:added "1.30"]})

 (proc-doc/names
  preferences:layer?
  (-> any/c boolean?)
  (v)
  @{Determines if @racket[v] is a @deftech{preferences layer}.

  A preferences layer gives a form of scoping to preferences. When
  a new preference is first registered with this library (via a call to
  @racket[preferences:set-default] or @racket[preferences:add-callback])
  it is put into the layer in @racket[preferences:current-layer]
  (and not into any of that layer's previous layers).
  When @racket[preferences:get], @racket[preferences:set],
  @racket[preferences:set-un/marshall] are called, they consult and
  manipulate only the layer where the preference was first installed.
  Accordingly, preference layers give a way to discard some set of
  calls to @racket[preference:set-default] and other preference configuration
  and to start over with a new set. Note that this affects only the configuration
  of the preferences for the library; the values are all stored centrally
  (see @racket[preferences:low-level-put-preferences]) and are unaffected
  by the layers.

  @examples[#:eval pref-layer-eval

            (define original-layer (preferences:current-layer))

            (define layer2 (preferences:new-layer original-layer))
            (parameterize ([preferences:current-layer layer2])
              (code:comment "initialize 'a-pref in layer2")
              (preferences:set-default 'a-pref 5 real?)
              (preferences:set 'a-pref 6)
              (preferences:get 'a-pref))

            (define layer3 (preferences:new-layer original-layer))
            (parameterize ([preferences:current-layer layer3])
              (code:comment "initialize 'a-pref again, this time in layer3")
              (code:comment "without the new layer in place, this would be an error")
              (preferences:set-default 'a-pref 5 real?)
              (code:comment "the actual value of the preference remains")
              (code:comment "from the previous call to preferences:set")
              (preferences:get 'a-pref))]

  @history[#:added "1.30"]
  })

 (parameter-doc
  preferences:current-layer
  (parameter/c preferences:layer?)
  preferences-layer
  @{Determines the current @tech{preferences layer}.
   @history[#:added "1.30"]})
 )
