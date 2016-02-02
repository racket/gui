#lang racket/base
(require racket/class
         racket/gui/base
         "arrow-toggle-snip.rkt")
(provide inherit-styles-editor-snip%
         expandable-snip%)

;; inherit-styles-editor-snip% propagates the style list of its enclosing
;; editor to its own editor.

(define inherit-styles-editor-snip%
  (class editor-snip%
    (inherit get-admin get-editor)
    (init-field [inherit-styles? #t])
    (super-new)
    (define/override (set-admin a)
      (super set-admin a)
      (define new-admin (get-admin))
      (when (and inherit-styles? new-admin)
        (define sl (send (send new-admin get-editor) get-style-list))
        (update-style-list sl)))
    ;; hook for additional action on style-list change:
    (define/public (update-style-list sl)
      (send (get-editor) set-style-list sl))))

;; ============================================================

;; An expandable snip consists of three parts: an arrow toggle snip,
;; the summary text, and the expanded text. The arrow toggles between
;; closed and open mode. In closed mode, only the toggle snip and
;; summary text are displayed adjacent on a single line. There are two
;; layout options for open mode:
;; - 'append:  The first line is unchanged, and the expanded text is
;;             displayed on the second line.
;; - 'replace: The summary text is replaced with the expanded text.

;; expandable-snip%
(define expandable-snip%
  (class inherit-styles-editor-snip%
    (inherit get-editor
             get-admin)
    (init [closed-editor (new text%)]
          [open-editor (new text%)]
          [callback void])
    (init-field [layout 'append]) ;; (U 'replace 'append)
    (super-new)

    (field [open? #f])

    (field [open-es (new editor-snip% (editor open-editor) (with-border? #f))])
    (send open-es set-margin 0 0 0 0)
    (send open-es set-inset 0 0 0 0)

    (field [closed-es (new editor-snip% (editor closed-editor) (with-border? #f))])
    (send closed-es set-margin 0 0 0 0)
    (send closed-es set-inset 0 0 0 0)

    (let ([outer-t (get-editor)])
      (define (toggle-callback now-open?)
        (unless (eq? now-open? open?)
          (set! open? now-open?)
          (refresh-contents)
          (callback now-open?)))
      (send* outer-t
        [insert (new arrow-toggle-snip% [callback toggle-callback])]
        [change-style top-aligned 0 (send outer-t last-position)]
        ;; Can't base-align; messes up with 'replace layout
        ;; [change-style base-aligned 0 1]
        [hide-caret #t]
        [lock #t]))

    (define/public (get-open-editor) (send open-es get-editor))
    (define/public (get-closed-editor) (send closed-es get-editor))

    (define/public (get-open-state) open?)
    (define/public (set-open-state v)
      (let ([v (and v #t)])
        (unless (eq? open? v)
          (set! open? v)
          (refresh-contents))))

    (define/override (update-style-list sl)
      (super update-style-list sl)
      (define outer-t (get-editor))
      (define standard (send sl find-named-style "Standard"))
      (with-unlock outer-t
        (send outer-t change-style standard 0 (send outer-t last-position))))

    ;; if layout is 'replace, editor contains
    ;;  - open? = #f : [turn-snip][closed-es]
    ;;  - open? = #t : [turn-snip][open-es]
    ;; if layout is 'append, editor contains
    ;;  - open? = #f : [turn-snip][closed-es]
    ;;  - open? = #t : [turn-snip][closed-es]\n[open-es]

    (define/private (refresh-contents)
      (define outer-t (get-editor))
      (with-unlock outer-t
        (send outer-t release-snip closed-es)
        (send outer-t release-snip open-es)
        (send outer-t delete 1 (send outer-t last-position))
        (when (or (not open?) (eq? layout 'append))
          (send outer-t insert closed-es (send outer-t last-position)))
        (when (and open? (eq? layout 'append))
          (send outer-t insert "\n" (send outer-t last-position)))
        (when open?
          (send outer-t insert open-es (send outer-t last-position)))
        (send outer-t change-style top-aligned 0 (send outer-t last-position))))

    (refresh-contents)
    ))

(define top-aligned (make-object style-delta% 'change-alignment 'top))
(define base-aligned (make-object style-delta% 'change-alignment 'base))

;; (with-unlock text-expression . body)
(define-syntax with-unlock
  (syntax-rules ()
    [(with-unlock text . body)
     (let* ([t text] [locked? (send t is-locked?)])
       (dynamic-wind
         (lambda ()
           (send* t
             (begin-edit-sequence #f)
             (lock #f)))
         (lambda () . body)
         (lambda ()
           (send* t
             (lock locked?)
             (end-edit-sequence)))))]))

;; ============================================================

(module+ main
  (provide (all-defined-out))
  (define f (new frame% (label "test") (height 400) (width 600)))
  (define t (new text%))
  (define ec (new editor-canvas% (editor t) (parent f)))
  (send f show #t)

  (send t insert "Here's what's I'm talking about,\na nice expandable snip: ")

  (define es (new expandable-snip% (with-border? #t) (layout 'append)))
  ;;(send es set-margin 0 0 0 0)

  (send* (send es get-closed-editor)
    [insert "alphabet"]
    ;; [hide-caret #t]
    [lock #t])

  (send* (send es get-open-editor)
    [insert "abcdefg\nhijklmno\npqrstuv\nwxyz"]
    ;; [hide-caret #t]
    [lock #t])

  (send t insert es)
  (send t hide-caret #t)
  (send t lock #t))
