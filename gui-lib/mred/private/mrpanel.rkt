#lang racket/base

(require racket/class
         (prefix-in wx: "kernel.rkt")
         "lock.rkt"
         "const.rkt"
         "check.rkt"
         "helper.rkt"
         "wx.rkt"
         "wxpanel.rkt"
         "mrwindow.rkt"
         "mrcontainer.rkt"
         "wxtabcanvas.rkt"
         "gdi.rkt")

(provide pane%
         vertical-pane%
         horizontal-pane%
         grow-box-spacer-pane%
         panel%
         vertical-panel%
         horizontal-panel%
         tab-panel%
         group-box-panel%)

(define-local-member-name
  get-initial-label
  as-plain-panel?
  finish-wx
  get-inner-getter
  get-outer-getter)

(define pane%
  (class (make-subarea% (make-container% area%))
    (init parent
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (define wx #f)
    (let* ([who (cond ; yuck! - we do this to make h-p and v-p subclasses of p
                 [(is-a? this vertical-pane%) 'vertical-pane]
                 [(is-a? this horizontal-pane%) 'horizontal-pane]
                 [(is-a? this grow-box-spacer-pane%) 'grow-box-spacer-pane]
                 [else 'pane])]
           [cwho `(constructor ,who)])
      (check-container-parent cwho parent)
      (as-entry
       (lambda ()
         (super-new
          [mk-wx
           (lambda ()
             (set! wx (make-object (case who
                                     [(vertical-pane) wx-vertical-pane%]
                                     [(horizontal-pane) wx-horizontal-pane%]
                                     [(grow-box-spacer-pane) wx-grow-box-pane%]
                                     [else wx-pane%])
                                   this this (mred->wx-container parent) null
                                   #f))
             wx)]
          [get-wx-pan (lambda () wx)]
          [get-outer-wx-pan (lambda () wx)]
          [mismatches
           (lambda ()
             (check-container-ready cwho parent))]
          [parent parent]
          [vert-margin vert-margin]
          [horiz-margin horiz-margin]
          [border border]
          [spacing spacing]
          [alignment alignment]
          [min-width min-width]
          [min-height min-height]
          [stretchable-width stretchable-width]
          [stretchable-height stretchable-height])
         (send (send wx area-parent) add-child wx)))
      (send parent after-new-child this))))

(define vertical-pane%
  (class pane%
    (init parent
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (super-new [parent parent]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [border border]
               [spacing spacing]
               [alignment alignment]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])))

(define horizontal-pane%
  (class pane%
    (init parent
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (super-new [parent parent]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [border border]
               [spacing spacing]
               [alignment alignment]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])))

(define grow-box-spacer-pane%
  (class pane%
    (init parent
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (super-new [parent parent]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [border border]
               [spacing spacing]
               [alignment alignment]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])))

(define panel%
  (class* (make-subwindow%
           (make-area-container-window%
            (make-window% #f (make-subarea% (make-container% area%)))))
    (subwindow<%>)
    (init parent [style null]
          ;; These additional init args are for the superclass
          ;; initializations and are needed to make sure the user can supply
          ;; these init args. They were originally handled by a class100 kw
          ;; macro. They're handed to super-instantiate below.
          [enabled #t]
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (define wx #f)
    (public* [get-initial-label (lambda () #f)]
             [as-plain-panel? (lambda () #f)]
             [finish-wx (lambda (proc) (proc this))]
             [get-inner-getter (lambda () (lambda () wx))]
             [get-outer-getter (lambda () (lambda () wx))])
    (let* ([who (cond ; yuck! - we do this to make h-p and v-p subclasses of p
                 [(is-a? this tab-panel%) 'tab-panel]
                 [(is-a? this group-box-panel%) 'group-box-panel]
                 [(is-a? this vertical-panel%) 'vertical-panel]
                 [(is-a? this horizontal-panel%) 'horizontal-panel]
                 [else 'panel])]
           [cwho `(constructor ,who)]
           [can-canvas? (memq who '(vertical-panel
                                    horizontal-panel
                                    panel))]
           [as-canvas? (lambda () (or (memq 'vscroll style)
                                      (memq 'auto-vscroll style)
                                      (memq 'hide-vscroll style)
                                      (memq 'hscroll style)
                                      (memq 'auto-hscroll style)
                                      (memq 'hide-hscroll style)))])
      (check-container-parent cwho parent)
      (check-style cwho #f (append '(border deleted)
                                   (cond
                                     [can-canvas?
                                      '(hscroll vscroll
                                                auto-hscroll auto-vscroll 
                                                hide-hscroll hide-vscroll)]
                                     [(eq? who 'tab-panel) '(can-reorder can-close flat-portable)]
                                     [else null]))
                   style)

      (define (add-scrolls style)
        (append
         (if (memq 'hide-vscroll style) 
             '(auto-vscroll)
             null)
         (if (memq 'hide-hscroll style) 
             '(auto-hscroll)
             null)
         style))

      (as-entry
       (lambda ()
         (super-instantiate
          ((lambda () (set! wx (finish-wx
                                (lambda (mred)
                                  (make-object (case who
                                                 [(vertical-panel)
                                                  (if (as-canvas?)
                                                      wx-vertical-canvas-panel%
                                                      wx-vertical-panel%)]
                                                 [(tab-panel)
                                                  (if (as-plain-panel?)
                                                      wx-vertical-panel%
                                                      wx-vertical-tab-panel%)]
                                                 [(group-box-panel) wx-vertical-group-panel%]
                                                 [(horizontal-panel)
                                                  (if (as-canvas?)
                                                      wx-horizontal-canvas-panel%
                                                      wx-horizontal-panel%)]
                                                 [else (if (as-canvas?)
                                                           wx-canvas-panel%
                                                           wx-panel%)])
                                               mred this (mred->wx-container parent)
                                               (cons 'transparent (add-scrolls style))
                                               (get-initial-label)))))
                   wx)
           (get-inner-getter)
           (get-outer-getter)
           (lambda () (check-container-ready cwho parent))
           #f parent #f)
          [enabled enabled]
          [vert-margin vert-margin]
          [horiz-margin horiz-margin]
          [border border]
          [spacing spacing]
          [alignment alignment]
          [min-width min-width]
          [min-height min-height]
          [stretchable-width stretchable-width]
          [stretchable-height stretchable-height])
         (unless (memq 'deleted style)
           (send (send wx area-parent) add-child wx))))
      (send parent after-new-child this))))

(define vertical-panel%
  (class panel%
    (init parent [style null] 
          [enabled #t]
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (super-new [parent parent]
               [style style]
               [enabled enabled]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [border border]
               [spacing spacing]
               [alignment alignment]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])
    (public* [set-orientation (λ (x) (send (mred->wx this) set-orientation x))]
             [get-orientation (λ () (send (mred->wx this) get-orientation))])))
(define horizontal-panel%
  (class panel%
    (init parent [style null] 
          [enabled #t]
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (super-new [parent parent]
               [style style]
               [enabled enabled]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [border border]
               [spacing spacing]
               [alignment alignment]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])
    (public* [set-orientation (λ (x) (send (mred->wx this) set-orientation x))]
             [get-orientation (λ () (send (mred->wx this) get-orientation))])))

(define list-append append)
(define always-use-tab-canvas? (and (getenv "PLT_FLAT_PORTABLE_TAB_PANEL") #t))

(define tab-panel%
  (class vertical-panel%
    (init choices parent [callback (lambda (b e) (void))])
    (init-field [style null])
    (init [font no-val]
          ;; inherited inits
          [enabled #t]
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (define save-choices choices)
    (override* [get-initial-label (lambda () save-choices)])

    (define use-tab-canvas? (and (memq 'no-border style)
                                 (or always-use-tab-canvas?
                                     (memq 'flat-portable style)
                                     (and (or (memq 'can-reorder style)
                                              (memq 'can-close style))
                                          (eq? 'windows (system-type))))))

    (define tab-canvas #f)
    (define outside #f)
    (define inside #f)
    (define init-font font)
    
    (define/override (as-plain-panel?) use-tab-canvas?)
    (define/override (finish-wx proc)
      (define wx (proc (if use-tab-canvas? #f this)))
      (set! outside wx)
      (cond
        [use-tab-canvas?
         (set! tab-canvas (make-object wx-tab-canvas%
                                       save-choices
                                       style
                                       (if (eq? init-font no-val)
                                           normal-control-font
                                           init-font)
                                       (lambda (index) (on-close-request index))
                                       (lambda (new-positions) (on-reorder new-positions))
                                       this this
                                       wx
                                       -1 -1
                                       0 0
                                       '(no-focus transparent)
                                       #f))
         (send tab-canvas skip-enter-leave-events #t)
         (set! inside (make-object wx-vertical-panel%
                                   this this
                                   wx
                                   '()
                                   #f))
         (send inside skip-subwindow-events? #t)
         (send inside skip-enter-leave-events #t)
         (send (send inside area-parent) add-child inside)
         (send wx set-container inside)
         (send tab-canvas set-sibling-client inside)]
        [else
         (set! inside wx)])
      wx)
    (define/override (get-inner-getter) (lambda () inside))
    (define/override (get-outer-getter) (lambda () outside))

    ;; Between the time that tabs have been rearranged by dragging and a notification
    ;; is been delivered via `on-reorder`, pretend that things are still in the old
    ;; order by setting this list to a non-#f value:
    (define external-mapping #f)
    (define/private (external->internal i)
      (if external-mapping
          (for/first ([ext-i (in-list external-mapping)]
                      [actual-i (in-naturals)]
                      #:when (= ext-i i))
            actual-i)
          i))
    (define/private (internal->external i)
      (if external-mapping
          (list-ref external-mapping i)
          i))

    (let ([cwho '(constructor tab-panel)])
      (unless (and (list? choices) (andmap label-string? choices))
        (raise-argument-error (who->name cwho) "label-string?" choices))
      (check-callback cwho callback)
      (check-container-parent cwho parent)
      (check-style cwho #f '(deleted no-border can-reorder can-close flat-portable) style)
      (check-font cwho font))
    (super-new [parent parent]
               [style (if (memq 'no-border style)
                          (remq 'no-border style)
                          (cons 'border style))]
               [enabled enabled]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [border border]
               [spacing spacing]
               [alignment alignment]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])
    
    (define/private (get-tab-widget)
      (or tab-canvas (mred->wx this)))

    (send (get-tab-widget) set-callback (lambda (wx e) (callback (wx->mred wx) e)))
    
    (public*
     [get-number (lambda () (length save-choices))]
     [append (entry-point
              (lambda (n)
                (check-label-string '(method tab-panel% append) n)
                (let ([n (string->immutable-string n)])
                  (set! save-choices (list-append save-choices (list n)))
                  (when external-mapping
                    (set! external-mapping (append external-mapping (length external-mapping))))
                  (send (get-tab-widget) append n))))]
     [get-selection (lambda () (and (pair? save-choices)
                                    (internal->external (send (get-tab-widget) get-selection))))]
     [set-selection (entry-point
                     (lambda (i)
                       (check-item 'set-selection i)
                       (send (get-tab-widget) set-selection (external->internal i))))]
     [delete (entry-point
              (lambda (i)
                (check-item 'delete i)
                (let ([i (external->internal i)])
                  (set! save-choices (let loop ([p 0] [l save-choices])
                                       (if (= p i)
                                           (cdr l)
                                           (cons (car l) (loop (add1 p) (cdr l))))))
                  (send (get-tab-widget) delete i))))]
     [set-item-label (entry-point
                      (lambda (i s)
                        (check-item 'set-item-label i)
                        (check-label-string '(method tab-panel% set-item-label) s)
                        (let ([s (string->immutable-string s)])
                          (set! save-choices (let loop ([save-choices save-choices][i i])
                                               (if (zero? i)
                                                   (cons s (cdr save-choices))
                                                   (cons (car save-choices) (loop (cdr save-choices) (sub1 i))))))
                          (send (get-tab-widget) set-label i s))))]
     [set
      (entry-point (lambda (l)
                     (unless (and (list? l) (andmap label-string? l))
                       (raise-argument-error (who->name '(method tab-panel% set))
                                             "(listof label-string?)" l))
                     (set! save-choices (map string->immutable-string l))
                     (set! external-mapping #f)
                     (send (get-tab-widget) set l)))]
     [get-item-label (entry-point
                      (lambda (i)
                        (check-item 'get-item-label i)
                        (let ([i (external->internal i)])
                          (list-ref save-choices i))))]

     [do-on-choice-reorder (lambda (new-positions)
                             ;; in atomic mode when we get here
                             (set! save-choices (for/list ([i (in-list new-positions)])
                                                  (list-ref save-choices i)))
                             (set! external-mapping (for/list ([old-internal (in-list new-positions)])
                                                      (internal->external old-internal)))
                             (wx:queue-callback (lambda ()
                                                  (set! external-mapping #f)
                                                  (on-reorder new-positions))
                                                wx:middle-queue-key))]
     [on-close-request (lambda (which) (void))])
    (pubment*
     [on-reorder (lambda (new-positions) (inner (void) on-reorder new-positions))])

    (private*
     [check-item
      (lambda (method n)
        (check-non-negative-integer `(method tab-panel% ,method) n)
        (let ([m (length save-choices)])
          (unless (< n m)
            (raise-range-error (who->name `(method tab-panel% ,method))
                               "panel" "tab "
                               n
                               this
                               0
                               (sub1 m)
                               #f))))])))

(define group-box-panel%
  (class vertical-panel%
    (init label parent [style null] [font no-val]
          ;; This is a vestige of the class100 keyword handling macro
          ;; that was used. Since `horiz-margin` and `vert-margin` are
          ;; used below, we have to supply it here (even though it's
          ;; handled by the subarea init args)
          [enabled #t]
          [vert-margin no-val]
          [horiz-margin no-val]
          [border no-val]
          [spacing no-val]
          [alignment no-val]
          [min-width no-val]
          [min-height no-val]
          [stretchable-width no-val]
          [stretchable-height no-val])
    (init-rest)
    (define lbl label)
    (override* [get-initial-label (lambda () lbl)])

    (let ([cwho '(constructor group-box-panel)])
      (check-label-string cwho label)
      (check-container-parent cwho parent)
      (check-style cwho #f '(deleted) style)
      (check-font cwho font))

    ;; Technically a bad way to change margin defaults, since it's
    ;;  implemented with an update after creation:
    (when (eq? horiz-margin no-val) (set! horiz-margin 2))
    (when (eq? vert-margin no-val) (set! vert-margin 2))

    (super-instantiate
     (parent
      (if (memq 'deleted style)
          '(deleted)
          null))
     [enabled enabled]
     [horiz-margin horiz-margin]
     [vert-margin vert-margin]
     [border border]
     [spacing spacing]
     [alignment alignment]
     [min-width min-width]
     [min-height min-height]
     [stretchable-width stretchable-width]
     [stretchable-height stretchable-height])

    (override*
     [set-label (entry-point
                 (lambda (s)
                   (check-label-string '(method group-box-panel% set-label) s)
                   (set! lbl (if (immutable? s)
                                 s
                                 (string->immutable-string s)))
                   (send (mred->wx this) set-label s)))]
     [get-label (lambda () lbl)])))
