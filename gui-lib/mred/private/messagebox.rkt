#lang racket/base

(require racket/class
         racket/string
         racket/snip/private/style
         racket/draw
         (prefix-in wx: "kernel.rkt")
         "const.rkt"
         "check.rkt"
         "helper.rkt"
         "editor.rkt"
         "mrtop.rkt"
         "mrcanvas.rkt"
         "mrpopup.rkt"
         "mrmenu.rkt"
         "mritem.rkt"
         "mrpanel.rkt"
         "panel-wob.rkt")

(provide message-box
         message-box/custom
         message+check-box
         message+check-box/custom)

(define do-message-box/custom
  (lambda (who title message
               button1 button2 button3
               parent style close-result
               check? two-results? check-message
               return-the-dialog? dialog-mixin)
    (check-label-string who title)
    (check-string/false who message)
    (when check?
      (check-label-string who check-message))
    (check-label-string-or-bitmap/false who button1)
    (check-label-string-or-bitmap/false who button2)
    (check-label-string-or-bitmap/false who button3)
    (check-top-level-parent/false who parent)
    (check-style who
                 '(default=1 default=2 default=3 no-default)
                 (let ([l '(disallow-close number-order caution stop no-icon)])
                   (if check?
                       (cons 'checked l)
                       l))
                 style)

    (let ([go (lambda ()
                (create-message-box/custom
                 who title message
                 button1 button2 button3
                 parent style close-result
                 check? two-results? check-message
                 return-the-dialog? dialog-mixin))]
          [es (if parent
                  (send parent get-eventspace)
                  (wx:current-eventspace))])
      (if (eq? (current-thread) (wx:eventspace-handler-thread es))
          ;; In the right thread:
          (go)
          ;; Not in the right thread:
          (let ([ch (make-channel)])
            (parameterize ([wx:current-eventspace es])
              (wx:queue-callback
               (lambda ()
                 (channel-put ch (call-with-values go list)))))
            (apply values (channel-get ch)))))))

(define (create-message-box/custom who title message
                                   button1 button2 button3
                                   parent style close-result
                                   check? two-results? check-message
                                   return-the-dialog? dialog-mixin)
  (define result close-result)
  (define message-box/custom-dialog%
    (class dialog%
      (inherit show)
      (define/public (show-and-return-results)
        (show #t)
        (if two-results?
            (values result (and check? (send check-box get-value)))
            result))
      (define/public (set-message new-message)
        (set! message new-message)
        (send dlg begin-container-sequence)
        (fill-message-box-message check? msg-pnl check-parent-panel msg-h-align msg-v-align dlg message icon-msg)
        (send dlg end-container-sequence))
      (define/public (get-message) message)
      (augment*
       [can-close? (lambda ()
                     (if (memq 'disallow-close style)
                         (begin
                           (wx:bell)
                           #f)
                         #t))])
      (override*
       [on-subwindow-event
        (lambda (w e)
          (if (send e button-down?)
              (if (is-a? w button%)
                  #f
                  (if (or (is-a? w message%)
                          (and
                           (is-a? w editor-canvas%)
                           (let-values ([(w h) (send w get-client-size)])
                             (< (send e get-x) w))))
                      (begin
                        (send w popup-menu
                              (let ([m (make-object popup-menu%)])
                                (make-object menu-item%
                                  "Copy Message"
                                  m
                                  (lambda (i e)
                                    (send (wx:get-the-clipboard)
                                          set-clipboard-string
                                          message
                                          (send e get-time-stamp))))
                                m)
                              (send e get-x)
                              (send e get-y))
                        #t)
                      #f))
              #f))])
      (super-make-object title parent box-width)))
  (define message-box+check-box/custom-dialog%
    (if check?
        (class message-box/custom-dialog%
          (define/public (set-check-label msg)
            (unless (label-string? msg)
              (raise-argument-error 'set-check-label "label-string?" msg))
            (send check-box set-label msg))
          (super-new))
        message-box/custom-dialog%))
  (define dlg (make-object (dialog-mixin message-box+check-box/custom-dialog%)))
  (define icon-id
    (cond
      [(memq 'no-icon style) #f]
      [(memq 'stop style) 'stop]
      [(memq 'caution style) 'caution]
      [else 'app]))
  (define-values (msg-pnl icon-msg btn-pnl cb-pnl extra-width btn-h-align msg-h-align msg-v-align)
    (case (system-type)
      [(macosx)
       (define p (make-object horizontal-pane% dlg))
       (send dlg min-width 300)
       (send p set-alignment 'center 'top)
       (when icon-id
         (let ([m (make-object message% icon-id p)])
           (send m horiz-margin 16)
           (send m vert-margin 16)))
       (let* ([rhs-pnl (make-object vertical-pane% p)]
              [msg-pnl (make-object vertical-pane% rhs-pnl)]
              [btn-pnl (make-object vertical-pane% rhs-pnl)])
         (send msg-pnl vert-margin 16)
         (send btn-pnl vert-margin 8)
         (send msg-pnl min-height 40)
         (send msg-pnl min-width 300)
         (send btn-pnl stretchable-height #f)
         (values msg-pnl #f btn-pnl btn-pnl 96 'right 'left 'top))]
      [else
       (define p (new horizontal-pane% [parent dlg] [alignment '(center top)]))
       (define icon-msg (and icon-id (make-object message% icon-id p)))
       (define msg-pnl-parent (new vertical-panel% [parent p]))
       (define msg-pnl (new vertical-panel% [parent msg-pnl-parent] [stretchable-height #f]))
       (values msg-pnl icon-msg dlg msg-pnl-parent 0 'center 'center 'center)]))
  (define check-parent-panel
    (and check?
         (new vertical-pane% [parent cb-pnl]
              [stretchable-height #f]
              [alignment '(left center)])))
  (fill-message-box-message check? msg-pnl check-parent-panel msg-h-align msg-v-align dlg message icon-msg)
  (define check-box
    (and check?
         (new check-box%
              [label check-message]
              [parent check-parent-panel]
              [callback void]
              [stretchable-width #t]
              [value (memq 'checked style)])))
  (define p (make-object horizontal-pane% btn-pnl))
  (define (mk-button title v default?)
    (let ([b (make-object button% title p (lambda (b e) (set! result v) (send dlg show #f))
               (if default? '(border) null))])
      (when default? (send b focus))))
  (send p set-alignment btn-h-align 'center)
  (send p stretchable-height #f)
  (send p stretchable-width #t) ; to get panel's centering
  (define (mk-1)
    (when button1
      (mk-button button1 1 (memq 'default=1 style))))
  (define (mk-2)
    (when button2
      (mk-button button2 2 (memq 'default=2 style))))
  (define (mk-3)
    (when button3
      (mk-button button3 3 (memq 'default=3 style))))
  (cond
    [(or (memq 'number-order style)
         (memq (system-type) '(windows)))
     (mk-1)
     (mk-2)
     (mk-3)]
    [else
     (mk-3)
     (make-object horizontal-pane% p)
     (mk-2)
     (mk-1)])
  (send dlg center)
  (cond
    [return-the-dialog?
     dlg]
    [else
     (send dlg show-and-return-results)]))

(define (fill-message-box-message check? _msg-pnl check-parent-panel msg-h-align msg-v-align dlg message icon-msg)
  (define strings (regexp-split #rx"\n" message))
  (define single?
    (and (< (length strings) 10)
         (andmap (lambda (s) (< (string-length s) 60)) strings)))

  (send _msg-pnl change-children (λ (l) '()))
  (define msg-pnl
    (cond
      [(and icon-msg (= 1 (length strings)))
       (new horizontal-panel%
            [parent _msg-pnl]
            [alignment '(center top)]
            [min-height (send icon-msg min-height)])]
      [else _msg-pnl]))

  ;; Match text-panel margin based on `single?` and `check?`.
  (when check?
    (when (equal? 'macosx (system-type))
      (send check-parent-panel horiz-margin (if single? 8 0))))
  (send msg-pnl horiz-margin (if single? 8 0))
  (cond
    [single?
     (send msg-pnl set-alignment (if (= (length strings) 1) msg-h-align 'left) msg-v-align)
     (for-each (lambda (s) (make-object message% (protect& s) msg-pnl)) strings)
     (send dlg stretchable-width #f)
     (send dlg stretchable-height #f)
     (send dlg reflow-container)]
    [else
     (send dlg stretchable-width #t)
     (send dlg stretchable-height #t)
     ;; Try without scrollbar
     (define c (new editor-canvas%
                    [parent msg-pnl]
                    [style '(no-hscroll no-vscroll transparent no-border)]))
     (define e (fill-canvas-with-content c dlg message #f))
     ;; Check whether it actually fits
     (define vh (box 0))
     (define eh (box 0))
     (send e get-view-size #f vh)
     (send e get-extent #f eh)
     (unless ((unbox eh) . <= . (unbox vh))
       ;; Didn't fit; try again with a scrollbar
       (send c show #f)
       (send msg-pnl delete-child c)
       (define new-c (new editor-canvas%
                      [parent msg-pnl]
                      [style '(no-hscroll)]))
       (when (white-on-black-panel-scheme?)
         (send new-c set-canvas-background (send the-color-database find-color "black")))
       (define e (fill-canvas-with-content new-c dlg message #t))
       (void))]))

(define (fill-canvas-with-content c dlg message scroll?)
  (define e (make-object text%))
  (send c set-editor e)
  (send c min-width 400)
  (send c set-line-count 5)
  (send c allow-tab-exit #t)
  (send dlg reflow-container)
  (send e auto-wrap #t)
  (send e insert message)
  (send e set-position 0)
  (send e hide-caret #t)
  (send e set-cursor (make-object wx:cursor% 'arrow) #t)
  (when (white-on-black-panel-scheme?)
    (define sd (new style-delta%))
    (send sd set-delta-foreground "white")
    (send e change-style sd 0 (send e last-position)))
  (send e lock #t)
  e)

(define message-box/custom
  (lambda (title message
                 button1
                 button2
                 button3
                 [parent #f]
                 [style '(no-default)]
                 [close-result #f]
                 #:return-the-dialog? [return-the-dialog? #f]
                 #:dialog-mixin [dialog-mixin values])
    (do-message-box/custom 'message-box/custom
                           title message button1 button2 button3
                           parent style close-result
                           #f #f #f return-the-dialog? dialog-mixin)))

(define do-message-box
  (lambda (who title message parent style check? check-message dialog-mixin)
    (check-label-string who title)
    (check-string/false who message)
    (when check?
      (check-label-string who check-message))
    (check-top-level-parent/false who parent)
    (check-style who
                 '(ok ok-cancel yes-no)
                 (let ([l '(caution stop no-icon)])
                   (if check?
                       (cons 'checked l)
                       l))
                 style)

    (let-values ([(one two one-v two-v close-val default)
                  (cond
                   [(memq 'ok style)
                    (values "OK" #f 'ok #f 1 'default=1)]
                   [(memq 'ok-cancel style)
                    (values "OK" "Cancel" 'ok 'cancel 2 'default=1)]
                   [(memq 'yes-no style)
                    (values "&Yes" "&No" 'yes 'no #f 'no-default)])])
      (let-values ([(result checked?)
                    (do-message-box/custom who
                                           title message
                                           one two #f
                                           parent
                                           (append
                                            (cond
                                             [(memq 'checked style) '(checked)]
                                             [else null])
                                            (cond
                                             [(memq 'no-icon style) '(no-icon)]
                                             [(memq 'stop style) '(stop)]
                                             [(memq 'caution style) '(caution)]
                                             [else null])
                                            (if close-val
                                                (list default)
                                                (list default 'disallow-close)))
                                           close-val
                                           check? #t check-message
                                           #f dialog-mixin)])
        (let ([result (case result
                        [(1) one-v]
                        [(2) two-v])])
          (if check?
              (values result checked?)
              result))))))

(define message-box
  (lambda (title message [parent #f] [style '(ok)] #:dialog-mixin [dialog-mixin values])
    (do-message-box 'message-box title message parent style #f #f dialog-mixin)))

(define message+check-box/custom
  (lambda (title message
                 checkbox-message
                 button1
                 button2
                 button3
                 [parent #f]
                 [style '(no-default)]
                 [close-result #f]
                 #:return-the-dialog? [return-the-dialog? #f]
                 #:dialog-mixin [dialog-mixin values])
    (do-message-box/custom 'message+check-box/custom
                           title message button1 button2 button3
                           parent style close-result
                           #t #t checkbox-message
                           return-the-dialog? dialog-mixin)))

(define message+check-box
  (lambda (title message check-message [parent #f] [style '(ok)] #:dialog-mixin [dialog-mixin values])
    (do-message-box 'message-box title message parent style #t check-message dialog-mixin)))
