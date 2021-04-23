#lang racket/base
(require "sig.rkt"
         "text-sig.rkt"
         (prefix-in base: racket/base)
         simple-tree-text-markup/data
         (only-in simple-tree-text-markup/construct markup-transform-image-data)
         racket/unit
         racket/class
         racket/match
         racket/draw
         mred/mred-sig
         mrlib/interactive-value-port
         (prefix-in image-core: mrlib/image-core))
(provide text-port@)

(define-unit text-port@
  (import mred^
          text-basic^
          (except text-mixed-in-classes^ keymap%)
          [prefix icon: framework:icon^]
          [prefix editor: framework:editor^]
          [prefix srcloc-snip: framework:srcloc-snip^]
          [prefix number-snip: framework:number-snip^]
          [prefix text: text-misc^])
  (export text-port^)

  (define wide-snip<%>
    (interface (basic<%>)
      add-wide-snip
      add-tall-snip))

  (define wide-snip-mixin
    (mixin (basic<%>) (wide-snip<%>)
      (define wide-snips '())
      (define tall-snips '())
      (define/public (add-wide-snip s) (set! wide-snips (cons s wide-snips)))
      (define/public (get-wide-snips) wide-snips)
      (define/public (add-tall-snip s) (set! tall-snips (cons s tall-snips)))
      (define/public (get-tall-snips) tall-snips)
      (super-new)))
  
  (define ports<%>
    (interface ()
      delete/io
      get-insertion-point 
      set-insertion-point
      get-unread-start-point
      set-unread-start-point
      set-allow-edits
      get-allow-edits
      insert-between
      insert-before
      submit-to-port?
      on-submit
      send-eof-to-in-port
      send-eof-to-box-in-port
      reset-input-box
      clear-output-ports
      clear-input-port
      clear-box-input-port
      get-out-style-delta
      get-err-style-delta
      get-value-style-delta
      get-in-port
      get-in-box-port
      get-out-port
      get-err-port
      get-value-port
      after-io-insertion
      get-box-input-editor-snip%
      get-box-input-text%))

  ;; class for snips embedded in markup
  (define markup-text%
    (text:foreground-color-mixin
     (wide-snip-mixin
      (basic-mixin
       (editor:standard-style-list-mixin
        (editor:basic-mixin
         text%))))))
  
  (define-struct peeker (bytes skip-count pe resp-chan nack polling?) #:inspector (make-inspector))
  (define-struct committer (kr commit-peeker-evt done-evt resp-chan resp-nack))

  (define msec-timeout 500)

  ;; this value (4096) is also mentioned in the test suite (collects/tests/framework/test.rkt)
  ;; so if you change it, be sure to change things over there too
  (define output-buffer-full 4096)

  (define-local-member-name 
    new-box-input
    box-input-not-used-anymore
    set-port-text)

  (define (set-box/f! b v) (when (box? b) (set-box! b v)))

  (define arrow-cursor (make-object cursor% 'arrow))

  (define eof-snip%
    (class image-snip%
      (init-field port-text)
      (define/override (get-extent dc x y w h descent space lspace rspace)
        (super get-extent dc x y w h descent space lspace rspace)
        (set-box/f! descent 7)) ;; depends on actual bitmap used ...
    
      (define/override (on-event dc x y editorx editory event)
        (when (send event button-up? 'left)
          (send port-text send-eof-to-box-in-port)))
      (define/override (adjust-cursor dc x y edx edy e)
        arrow-cursor)
      (super-make-object (icon:get-eof-bitmap))
      (inherit set-flags get-flags)
      (set-flags (list* 'handles-events (get-flags)))))

  (define out-style-name "text:ports out")
  (define error-style-name "text:ports err")
  (define value-style-name "text:ports value")
  (let ([create-style-name
         (λ (name sd)
           (let* ([sl (editor:get-standard-style-list)])
             (send sl new-named-style 
                   name
                   (send sl find-or-create-style
                         (send sl find-named-style "Standard")
                         sd))))])
    (let ([out-sd (make-object style-delta% 'change-nothing)])
      (send out-sd set-delta-foreground (make-object color% 150 0 150))
      (create-style-name out-style-name out-sd))
    (let ([err-sd (make-object style-delta% 'change-italic)])
      (send err-sd set-delta-foreground (make-object color% 255 0 0))
      (create-style-name error-style-name err-sd))
    (let ([value-sd (make-object style-delta% 'change-nothing)])
      (send value-sd set-delta-foreground (make-object color% 0 0 175))
      (create-style-name value-style-name value-sd)))

  ;; data : any
  ;; to-insert-chan : (or/c #f channel)
  ;;   if to-insert-chan is a channel, this means
  ;;   the eventspace handler thread is the one that
  ;;   is initiating the communication, so instead of
  ;;   queueing a callback to do the update of the editor,
  ;;   just send the work back directly and it will be done
  ;;   syncronously there. If it is #f, then we queue a callback
  ;;   to do the work
  (define-struct data/chan (data to-insert-chan))

  (struct snip-special (snip name bytes))
  (define (make-snip-special snip)
    (define the-snipclass (send snip get-snipclass))
    (cond
      [the-snipclass
       (define base (new editor-stream-out-bytes-base%))
       (define stream (make-object editor-stream-out% base))
       (write-editor-global-header stream)
       (send snip write stream)
       (write-editor-global-footer stream)
       (snip-special snip
                     (send the-snipclass get-classname)
                     (send base get-bytes))]
      [else
       (snip-special snip #f #f)]))
  ;; -> (or/c (is-a?/c snip%) exn:fail?)
  (define (snip-special->snip snip-special)
    (define the-name (snip-special-name snip-special))
    (define snipclass (and the-name (send (get-the-snip-class-list) find the-name)))
    (cond
      [snipclass
       (with-handlers ([exn:fail? values])
         (define base (make-object editor-stream-in-bytes-base%
                        (snip-special-bytes snip-special)))
         (define es (make-object editor-stream-in% base))
         (read-editor-global-header es)
         (define the-snip (send snipclass read es))
         (read-editor-global-footer es)
         (or the-snip
             (snip-special-snip snip-special)))]
      [else
       (snip-special-snip snip-special)]))

  (define use-style-background-editor-snip%
    (class editor-snip%
      (super-new)
      (inherit use-style-background)
      (use-style-background #t)))
  
  (define ports-mixin
    (mixin (wide-snip<%>) (ports<%>)
      (inherit begin-edit-sequence
               change-style
               delete
               end-edit-sequence
               find-snip
               insert
               get-canvas
               get-start-position
               get-end-position
               get-snip-position
               get-style-list
               get-port-name
               is-locked?
               last-position
               lock
               paragraph-start-position
               position-paragraph
               release-snip
               set-caret-owner
               split-snip
               get-focus-snip
               get-view-size
               scroll-to-position
               position-location
               get-styles-fixed
               set-styles-fixed
               auto-wrap
               get-autowrap-bitmap-width)
    
      ;; private field
      (define eventspace (current-eventspace))
    
      ;; insertion-point : number
      ;; the place where the output ports insert data
      ;; only updated in `eventspace' (above)'s main thread
      (define insertion-point 0)
    
      ;; unread-start-points : number
      ;; from this position to the end of the buffer is the
      ;;      users editing that has not been committed to the
      ;;      port.
      ;; only updated in `eventspace' (above)'s main thread
      (define unread-start-point 0)
    
      ;; box-input : (union #f (is-a?/c editor-snip%))
      ;; the snip where the user's input is typed for the box input port
      (define box-input #f)
      (define eof-button (new eof-snip% (port-text this)))
    
      ;; allow-edits? : boolean
      ;; when this flag is set, only insert/delete after the
      ;; insertion-point are allowed.
      (define allow-edits? #f)
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  public interface
      ;;
    
      ;; insert-between : string/snp -> void
      ;; inserts something between the insertion point and the unread region
      (define/public-final (insert-between str/snp)
        (insert str/snp unread-start-point unread-start-point)
        (set! unread-start-point (+ unread-start-point 
                                    (amt-of-space str/snp))))
    
      ;; insert-before : string/snp -> void
      ;; inserts something before both the insertion point and the unread region
      (define/public-final (insert-before str/snp)
        (insert str/snp insertion-point insertion-point)
        (let ([amt (amt-of-space str/snp)])
          (set! insertion-point (+ insertion-point amt))
          (set! unread-start-point (+ unread-start-point amt))))
    
      (define/private (amt-of-space str/snp)
        (cond
          [(string? str/snp) (string-length str/snp)]
          [(is-a? str/snp snip%)
           (send str/snp get-count)]))
    
      (define/public-final (get-insertion-point) insertion-point)
      (define/public-final (set-insertion-point ip) (set! insertion-point ip))
      (define/public-final (get-unread-start-point)
        unread-start-point)
      (define/public-final (set-unread-start-point u) 
        (unless (<= u (last-position))
          (error 'set-unread-start-point "~e is too large, last-position is ~e"
                 unread-start-point 
                 (last-position)))
        (set! unread-start-point u))
    
      (define/public-final (set-allow-edits allow?) (set! allow-edits? allow?))
      (define/public-final (get-allow-edits) allow-edits?)
    
      (define/public-final (send-eof-to-in-port) 
        (when box-input (new-box-input (send box-input get-editor)))
        (channel-put read-chan (cons eof (position->line-col-pos unread-start-point))))
      (define/public-final (send-eof-to-box-in-port) 
        (when box-input (new-box-input (send box-input get-editor)))
        (channel-put box-read-chan (cons eof (position->line-col-pos unread-start-point))))
      (define/public-final (clear-input-port) (channel-put clear-input-chan (void)))
      (define/public-final (clear-box-input-port) (channel-put box-clear-input-chan (void)))
      (define/public-final (clear-output-ports) 
        (channel-put clear-output-chan (void))
        (init-output-ports))
    
      ;; delete/io: number number -> void
      (define/public-final (delete/io start end)
        (unless (<= start end insertion-point)
          (error 'delete/io "expected start (~a) <= end (~a) <= insertion-point (~a)"
                 start end insertion-point))
      
        (let ([dist (- end start)])
          (set! insertion-point (- insertion-point dist))
          (set! unread-start-point (- unread-start-point dist)))
      
        (let ([before-allowed? allow-edits?])
          (set! allow-edits? #t)
          (delete start end #f)
          (set! allow-edits? before-allowed?)))
    
      (define/public-final (insert/io str start [style #f])
        (unless (<= start insertion-point)
          (error 'insert/io "expected start (~a) <= insertion-point (~a)"
                 start (string-length str) insertion-point))
        (define len (string-length str))
        (set! insertion-point (+ insertion-point len))
        (set! unread-start-point (+ unread-start-point len))
        (let ([before-allowed? allow-edits?])
          (set! allow-edits? #t)
          (insert str start start #f)
          (when style
            (change-style (add-standard style) start (+ start len)))
          (set! allow-edits? before-allowed?)))
      
      (define/public-final (get-in-port)
        (unless in-port (error 'get-in-port "not ready"))
        in-port)
      (define/public-final (get-in-box-port)
        (unless in-port (error 'get-in-box-port "not ready"))
        in-box-port)
      (define/public-final (get-out-port)
        (unless out-port (error 'get-out-port "not ready"))
        out-port)
      (define/public-final (get-err-port)
        (unless err-port (error 'get-err-port "not ready"))
        err-port)
      (define/public-final (get-value-port)
        (unless value-port (error 'get-value-port "not ready"))
        value-port)
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  specialization interface
      ;;
    
      (define/pubment (submit-to-port? key) (inner #t submit-to-port? key))
      (define/pubment (on-submit) (inner (void) on-submit))
      (define/public (get-out-style-delta) out-style-name)
      (define/public (get-err-style-delta) error-style-name)
      (define/public (get-value-style-delta) value-style-name)
    
      (define/public (get-box-input-editor-snip%) use-style-background-editor-snip%)
      (define/public (get-box-input-text%) input-box%)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  editor integration
      ;;
    
      (define/augment (can-insert? start len)
        (and (or allow-edits? 
                 (start . >= . unread-start-point))
             (inner #t can-insert? start len)))
    
      (define/augment (can-delete? start len)
        (and (or allow-edits?
                 (start . >= . unread-start-point))
             (inner #t can-delete? start len)))
    
      (inherit set-position)
      (define/override (on-local-char key)
        (let ([start (get-start-position)]
              [end (get-end-position)]
              [code (send key get-key-code)])
          (cond
            [(not (or (eq? code 'numpad-enter)
                      (equal? code #\return)
                      (equal? code #\newline)))
             (super on-local-char key)]
            [(and (insertion-point . <= . start)
                  (= start end)
                  (submit-to-port? key))
             (insert "\n" (last-position) (last-position))
             (do-submission)]
            [else
             (super on-local-char key)])))
    
      (define/public-final (do-submission)
        (set-position (last-position) (last-position))
        (for-each/snips-chars
         unread-start-point
         (last-position)
         (λ (s/c line-col-pos) 
           (cond
             [(is-a? s/c snip%)
              (channel-put read-chan (cons s/c line-col-pos))]
             [(char? s/c)
              (for-each (λ (b) (channel-put read-chan (cons b line-col-pos)))
                        (bytes->list (string->bytes/utf-8 (string s/c))))])))
        (set! unread-start-point (last-position))
        (set! insertion-point (last-position))
        (on-submit))
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; box input port management
      ;;
    
      (define/public-final (reset-input-box) 
        (when box-input
          (let ([l? (is-locked?)]
                [old-allow-edits? allow-edits?])
            (lock #f)
            (set! allow-edits? #t)
            (send box-input release-from-owner)
            (send eof-button release-from-owner)
            (set! unread-start-point (- unread-start-point 2))
            (set! allow-edits? old-allow-edits?)
            (lock l?))
          (set! box-input #f)))
    
      (define/private (adjust-box-input-width)
        (when box-input
          (define w (box 0))
          (define x (box 0))
          (define bw (send (icon:get-eof-bitmap) get-width))
          (get-view-size w #f)
          (define pos (- (last-position) 2))
          (position-location pos x #f #t
                             (not (= pos (paragraph-start-position (position-paragraph pos)))))
          (define auto-wrap-icon-size (get-autowrap-bitmap-width))
          (define size (- (unbox w) (unbox x) bw 24 auto-wrap-icon-size))
          (when (positive? size)
            (send box-input set-min-width size))))
    
      (define/augment (on-display-size)
        (adjust-box-input-width)
        (inner (void) on-display-size))
    
      (define/private (on-box-peek)
        (unless box-input
          (let* ([ed (new (get-box-input-text%))]
                 [es (new (get-box-input-editor-snip%) 
                          (editor ed))]
                 [locked? (is-locked?)])
            (begin-edit-sequence)
            (send ed set-port-text this)
            (lock #f)
            #;(unless (= unread-start-point (paragraph-start-position 
                                             (position-paragraph unread-start-point)))
                (insert-between "\n"))
            (insert-between es)
            (insert-between eof-button)
            #;(send (get-canvas) add-wide-snip es)
            (set! box-input es)
            (adjust-box-input-width)
            (set-caret-owner es 'display)
            (lock locked?)
            (end-edit-sequence))))
    
      (define/public (new-box-input ed)
        (when (eq? ed (send box-input get-editor)) ;; just in case things get out of sync.
          (let ([locked? (is-locked?)])
            (begin-edit-sequence)
            (send box-input set-min-width 'none)
            (lock #f)
          
            (let ([old-insertion-point insertion-point])
              (let loop ([snip (send (send box-input get-editor) find-first-snip)])
                (when snip
                  (let ([next (send snip next)])
                    (send snip release-from-owner)
                    (do-insertion
                     (list (cons (cond
                                   [(is-a? snip string-snip%)
                                    (send snip get-text 0 (send snip get-count))]
                                   [else snip])
                                 (make-object style-delta%)))
                     #t)
                    (loop next))))
            
              ;; this is copied code ...
              (for-each/snips-chars
               old-insertion-point
               insertion-point
               (λ (s/c line-col-pos) 
                 (cond
                   [(is-a? s/c snip%)
                    (channel-put box-read-chan (cons s/c line-col-pos))]
                   [(char? s/c)
                    (for-each (λ (b) (channel-put box-read-chan (cons b line-col-pos)))
                              (bytes->list (string->bytes/utf-8 (string s/c))))]))))
          
            (lock locked?)
            (adjust-box-input-width)
            (end-edit-sequence))))
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; output port synchronization code
      ;;
    
      ;; the flush chans signal that the buffer-thread should flush pending output
      ;; the diy variant just gets the data back and flushes it itself
      ;; the other causes the thread that services all the events to flush
      ;; the data via queue-callback
      (define flush-chan/diy (make-channel))
      (define flush-chan/queue (make-channel))
    
      ;; clear-output-chan : (channel void)
      (define clear-output-chan (make-channel))
    
      ;; write-chan : (channel (cons (union snip bytes) style))
      ;; send output to the editor
      (define write-chan (make-channel))
    
      ;; readers-chan : (channel (list (channel (union byte snip))
      ;;                               (channel ...)))
      (define readers-chan (make-channel))
    
      ;; queue-insertion : (listof (cons (union string snip) style)) evt -> void
      ;; txt is in the reverse order of the things to be inserted.
      ;; the evt is waited on when the text has actually been inserted
      ;; thread: any thread, except the eventspace main thread
      (define/private (queue-insertion txts signal #:async? [async? #f])
        (parameterize ([current-eventspace eventspace])
          (queue-callback
           (λ ()
             (do-insertion txts #f)
             (if async? (thread (λ () (sync signal))) (sync signal)))
           #f)))

      (inherit line-start-position position-line)
      
      ;; do-insertion : (listof (cons (union string snip) style-delta)) boolean -> void
      ;; thread: eventspace main thread
      (define/private (do-insertion txts showing-input?)
        (define locked? (is-locked?))
        (define sf? (get-styles-fixed))
        (begin-edit-sequence #f)
        (lock #f)
        (set-styles-fixed #f)
        (set! allow-edits? #t)
        (let loop ([txts txts])
          (cond
            [(null? txts) (void)]
            [else 
             (define fst (car txts))

             (define (markup->snip markup style framed?)
               (let* ([text (new markup-text%)]
                      [snip (new editor-snip% [editor text] [with-border? framed?])])
                 (send snip use-style-background #t)
                 (send text set-styles-sticky #f)
                 (define start (send text get-end-position))
                 (insert-markup markup text style #f)
                 (send snip set-style style)
                 (send text lock #t)
                 snip))

             (define (insert-markup markup text style inline?)
               (cond
                 ((string? markup)
                  (send text insert markup))
                 ((empty-markup? markup) (void))
                 ((horizontal-markup? markup)
                  (for-each (lambda (markup)
                              (insert-markup markup text style #t))
                            (horizontal-markup-markups markup)))
                 ((vertical-markup? markup)
                  (if inline?
                      (send text insert (markup->snip markup style #f))
                      (for-each/between (lambda (markup)
                                          (insert-markup markup text style #f))
                                        (lambda () (send text insert #\newline))
                                        (vertical-markup-markups markup))))
                 ((srcloc-markup? markup)
                  (insert-srcloc-markup markup text style))
                 ((framed-markup? markup)
                  (send text insert (markup->snip (framed-markup-markup markup) style #t)))
                 ((image-markup? markup)
                  (send text insert (image-markup->snip markup style)))
                 ((number-markup? markup)
                  (send text insert
                        (number-snip:number->string/snip (number-markup-number markup)
                                                         #:exact-prefix (number-markup-exact-prefix markup)
                                                         #:inexact-prefix (number-markup-inexact-prefix markup)
                                                         #:fraction-view (number-markup-fraction-view markup))))))
                  
             (define (image-markup->snip markup style)
               (let ((data (image-markup-data markup)))
                 (cond
                   ((is-a? data snip%) data)
                   ((snip-special? data)
                    (define snip (snip-special->snip data))
                    (if (exn:fail? snip)
                        (markup->snip (image-markup-alt-markup markup) style #f)
                        snip))
                   (else
                    (markup->snip (image-markup-alt-markup markup) style #f)))))

             (define (insert-srcloc-markup srcloc-markup text style)
               (let ((start (send text get-end-position)))
                 (insert-markup (srcloc-markup-markup srcloc-markup) text style #t)
                 (let ([end (send text get-end-position)])
                   (send text set-clickback
                         start end
                         (lambda (t s e)
                           (srcloc-snip:select-srcloc (srcloc-markup-srcloc srcloc-markup))))
                   (send text change-style
                         (make-object style-delta% 'change-underline #t)
                         start end #f))))
             
             ; like for-each, but with a thunk that gets called between elements
             (define (for-each/between proc between list)
               (let loop ((list list))
                 (cond
                   ((null? list) (void))
                   ((null? (cdr list))
                    (proc (car list)))
                   (else
                    (proc (car list))
                    (between)
                    (loop (cdr list))))))

             (define (insert-str/snp! str/snp style)
               (define inserted-count
                 (if (is-a? str/snp snip%)
                     (send str/snp get-count)
                     (string-length str/snp)))
               (define old-insertion-point insertion-point)
               (set! insertion-point (+ insertion-point inserted-count))
               (set! unread-start-point (+ unread-start-point inserted-count))
               
               (insert str/snp old-insertion-point old-insertion-point #t)
               ;; the idea here is that if you made a string snip, you
               ;; could have made a string and gotten the style, so you
               ;; must intend to have your own style.
               (unless (is-a? str/snp string-snip%)
                 (change-style style old-insertion-point insertion-point)))

             (define (insert-markup-top-level markup style)
               (cond
                 [(string? markup) (insert-str/snp! markup style)]
                 [(empty-markup? markup) (void)]
                 [(horizontal-markup? markup)
                  (for-each (lambda (markup)
                              (insert-markup-top-level markup style))
                            (horizontal-markup-markups markup))]
                 [(vertical-markup? markup)
                  (define pos (get-end-position))
                  (if (= pos (line-start-position (position-line pos))) ; at bol?
                      (for-each/between (lambda (markup)
                                          (insert-markup-top-level markup style))
                                        (lambda () (insert-str/snp! "\n" style))
                                        (vertical-markup-markups markup))
                      (insert-str/snp! (markup->snip markup style #f) style))]
                 [(srcloc-markup? markup)
                  (let* ([snip (new srcloc-snip:snip% [srcloc (srcloc-markup-srcloc markup)])]
                         [editor (send snip get-editor)])
                    (insert-markup (srcloc-markup-markup markup) editor style #t)
                    (let ((end (send editor get-end-position)))
                      (send editor change-style style 0 end #f)
                      (send editor change-style
                            (make-object style-delta% 'change-underline #t)
                            0 end #f))
                    (send snip activate-link)
                    (insert-str/snp! snip style))]
                 [(framed-markup? markup)
                  (insert-str/snp! (markup->snip (framed-markup-markup markup) style #t) style)]
                 [(image-markup? markup)
                  (insert-str/snp! (image-markup->snip markup style) style)]
                 [(number-markup? markup)
                  (insert-str/snp! (number-snip:number->string/snip (number-markup-number markup)
                                                                    #:exact-prefix (number-markup-exact-prefix markup)
                                                                    #:inexact-prefix (number-markup-inexact-prefix markup)
                                                                    #:fraction-view (number-markup-fraction-view markup))
                                   style)]))
             
             (define thing (car fst))
             (define style (cdr fst))
             
             (cond
               [(snip-special? thing)
                (define the-snip
                  (snip-special->snip thing))
                (if (exn:fail? the-snip)
                    (insert-str/snp! (apply
                                      string-append
                                      "error while rendering snip "
                                      (format "~s" (snip-special-name thing))
                                      ":\n"
                                      (exn-message the-snip)
                                      "  context:\n"
                                      (for/list ([x (in-list (continuation-mark-set->context
                                                              (exn-continuation-marks
                                                               the-snip)))])
                                        (format "   ~s\n" x)))
                                     (add-standard error-style-name))
                    (insert-str/snp! the-snip style))]
               [(string? thing) (insert-str/snp! thing style)]
               [(markup? thing) (insert-markup-top-level thing style)]
               [(is-a? thing snip%) (insert-str/snp! (send thing copy) style)]
               [else (void)])
             
             (loop (cdr txts))]))
        (set-styles-fixed sf?)
        (set! allow-edits? #f)
        (lock locked?)
        (unless showing-input?
          (when box-input
            (adjust-box-input-width)
            (when (eq? box-input (get-focus-snip))
              (scroll-to-position (last-position)))))
        (end-edit-sequence)
        (unless (null? txts)
          (after-io-insertion)))

    
      (define/public (after-io-insertion) (void))

      (let ()
        (define converter (bytes-open-converter "UTF-8-permissive" "UTF-8"))
        (define (output-buffer-thread)
          (let loop (;; text-to-insert : (queue (cons (union snip bytes) style))
                     [text-to-insert (empty-at-queue)]
                     [last-flush (current-inexact-milliseconds)])
            (sync
             (if (at-queue-empty? text-to-insert)
                 never-evt
                 (handle-evt
                  (alarm-evt (+ last-flush msec-timeout))
                  (λ (_)
                    (define-values (viable-bytes remaining-queue flush-keep-trying?)
                      (split-queue converter text-to-insert))
                    ;; we always queue the work here since the
                    ;; always event means no one waits for the callback
                    (queue-insertion viable-bytes always-evt)
                    (loop remaining-queue (current-inexact-milliseconds)))))
             (handle-evt
              flush-chan/diy
              (λ (return-evt/to-insert-chan)
                (define remaining-queue #f)
                (define viable-bytess
                  (let loop ([q text-to-insert])
                    (define-values (viable-bytes next-remaining-queue flush-keep-trying?)
                      (split-queue converter q))
                    (cond
                      [flush-keep-trying?
                       (cons viable-bytes (loop next-remaining-queue))]
                      [else
                       (set! remaining-queue next-remaining-queue)
                       (list viable-bytes)])))
                (channel-put return-evt/to-insert-chan viable-bytess)
                (loop remaining-queue (current-inexact-milliseconds))))
             (handle-evt
              flush-chan/queue
              (λ (return-evt/to-insert-chan)
                (define remaining-queue #f)
                (let loop ([q text-to-insert])
                  (define-values (viable-bytes next-remaining-queue flush-keep-trying?)
                    (split-queue converter q))
                  (cond
                    [flush-keep-trying?
                     (queue-insertion viable-bytes always-evt)
                     (loop next-remaining-queue)]
                    [else
                     (set! remaining-queue next-remaining-queue)
                     (queue-insertion viable-bytes return-evt/to-insert-chan #:async? #t)
                     #f]))
                (loop remaining-queue (current-inexact-milliseconds))))
             (handle-evt
              clear-output-chan
              (λ (_)
                (loop (empty-at-queue) (current-inexact-milliseconds))))
             (handle-evt
              write-chan
              (λ (pr-pr)
                (define return-chan (car pr-pr))
                (define pr (cdr pr-pr))
                (let ([new-text-to-insert (at-enqueue pr text-to-insert)])
                  (cond
                    [((at-queue-size text-to-insert) . < . output-buffer-full)
                     (when return-chan
                       (channel-put return-chan '()))
                     (loop new-text-to-insert
                           (if (at-queue-empty? text-to-insert)
                               (current-inexact-milliseconds)
                               last-flush))]
                    [else
                     (define-values (viable-bytes remaining-queue flush-keep-trying?)
                       (split-queue converter new-text-to-insert))
                     (cond
                       [return-chan
                        (channel-put return-chan viable-bytes)]
                       [else
                        (define chan (make-channel))
                        (queue-insertion viable-bytes (channel-put-evt chan (void)))
                        (channel-get chan)])
                     (loop remaining-queue (current-inexact-milliseconds))])))))))
        (thread output-buffer-thread))
    
      (field [in-port-args #f]
             [out-port #f]
             [err-port #f]
             [value-port #f])
    
      (define/private (init-output-ports)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;;  the following must be able to run
        ;;  in any thread (even concurrently)
        ;;
        (define (make-write-bytes-proc style)
          (λ (to-write start end block/buffer? enable-breaks?)
            (cond
              [(= start end) (flush-proc)]
              [else
               (define pair (cons (if (and (= start 0)
                                           (= end (bytes-length to-write))
                                           (immutable? to-write))
                                      to-write
                                      (subbytes to-write start end))
                                  style))
               (cond
                 [(eq? (current-thread) (eventspace-handler-thread eventspace))
                  (define return-channel (make-channel))
                  (thread (λ () (channel-put write-chan (cons return-channel pair))))
                  (do-insertion (channel-get return-channel) #f)]
                 [else
                  (channel-put write-chan (cons #f pair))])])
            (- end start)))
      
        (define (flush-proc)
          (cond
            [(eq? (current-thread) (eventspace-handler-thread eventspace))
             (define to-insert-channel (make-channel))
             (thread (λ () (channel-put flush-chan/diy to-insert-channel)))
             (for ([ele (in-list (channel-get to-insert-channel))])
               (do-insertion ele #f))]
            [else
             (sync
              (nack-guard-evt
               (λ (fail-channel)
                 (let* ([return-channel (make-channel)]
                        [return-evt
                         (choice-evt
                          fail-channel
                          (channel-put-evt return-channel (void)))])
                   (channel-put flush-chan/queue return-evt)
                   return-channel))))]))
      
        (define (out-close-proc)
          (void))
        
        (define (make-write-special-proc style)
          (λ (special can-buffer? enable-breaks?)
            (define do-put
              (cond
                [(eq? (current-thread) (eventspace-handler-thread eventspace))
                 (define return-chan (make-channel))
                 (lambda (str/snp)
                   (thread (λ () (channel-put write-chan (cons return-chan (cons str/snp style)))))
                   (do-insertion (channel-get return-chan) #f))]
                [else
                 (lambda (str/snp)
                   (channel-put write-chan (cons #f (cons str/snp style))))]))

            (define (put-special special)
              (cond
                [(string? special) (do-put special)]
                [(snip-special? special) (do-put special)]
                [(is-a? special snip%) (do-put special)]
                [(markup? special) (do-put (markup-transform-image-data encode-image-data special))]
                [else (do-put (format "~s" special))]))

            (put-special special)))
      
        (let ([out-style (add-standard (get-out-style-delta))]
              [err-style (add-standard (get-err-style-delta))]
              [value-style (add-standard (get-value-style-delta))])
          (set! out-port (make-output-port #f
                                           always-evt
                                           (make-write-bytes-proc out-style)
                                           out-close-proc
                                           (make-write-special-proc out-style)))
          (set! err-port (make-output-port #f 
                                           always-evt
                                           (make-write-bytes-proc err-style)
                                           out-close-proc
                                           (make-write-special-proc err-style)))
          (set! value-port (make-output-port #f
                                             always-evt
                                             (make-write-bytes-proc value-style)
                                             out-close-proc
                                             (make-write-special-proc value-style)))
          (let ([install-handlers
                 (λ (port)
                   ;; don't want to set the port-print-handler here; 
                   ;; instead drracket sets the global-port-print-handler
                   ;; to catch fractions and the like
                   (set-interactive-write-handler port #:snip-handler send-snip-to-port)
                   (set-interactive-display-handler port #:snip-handler send-snip-to-port))])
            (install-handlers out-port)
            (install-handlers err-port)
            (install-handlers value-port))))
    
      (define/private (add-standard sd)
        (cond
          [(string? sd)
           (define style-list (get-style-list))
           (or (send style-list find-named-style sd)
               (send style-list find-named-style "Standard")
               (send style-list basic-style))]
          [sd
           (define style-list (get-style-list))
           (define std (send style-list find-named-style "Standard"))
           (cond
             [std
              (send style-list find-or-create-style std sd)]
             [else
              (define basic (send style-list basic-style))
              (send style-list find-or-create-style basic sd)])]))
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  helpers
      ;;
    
      ;; type line-col-pos = (list (union #f fixnum) (union #f fixnum) (union #f fixnum)))
    
      ;; position->line-col-pos : number -> (list number number number)
      (define/private (position->line-col-pos pos)
        (let* ([para (position-paragraph pos)]
               [para-start (paragraph-start-position para)])
          (list (+ para 1)
                (- pos para-start)
                (+ pos 1))))
    
      ;; for-each/snips-chars : number number ((union char snip) line-col-pos -> void) -> void
      (define/private (for-each/snips-chars start end func)
        (split-snip start)
        (split-snip end)
        (let loop ([snip (find-snip start 'after-or-none)])
          (cond
            [(not snip) (void)]
            [(< (get-snip-position snip) end)
             (let ([line-col-pos (position->line-col-pos (get-snip-position snip))])
               (cond
                 [(is-a? snip string-snip%)
                  (let ([str (send snip get-text 0 (send snip get-count))])
                    (let loop ([i 0])
                      (when (< i (string-length str))
                        (func (string-ref str i)
                              (list (car line-col-pos)
                                    (+ i (cadr line-col-pos))
                                    (+ i (caddr line-col-pos))))
                        (loop (+ i 1)))))
                  (loop (send snip next))]
                 [else
                  (func (send snip copy) line-col-pos)
                  (loop (send snip next))]))]
            [else (void)])))
    
    
      ;; split-queue : converter (queue (cons (union snip bytes) style) 
      ;;            -> (values (listof (queue (cons (union snip bytes) style))
      ;;                       queue
      ;;                       boolean)
      ;; this function must only be called on the output-buffer-thread
      ;; extracts the viable bytes (and other stuff) from the front of the queue
      ;; and returns them as strings (and other stuff).
      ;; the boolean result is #t when a flush should try to get more stuff out of the
      ;;   queue for a second GUI callback
      (define/private (split-queue converter q)
      
        ;; this number based on testing in drracket's REPL
        ;; the number can be 10x bigger if you use a vanilla
        ;; text, but something about something in how DrRacket's
        ;; styles or something else is set up makes this number
        ;; take more like 20-60 msec per event (on my laptop)
        ;; for a bytes containing all (char->integer #\a)s. Random
        ;; bytes are slower, but probably that's not the common case.
        (define bytes-limit-for-a-single-go 1000)
      
        (let loop ([lst (at-queue->list q)] [acc null])
          (cond
            [(null? lst)
             (values (reverse acc)
                     (empty-at-queue)
                     #f)]
            [else
             (define-values (front rest) (peel lst))
             (cond
               [(not front) (values (reverse acc)
                                    (empty-at-queue)
                                    #f)]
               [(bytes? (car front))
                (define the-bytes (car front))
                (define key (cdr front))
                (define too-many-bytes? (>= (bytes-length the-bytes) bytes-limit-for-a-single-go))
                (cond
                  [(or (null? rest) too-many-bytes?)
                   (define-values (converted-bytes src-read-amt termination)
                     (bytes-convert converter the-bytes 0 (min (bytes-length the-bytes)
                                                               bytes-limit-for-a-single-go)))
                   (define new-at-queue 
                     (cond
                       [(= src-read-amt (bytes-length the-bytes))
                        (list->at-queue rest)]
                       [else
                        (define leftovers (subbytes the-bytes src-read-amt (bytes-length the-bytes)))
                        (list->at-queue (cons (cons leftovers key) rest))]))
                   (define converted-str (bytes->string/utf-8 converted-bytes))
                   (values (reverse (cons (cons converted-str key) acc))
                           new-at-queue
                           too-many-bytes?)]
                  [else
                   (define-values (converted-bytes src-read-k termination)
                     (bytes-convert converter the-bytes))
                   (define-values (more-bytes more-termination) (bytes-convert-end converter))
                   (loop rest
                         (cons (cons (bytes->string/utf-8 (bytes-append converted-bytes more-bytes))
                                     key)
                               acc))])]
               [else (loop rest
                           (cons front acc))])])))
    
      (define/override (after-set-port-unsaved-name)
        (set! in-port (make-in-port-with-a-name (get-port-name)))
        (set! in-box-port (make-in-box-port-with-a-name (get-port-name))))
    
      (super-new)
      (init-output-ports)
      (define-values (make-in-port-with-a-name read-chan clear-input-chan)
        (start-text-input-port #f))
      (define-values (make-in-box-port-with-a-name box-read-chan box-clear-input-chan)
        (start-text-input-port (lambda () (on-box-peek))))
      (define in-port (make-in-port-with-a-name (get-port-name)))
      (define in-box-port (make-in-box-port-with-a-name (get-port-name)))))

  (define (send-snip-to-port value port)
    (cond
      [(image-core:image? value)
       ;; do this computation here so that any failures
       ;; during drawing happen under the user's custodian
       (image-core:compute-image-cache value)

       ;; once that is done, we trust the value not to run
       ;; any code that the user wrote, so just send it over
       (write-special value port)]
      [else
       (define str (format "~s" value))
       (cond
         ;; special case these snips as they don't work properly
         ;; without this and we aren't ready to break them yet
         ;; and image-core:image? should be safe-- there is no user
         ;; code in those images to fail
         [(or (regexp-match? #rx"plot-snip%" str)
              (regexp-match? #rx"pict3d%" str))
          (write-special (send value copy) port)]
         [else
          (write-special (make-snip-special (send value copy)) port)])])
    (void))

  (define (encode-image-data value)
    (when (image-core:image? value)
      (image-core:compute-image-cache value))
    ; Note that image-core:image? is not an appropriate predicate here
    ; to control serialization, as it just checks subtyping for
    ; various classes and interfaces, which anyone could implement.
    (cond
      [(is-a? value snip%)
       (define str (format "~s" value))
       (cond
         ;; special case these snips as they don't work properly
         ;; without this and we aren't ready to break them yet
         ;; and image-core:image? should be safe-- there is no user
         ;; code in those images to fail
         [(or (regexp-match? #rx"plot-snip%" str)
              (regexp-match? #rx"pict3d%" str))
          (send value copy)]
         [else
          (define special (make-snip-special (send value copy)))
          (and (snip-special-name special)
               special)])]
      [(is-a? value bitmap%)
       (define w (send value get-width))
       (define h (send value get-height))
       (define copy (make-object bitmap% w h
                      (= (send value get-depth) 1)
                      (send value has-alpha-channel?)
                      (send value get-backing-scale)))
       (define pixels (make-bytes (* w h 4)))
       (send value get-argb-pixels 0 0 w h pixels)
       (send copy set-argb-pixels 0 0 w h pixels)
       (make-object image-snip% copy)]
      [(record-dc-datum? value)
       (with-handlers ((exn:fail? (lambda (e) #f)))
         (let ((proc (recorded-datum->procedure (record-dc-datum-datum value)))
               (bitmap (make-object bitmap% (record-dc-datum-width value) (record-dc-datum-height value))))
           (let ((dc (new bitmap-dc% [bitmap bitmap] )))
             (proc dc)
             (make-object image-snip% bitmap))))]
      [else #f]))
  
  (define input-box<%>
    (interface ((class->interface text%))
      ))

  (define input-box-mixin
    (mixin ((class->interface text%)) (input-box<%>)
      (inherit erase lock)
    
      (define port-text #f)
      (define/public (set-port-text pt) (set! port-text pt))
    
      (define in-use? #t)
      (define/public (box-input-not-used-anymore)
        (lock #t)
        (set! in-use? #f))

      (define/override (default-style-name)
        (editor:get-default-color-style-name))

      (define/override (on-default-char kevt)
        (super on-default-char kevt)
        (when in-use?
          (case (send kevt get-key-code)
            [(numpad-enter #\return)
             (send port-text new-box-input this)]
            [else (void)])))
    
      (super-new)))

  (define (start-text-input-port on-peek)
  
    ;; eventspace at the time this function was called. used for peek callbacks
    (define eventspace (current-eventspace))
  
    ;; read-chan : (channel (cons (union byte snip eof) line-col-pos))
    ;; send input from the editor
    (define read-chan (make-channel))
  
    ;; clear-input-chan : (channel void)
    (define clear-input-chan (make-channel))
  
    ;; progress-event-chan : (channel (cons (channel event) nack-evt)))
    (define progress-event-chan (make-channel))
  
    ;; peek-chan : (channel peeker)
    (define peek-chan (make-channel))
  
    ;; commit-chan : (channel committer)
    (define commit-chan (make-channel))
  
    ;; position-chan : (channel (cons (channel void) (channel line-col-pos)))
    (define position-chan (make-channel))
  
    (define input-buffer-thread
      (thread
       (λ ()
       
         ;; these vars are like arguments to the loop function
         ;; they are only set right before loop is called.
         ;; This is done to avoid passing the same arguments
         ;; over and over to loop.
         (define peeker-sema (make-semaphore 0))
         (define peeker-evt (semaphore-peek-evt peeker-sema))
         (define bytes-peeked 0)
         (define response-evts '())
         (define peekers '())     ;; waiting for a peek
         (define committers '())  ;; waiting for a commit
         (define positioners '()) ;; waiting for a position
         (define data (empty-at-queue)) ;; (queue (cons (union byte snip eof) line-col-pos))
         (define position #f)
       
         ;; loop : -> alpha
         ;; the main loop for this thread
         (define (loop)
           (let-values ([(not-ready-peekers new-peek-response-evts)
                         (separate peekers service-waiter)]
                        [(potential-commits new-commit-response-evts) 
                         (separate 
                          committers
                          (service-committer (at-queue-size data) peeker-evt))])
             (when (and on-peek
                        (not (null? not-ready-peekers)))
               (parameterize ([current-eventspace eventspace])
                 (queue-callback on-peek)))
             (set! peekers not-ready-peekers)
             (set! committers potential-commits)
             (set! response-evts 
                   (append response-evts
                           new-peek-response-evts
                           new-commit-response-evts))
             (sync
              (handle-evt
               position-chan
               (λ (pr)
                 (let ([nack-chan (car pr)]
                       [resp-chan (cdr pr)])
                   (set! positioners (cons pr positioners))
                   (loop))))
              (apply choice-evt (map service-positioner positioners))
              (handle-evt
               read-chan
               (λ (ent)
                 (at-enqueue! ent data)
                 (unless position
                   (set! position (cdr ent)))
                 (loop)))
              (handle-evt
               clear-input-chan
               (λ (_)
                 (semaphore-post peeker-sema)
                 (set! peeker-sema (make-semaphore 0))
                 (set! peeker-evt (semaphore-peek-evt peeker-sema))
                 (set! data (empty-at-queue))
                 (set! position #f)
                 (loop)))
              (handle-evt
               progress-event-chan
               (λ (return-pr)
                 (let ([return-chan (car return-pr)]
                       [return-nack (cdr return-pr)])
                   (set! response-evts
                         (cons (choice-evt
                                return-nack
                                (channel-put-evt return-chan peeker-evt))
                               response-evts))
                   (loop))))
              (handle-evt
               peek-chan
               (λ (peeker)
                 (set! peekers (cons peeker peekers))
                 (loop)))
              (handle-evt
               commit-chan
               (λ (committer)
                 (set! committers (cons committer committers))
                 (loop)))
              (apply 
               choice-evt
               (map
                (λ (a-committer)
                  (match a-committer
                    [(struct committer 
                       (kr
                        commit-peeker-evt
                        done-evt
                        resp-chan
                        resp-nack))
                     (choice-evt
                      (handle-evt 
                       commit-peeker-evt
                       (λ (_)
                         ;; this committer will be thrown out in next iteration
                         (loop)))
                      (handle-evt
                       done-evt
                       (λ (v)
                         (let* ([nth (at-peek-n data (- kr 1))]
                                [nth-pos (cdr nth)])
                           (set! position
                                 (if (eof-object? (car nth))
                                     nth-pos
                                     (list (car nth-pos)
                                           (+ 1 (cadr nth-pos))
                                           (+ 1 (caddr nth-pos))))))
                         (set! data (at-dequeue-n data kr))
                         (semaphore-post peeker-sema)
                         (set! peeker-sema (make-semaphore 0))
                         (set! peeker-evt (semaphore-peek-evt peeker-sema))
                         (set! committers (remq a-committer committers))
                         (set! response-evts
                               (cons 
                                (choice-evt
                                 resp-nack
                                 (channel-put-evt resp-chan #t))
                                response-evts))
                         (loop))))]))
                committers))
              (apply choice-evt 
                     (map (λ (resp-evt)
                            (handle-evt
                             resp-evt
                             (λ (_)
                               (set! response-evts (remq resp-evt response-evts))
                               (loop))))
                          response-evts)))))
       
         ;; service-positioner : (cons (channel void) (channel line-col-pos)) -> evt
         (define (service-positioner pr)
           (let ([nack-evt (car pr)]
                 [resp-evt (cdr pr)])
             (handle-evt
              (choice-evt nack-evt 
                          (channel-put-evt resp-evt (or position 
                                                      
                                                        ;; a bogus position for when 
                                                        ;; nothing has happened yet.
                                                        (list 1 0 1))))
              (let ([sent-position position])
                (λ (_)
                  (set! positioners (remq pr positioners))
                  (loop))))))
       
         ;; service-committer : queue evt -> committer -> (union #f evt)
         ;; if the committer can be dumped, return an evt that
         ;; does the dumping. otherwise, return #f
         (define ((service-committer size peeker-evt) a-committer)
           (match a-committer
             [(struct committer
                (kr commit-peeker-evt
                    done-evt resp-chan resp-nack))
              (cond
                [(not (eq? peeker-evt commit-peeker-evt))
                 (choice-evt
                  resp-nack
                  (channel-put-evt resp-chan #f))]
                [(< size kr)
                 (choice-evt
                  resp-nack
                  (channel-put-evt resp-chan 'commit-failure))]
                [else  ;; commit succeeds
                 #f])]))
       
         ;; service-waiter : peeker -> (union #f evt)
         ;; if the peeker can be serviced, build an event to service it
         ;; otherwise return #f
         (define (service-waiter a-peeker)
           (match a-peeker
             [(struct peeker (bytes skip-count pe resp-chan nack-evt polling?))
              (cond
                [(and pe (not (eq? pe peeker-evt)))
                 (choice-evt (channel-put-evt resp-chan #f)
                             nack-evt)]
                [((at-queue-size data) . > . skip-count)
                 (let ([nth (car (at-peek-n data skip-count))])
                   (choice-evt
                    nack-evt
                    (cond
                      [(byte? nth)
                       (bytes-set! bytes 0 nth)
                       (channel-put-evt resp-chan 1)]
                      [(eof-object? nth)
                       (channel-put-evt resp-chan nth)]
                      [else
                       (channel-put-evt 
                        resp-chan
                        (λ (src line col pos)
                          (if (is-a? nth readable-snip<%>)
                              (send nth read-special src line col pos)
                              nth)))])))]
                [polling? 
                 (choice-evt
                  nack-evt
                  (channel-put-evt resp-chan 0))]
                [else
                 #f])]))
       
         ;; separate (listof X) (X -> (union #f Y)) -> (values (listof X) (listof Y))
         ;; separates `eles' into two lists -- those that `f' returns #f for
         ;; and then the results of calling `f' for those where `f' doesn't return #f
         (define (separate eles f)
           (let loop ([eles eles]
                      [transformed '()]
                      [left-alone '()])
             (cond
               [(null? eles) (values left-alone transformed)]
               [else (let* ([ele (car eles)]
                            [maybe (f ele)])
                       (if maybe
                           (loop (cdr eles)
                                 (cons maybe transformed)
                                 left-alone)
                           (loop (cdr eles)
                                 transformed
                                 (cons ele left-alone))))])))
       
         ;;; start things going
         (loop))))
  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;  the following must be able to run
    ;;  in any thread (even concurrently)
    ;;
    (define (read-bytes-proc bstr)
      (let* ([progress-evt (progress-evt-proc)]
             [v (peek-proc bstr 0 progress-evt)])
        (cond
          [(sync/timeout 0 progress-evt)
           0]
          [else 
           (wrap-evt 
            v 
            (λ (v) 
              (if (and (number? v) (zero? v))
                  0
                  (if (commit-proc (if (number? v) v 1)
                                   progress-evt
                                   always-evt)
                      v
                      0))))])))
  
    (define (peek-proc bstr skip-count progress-evt)
      (poll-guard-evt
       (lambda (polling?)
         (define evt 
           (nack-guard-evt
            (λ (nack)
              (define chan (make-channel))
              (channel-put peek-chan (make-peeker bstr skip-count progress-evt chan nack polling?))
              chan)))
         (if polling? 
             (let ([v (sync evt)])
               (if (eq? v 0)
                   ;; Don't return 0, because that means something is
                   ;; probably ready. We want to indicate that nothing is
                   ;; ready.
                   never-evt
                   ;; Even on success, package it as an event, because
                   ;; `read-bytes-proc' expects an event
                   (wrap-evt always-evt (lambda (_) v))))
             evt))))
  
    (define (progress-evt-proc)
      (sync
       (nack-guard-evt
        (λ (nack)
          (let ([chan (make-channel)])
            (channel-put progress-event-chan (cons chan nack))
            chan)))))
  
    (define (commit-proc kr progress-evt done-evt)
      (sync
       (nack-guard-evt
        (λ (nack)
          (let ([chan (make-channel)])
            (channel-put commit-chan (make-committer kr progress-evt done-evt chan nack))
            chan)))))
  
    (define (close-proc) (void))
  
    (define (position-proc)
      (let ([chan (make-channel)])
        (apply 
         values
         (sync
          (nack-guard-evt
           (λ (fail)
             (channel-put position-chan (cons fail chan))
             chan))))))
    (define (make-the-port source)
      (define p (make-input-port source
                                 read-bytes-proc
                                 peek-proc
                                 close-proc
                                 progress-evt-proc
                                 commit-proc
                                 position-proc))
      (port-count-lines! p)
      p)
    (values make-the-port read-chan clear-input-chan))


  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; queues
;;
(define-struct at-queue (front back count) #:mutable)
(define (empty-at-queue) (make-at-queue '() '() 0))
(define (at-enqueue e q) (make-at-queue
                          (cons e (at-queue-front q))
                          (at-queue-back q)
                          (+ (at-queue-count q) 1)))
(define (at-enqueue! e q)
  (set-at-queue-front! q (cons e (at-queue-front q)))
  (set-at-queue-count! q (+ (at-queue-count q) 1)))
(define (at-queue-first q)
  (at-flip-around q)
  (let ([back (at-queue-back q)])
    (if (null? back)
        (error 'at-queue-first "empty queue")
        (car back))))
(define (at-queue-rest q)
  (at-flip-around q)
  (let ([back (at-queue-back q)])
    (if (null? back)
        (error 'queue-rest "empty queue")
        (make-at-queue (at-queue-front q)
                       (cdr back)
                       (- (at-queue-count q) 1)))))
(define (at-flip-around q)
  (when (null? (at-queue-back q))
    (set-at-queue-back! q (reverse (at-queue-front q)))
    (set-at-queue-front! q '())))

(define (at-queue-empty? q) (zero? (at-queue-count q)))
(define (at-queue-size q) (at-queue-count q))

;; at-queue->list : (queue x) -> (listof x)
;; returns the elements in the order that successive deq's would have
(define (at-queue->list q) 
  (let ([ans (append (at-queue-back q) (reverse (at-queue-front q)))])
    (set-at-queue-back! q ans)
    (set-at-queue-front! q '())
    ans))

(define (list->at-queue l) (make-at-queue '() l (length l)))

;; dequeue-n : queue number -> queue
(define (at-dequeue-n queue n)
  (let loop ([q queue]
             [n n])
    (cond
      [(zero? n) q]
      [(at-queue-empty? q) (error 'dequeue-n "not enough!")]
      [else (loop (at-queue-rest q) (- n 1))])))

;; peek-n : queue number -> queue
(define (at-peek-n queue init-n)
  (let loop ([q queue]
             [n init-n])
    (cond
      [(zero? n) 
       (when (at-queue-empty? q)
         (error 'peek-n "not enough; asked for ~a but only ~a available" 
                init-n 
                (at-queue-size queue)))
       (at-queue-first q)]
      [else 
       (when (at-queue-empty? q)
         (error 'dequeue-n "not enough!"))
       (loop (at-queue-rest q) (- n 1))])))

;;
;;  end queue abstraction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (check-equal? (let* ([q1 (empty-at-queue)]
                       [q2 (at-enqueue 1 q1)])
                  (at-queue-first q2))
                1)
  (check-equal? (let* ([q1 (empty-at-queue)]
                       [q2 (at-enqueue 1 q1)])
                  (list (at-queue-size q1)
                        (at-queue-size q2)))
                (list 0 1))
  (check-equal? (let* ([q1 (empty-at-queue)]
                       [q2 (at-enqueue 1 q1)]
                       [q3 (at-enqueue 2 q2)]
                       [q4 (at-enqueue 3 q3)])
                  (at-queue->list q4))
                '(1 2 3))
  (check-equal? (at-queue->list (list->at-queue '(1 2 3)))
                '(1 2 3)))

;; peel : (cons/c (cons/c (or/c bytes? (not/c bytes?)) X) 
;;                        (listof (cons (or/c bytes? (not/c bytes?)) X))
;;     -> (values (cons/c (or/c bytes? (not/c bytes?)) X)
;;                (listof (cons (or/c bytes? (not/c bytes?)) X)
;; finds the first segment of bytes with the same style and combines them,
;; otherwise a lot like (define (peel x) (values (car x) (cdr x)))
(define (peel lst)
  (let loop ([lst lst]
             [acc '()]
             [key #f])
    (cond
      [(null? lst) (values (cons (peel-acc->bytes acc) key) null)]
      [else 
       (let* ([fst (car lst)]
              [fst-key (cdr fst)]
              [fst-val (car fst)])
         (cond
           [(and (not key) (bytes? fst-val))
            (loop (cdr lst)
                  (list fst-val)
                  fst-key)]
           [(and key (bytes? fst-val) (eq? key fst-key))
            (loop (cdr lst)
                  (cons fst-val acc)
                  key)]
           [(not key)
            (values fst (cdr lst))]
           [else (if (pair? acc)
                     (values (cons (peel-acc->bytes acc) key) lst)
                     (values fst (cdr lst)))]))])))

(define (peel-acc->bytes acc)
  (apply bytes-append (reverse acc)))

(module+ test
  (require rackunit)
  (define (peek-lst arg) (define-values (x y) (peel arg)) (list x y))
  (check-equal? (peek-lst (list (cons #"x" 'one)))
                (list '(#"x" . one) '()))
  (check-equal? (peek-lst (list (cons 'nb 'one)))
                (list '(nb . one) '()))
  (check-equal? (peek-lst (list (cons 'nb1 'one) (cons 'nb2 'one)))
                (list '(nb1 . one) '((nb2 . one))))
  (check-equal? (peek-lst (list (cons 'nb1 'one) (cons 'nb2 'two)))
                (list '(nb1 . one) '((nb2 . two))))
  (check-equal? (peek-lst (list (cons #"x" 'one) (cons #"y" 'one)))
                (list '(#"xy" . one) '()))
  (check-equal? (peek-lst (list (cons #"x" 'one) (cons 'nb 'one)))
                (list '(#"x" . one) '((nb . one))))
  (check-equal? (peek-lst (list (cons #"x" 'one) (cons #"y" 'two)))
                (list '(#"x" . one) '((#"y" . two))))
  (check-equal? (peek-lst (list (cons #"x" 'one) (cons #"y" 'one) (cons #"z" 'two)))
                (list '(#"xy" . one) '((#"z" . two)))))
