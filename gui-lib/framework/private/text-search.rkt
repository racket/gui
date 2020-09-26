#lang racket/base

(require racket/unit
         racket/class
         racket/list
         mred/mred-sig
         "coroutine.rkt"
         "sig.rkt"
         "text-sig.rkt"
         "../preferences.rkt"
         "interfaces.rkt")
(provide text-search@)

(define-unit text-search@
  (import mred^
          [prefix frame: framework:frame^]
          [prefix editor: framework:editor^]
          [prefix keymap: framework:keymap^]
          text-basic^)
  (export text-search^)

  (define searching<%> 
    (interface (editor:keymap<%> basic<%>)
      set-replace-start
      get-replace-search-hit
      set-searching-state
      set-search-anchor
      get-search-bubbles
      get-search-hit-count
      finish-pending-search-work))

  (define normal-search-color (send the-color-database find-color "plum"))
  (define dark-search-color (send the-color-database find-color "mediumorchid"))
  (define light-search-color 
    (let ([f (λ (x) (+ x (floor (* (- 255 x) 2/3))))])
      (make-object color% 
        (f (send normal-search-color red))
        (f (send normal-search-color green))
        (f (send normal-search-color blue)))))
  (define white-on-black-yellow-bubble-color (make-object color% 50 50 5))

  (define searching-mixin
    (mixin (editor:basic<%> editor:keymap<%> basic<%>) (searching<%>)
      (inherit get-start-position get-end-position
               unhighlight-ranges/key unhighlight-range highlight-range
               begin-edit-sequence end-edit-sequence
               find-string in-edit-sequence? get-canvas get-top-level-window)
    
      (define has-focus? #f)
      (define clear-yellow void)
      (define searching-str #f)
      (define case-sensitive? #f)
      (define search-hit-count 0)
      (define before-caret-search-hit-count 0)
      (define search-coroutine #f)
    
      (define update-replace-bubble-callback-running? #f)
      (define search-position-callback-running? #f)
      
      (define anchor-pos #f)
    
      ;; replace-mode? : boolean?
      ;; #t if the replace portion of the GUI is visible
      ;; (and thus we have light/dark bubbles)
      (define replace-mode? #f)
    
      ;; to-replace-highlight : (or/c #f (cons/c number number))
      ;; the location where the next replacement will happen, or #f
      ;; if there isn't one (in case the insertion point is past
      ;; the last search hit, or replace-mode? is #f)
      ;; invariant: to-replace-highlight is not mapped in search-bubble-table
      ;;            (even though it is a legtimate hit)
      (define to-replace-highlight #f)
    
      ;; search-bubble-table : hash-table[(cons number number) -o> #t]
      (define search-bubble-table (make-hash))
    
      ;; get-replace-search-hit : -> (or/c number #f)
      ;; returns the nearest search hit after `replace-start'
      (define/public (get-replace-search-hit) 
        (and searching-str
             to-replace-highlight
             (car to-replace-highlight)))

      ;; NEW METHOD: used for test suites
      (define/public (search-updates-pending?)
        (or update-replace-bubble-callback-running?
            search-position-callback-running?
            search-coroutine))
    
      (define/public (set-replace-start n) (void))
    
      (define/public (get-anchor-pos) anchor-pos)

      (define/public (set-search-anchor position)
        (begin-edit-sequence #t #f)
        (when anchor-pos (unhighlight-anchor))
        (cond
          [(and position
                (preferences:get 'framework:anchored-search))
           (set! anchor-pos position)
           (highlight-anchor)]
          [else
           (set! anchor-pos #f)])
        (end-edit-sequence))
    
      (define/public (get-search-hit-count) (values before-caret-search-hit-count search-hit-count))
    
      (define/public (set-searching-state s in-cs? in-r? [notify-frame? #f])
        (define r? (and in-r? #t))
        (define cs? (and in-cs? #t))
        (unless (and (equal? searching-str s)
                     (equal? case-sensitive? cs?)
                     (equal? r? replace-mode?))
          (set! searching-str s)
          (set! case-sensitive? cs?)
          (set! replace-mode? r?)
          (redo-search notify-frame?)))
    
      (define/override (get-keymaps)
        (editor:add-after-user-keymap (keymap:get-search) (super get-keymaps)))
    
      (define/augment (after-insert start len)
        (when searching-str
          (redo-search #t))
        (inner (void) after-insert start len))
      (define/augment (after-delete start len)
        (when searching-str
          (redo-search #t))
        (inner (void) after-delete start len))
    
      (define/override (on-focus on?)
        (let ([f (get-top-level-window)])
          (when (is-a? f frame:searchable<%>)
            (set! has-focus? on?)
            (cond
              [on?
               ;; this triggers a call to update-yellow
               (send f set-text-to-search this)]
              [else
               (update-yellow)])))
        (super on-focus on?))
    
      (define/augment (after-set-position)
        (update-yellow)
        (maybe-queue-update-replace-bubble)
        (maybe-queue-search-position-update)
        (inner (void) after-set-position))
    
      (define/private (maybe-queue-update-replace-bubble)
        (unless update-replace-bubble-callback-running?
          (set! update-replace-bubble-callback-running? #t)
          (queue-callback
           (λ () 
             (set! update-replace-bubble-callback-running? #f)
             (unless search-coroutine
               ;; the search co-routine will update
               ;; the replace bubble to its proper color
               ;; before it finishes so we can just let
               ;; do this job
             
             
               (define (replace-highlight->normal-hit)
                 (when to-replace-highlight
                   (let ([old-to-replace-highlight to-replace-highlight])
                     (unhighlight-replace)
                     (highlight-hit old-to-replace-highlight))))
             
               (cond
                 [(or (not searching-str)
                      (not replace-mode?))
                  (when to-replace-highlight
                    (unhighlight-replace))]
                 [else
                  (define next (do-search (get-start-position)))
                  (begin-edit-sequence #t #f)
                  (cond
                    [next
                     (define-values (txt-t start-t end-t)
                       (if to-replace-highlight
                           (get-highlighting-text-and-range to-replace-highlight)
                           (values #f #f #f)))
                     (define next-to-replace-highlight (cons next (string-length searching-str)))
                     (define-values (txt-n start-n end-n)
                       (get-highlighting-text-and-range next-to-replace-highlight))
                     (cond
                       [(and (object-or-false=? txt-t txt-n)
                             (equal? start-t start-n)
                             (equal? end-t end-n))
                        ;; here the hit remains the same, so we do nothing
                        (void)]
                       [else
                        ;; here we have to move the next-hit search bubble
                        (replace-highlight->normal-hit)
                        (unhighlight-hit next-to-replace-highlight)
                        (highlight-replace next-to-replace-highlight)])]
                    [else
                     ;; here the next-search hit converts to a a regular one
                     ;; but we don't have another one to highlight
                     (replace-highlight->normal-hit)])
                  (end-edit-sequence)])))
           #f)))
    
      ;; maybe-queue-editor-position-update : -> void
      ;; updates the editor-position in the frame,
      ;; but delays it until the next low-priority event occurs.
      (define/private (maybe-queue-search-position-update)
        (unless search-position-callback-running?
          (set! search-position-callback-running? #t)
          (queue-callback
           (λ ()
             (when searching-str
               (define start-pos (get-focus-editor-start-position))
               (define (how-many-to-add k)
                 (if (search-result-compare <= (car k) start-pos) 1 0))
               (define count
                 (+ (if to-replace-highlight
                        (how-many-to-add to-replace-highlight)
                        0)
                    (for/sum ([(k v) (in-hash search-bubble-table)])
                      (how-many-to-add k))))
               (update-before-caret-search-hit-count count))
             (set! search-position-callback-running? #f))
           #f)))

      (define/private (get-focus-editor-start-position)
        (let loop ([txt this])
          (define focus (send txt get-focus-snip))
          (define embedded
            (and focus
                 (is-a? focus editor-snip%)
                 (is-a? (send focus get-editor) text%)
                 (send focus get-editor)))
          (cond
            [embedded
             (cons embedded (loop embedded))]
            [else (send txt get-start-position)])))
    
      (define/private (update-before-caret-search-hit-count c)
        (unless (equal? before-caret-search-hit-count c)
          (set! before-caret-search-hit-count c)
          (let ([tlw (get-top-level-window)])
            (when (is-a? tlw frame:searchable<%>)
              (send tlw search-hits-changed)))))
    
      (define/private (update-yellow)
        (cond
          [has-focus?
           (unless (eq? clear-yellow void)
             (clear-yellow)
             (set! clear-yellow void))]
          [searching-str
           (let ([start (get-start-position)]
                 [end (get-end-position)])
             (cond
               [(= start end)
                (clear-yellow)
                (set! clear-yellow void)]
               [else
                (begin-edit-sequence #t #f)
                (clear-yellow)
                (set! clear-yellow void)
                (when (and searching-str (= (string-length searching-str) (- end start)))
                  (when (find-string searching-str 'forward start end #t case-sensitive?)
                    (set! clear-yellow (highlight-range
                                        start end
                                        (if (preferences:get 'framework:white-on-black?)
                                            white-on-black-yellow-bubble-color
                                            "khaki")
                                        #f 'low 'ellipse))))
                (end-edit-sequence)]))]
          [else
           (clear-yellow)
           (set! clear-yellow void)]))

      (define/public (get-search-bubbles)
        (sort 
         (append
          (if to-replace-highlight
              (list (list to-replace-highlight 'dark-search-color))
              (list))
          (hash-map search-bubble-table
                    (λ (x _true)
                      (list x (if replace-mode? 'light-search-color 'normal-search-color)))))
         string<?
         #:key (λ (x) (format "~s" (car x)))))
    
    
      (define/private (redo-search notify-frame?)
        (define old-search-coroutine search-coroutine)
        (set! search-coroutine (create-search-coroutine notify-frame?))
        (unless old-search-coroutine 
          ;; when old-search-coroutine is not #f, then
          ;; we know that there is already a callback
          ;; pending; the set! above just change what 
          ;; it will be doing.
          (queue-callback (λ () (run-search)) #f)))
    
      (define/private (run-search)
        ;; there may be a call to (finish-pending-search-work) with a run-search
        ;; pending so we check to see if that happened and do no work in that case.
        (when search-coroutine
          (define done? (coroutine-run search-coroutine (void)))
          (cond
            [done?
             (set! search-coroutine #f)]
            [else
             (queue-callback
              (λ () (run-search))
              #f)])))
    
      (define/public (finish-pending-search-work)
        (when search-coroutine
          (let loop ()
            (define done? (coroutine-run search-coroutine (void)))
            (cond
              [done?
               (set! search-coroutine #f)]
              [else
               (loop)]))))
    
      (define/private (create-search-coroutine notify-frame?)
        (coroutine
         pause
         first-val
         (define start-time (current-inexact-milliseconds))
         (define did-something? #f)
         (define (maybe-pause)
           (cond
             [(not did-something?)
              (set! did-something? #t)]
             [((+ start-time 30) . < . (current-inexact-milliseconds))
              (define was-in-edit-sequence? (in-edit-sequence?))
              (when was-in-edit-sequence?
                (end-edit-sequence))
              (pause)
              (when was-in-edit-sequence?
                (begin-edit-sequence #t #f))
              (set! did-something? #f)
              (set! start-time (current-inexact-milliseconds))
              #t]
             [else #f]))
         
         (cond
           [searching-str
            (define new-search-bubbles '())
            (define new-replace-bubble #f)
            (define first-hit (do-search 0))
         
            (define-values (this-search-hit-count this-before-caret-search-hit-count)
              (cond
                [first-hit
                 (define sp (get-focus-editor-start-position))
                 (let loop ([bubble-start first-hit]
                            [search-hit-count 0]
                            [before-caret-search-hit-count
                             (if (search-result-compare < first-hit sp) 1 0)])
                   (maybe-pause)
                   (define bubble-end (search-result+ bubble-start (string-length searching-str)))
                   (define bubble (cons bubble-start (string-length searching-str)))
                   (define this-bubble
                     (cond
                       [(and replace-mode?
                             (not new-replace-bubble)
                             (search-result-compare <= sp bubble-start))
                        (set! new-replace-bubble bubble)
                        'the-replace-bubble]
                       [else
                        bubble]))
                   (set! new-search-bubbles (cons this-bubble new-search-bubbles))

                   (define next (do-search bubble-end))
                
                   (define next-before-caret-search-hit-count
                     (if (and next (search-result-compare < next sp))
                         (+ 1 before-caret-search-hit-count)
                         before-caret-search-hit-count))
                   (cond
                     [next
                      ;; start a new one if there is another hit
                      (loop next 
                            (+ search-hit-count 1)
                            next-before-caret-search-hit-count)]
                     [else
                      (values (+ search-hit-count 1) 
                              before-caret-search-hit-count)]))]
                [else (values 0 0)]))
         
            (set! search-hit-count this-search-hit-count)
            (set! before-caret-search-hit-count this-before-caret-search-hit-count)
         
            (maybe-pause)
         
            (begin-edit-sequence #t #f)
            (clear-all-regions)
         
            (maybe-pause)
         
            (for ([search-bubble (in-list (reverse new-search-bubbles))])
              (cond
                [(eq? search-bubble 'the-replace-bubble)
                 (highlight-replace new-replace-bubble)]
                [else
                 (highlight-hit search-bubble)])
              (maybe-pause))
         
            (update-yellow) 
            (end-edit-sequence)]
           [else
            (begin-edit-sequence #t #f)
            (clear-all-regions)
            (set! search-hit-count 0)
            (set! before-caret-search-hit-count 0)
            (update-yellow) 
            (end-edit-sequence)])
         (when notify-frame?
           (define canvas (get-canvas))
           (when canvas
             (let loop ([w canvas])
               (cond
                 [(is-a? w frame:searchable<%>)
                  (send w search-hits-changed)]
                 [(is-a? w area<%>)
                  (loop (send w get-parent))]))))))

      (define/private (search-result+ search-result num)
        (let loop ([search-result search-result])
          (cond
            [(number? search-result) (+ search-result num)]
            [(pair? search-result)
             (cons (car search-result)
                   (loop (cdr search-result)))])))

      (define/private (search-result-compare lt l r)
        (let loop ([txt this]
                   [l l]
                   [r r])
          (define (get-the-position x)
            ;; the zeros shouldn't happen because the editors should still
            ;; be in the main text object while we are doing stuff with them
            (define admin (send x get-admin))
            (cond
              [(is-a? admin editor-snip-editor-admin<%>)
               (or (send txt get-snip-position (send admin get-snip)) 0)]
              [else
               0]))
          (cond
            [(and (number? l) (number? r)) (lt l r)]
            [(or (number? l) (number? r))
             (define ln (if (number? l) l (get-the-position (car l))))
             (define rn (if (number? r) r (get-the-position (car r))))
             (lt ln rn)]
            [else
             (cond
               [(equal? (car l) (car r))
                (loop (car l) (cdr l) (cdr r))]
               [else
                (lt (get-the-position (car l))
                    (get-the-position (car r)))])])))

      (define all-txt-with-regions-to-clear (make-hasheq))
      (define/private (clear-all-regions) 
        (when to-replace-highlight 
          (unhighlight-replace))
        (for ([(txt _) (in-hash all-txt-with-regions-to-clear)])
          (send txt unhighlight-ranges/key 'plt:framework:search-bubbles))
        (set! all-txt-with-regions-to-clear (make-hasheq))
        (set! search-bubble-table (make-hash)))

      ;; do-search : context+search-position -> context+search-position
      ;; does a search starting at `start` and returning the next
      ;; ocurrence of the search string, possibly moving out into
      ;; later editors
      ;; the context+search-position is list the result of `find-string-embedded`,
      ;; except it cannot be #f.
      (define/private (do-search start)
        (define context (list this))
        (define position
          (let loop ([start start])
            (cond
              [(number? start) start]
              [else
               (set! context (cons (car start) context))
               (loop (cdr start))])))
        (let loop ([position position]
                   [context context])
          (define found-at-this-level
            (send (car context) find-string-embedded
                  searching-str 'forward position 'eof #t case-sensitive?))
          (cond
            [found-at-this-level
             (let loop ([context (cdr (reverse context))])
               (cond
                 [(null? context) found-at-this-level]
                 [else (cons (car context)
                             (loop (cdr context)))]))]
            [(null? (cdr context)) #f]
            [else
             (define admin (send (car context) get-admin))
             (cond
               [(is-a? admin editor-snip-editor-admin<%>)
                (define snip (send admin get-snip))
                (loop (+ (send (second context) get-snip-position snip)
                         (send snip get-count))
                      (cdr context))]
               [else
                (error 'framework/private/text.rkt::searching "admin went wrong ~s" admin)])])))
    
      ;; INVARIANT: when a search bubble is highlighted,
      ;; the search-bubble-table has it mapped to #t
      ;; the two methods below contribute to this, but
      ;; so does the 'clear-all-regions' method above
    
    
      ;; this method may be called with bogus inputs (ie a pair that has no highlight)
      ;; but only when there is a pending "erase all highlights and recompute everything" callback
      (define/private (unhighlight-hit bubble)
        (hash-remove! search-bubble-table bubble)
        (define-values (txt start end) (get-highlighting-text-and-range bubble))
        (when txt
          (send txt unhighlight-range
                start end
                (if replace-mode? light-search-color normal-search-color)
                #f
                'hollow-ellipse)))
      (define/private (highlight-hit bubble)
        (hash-set! search-bubble-table bubble #t)
        (define-values (txt start end) (get-highlighting-text-and-range bubble))
        (when txt
          (hash-set! all-txt-with-regions-to-clear txt #t)
          (send txt highlight-range
                start end
                (if replace-mode? light-search-color normal-search-color)
                #f
                'low
                'hollow-ellipse
                #:key 'plt:framework:search-bubbles
                #:adjust-on-insert/delete? #t)))
    
      ;; INVARIANT: the "next to replace" highlight is always
      ;; saved in 'to-replace-highlight'
      (define/private (unhighlight-replace)
        (define-values (txt start end) (get-highlighting-text-and-range to-replace-highlight))
        (when txt
          (send txt unhighlight-range
                start end
                dark-search-color
                #f
                'hollow-ellipse))
        (unless (equal? to-replace-highlight #f)
          (set! to-replace-highlight #f)
          (maybe-queue-search-position-update)))
    
      (define/private (highlight-replace new-to-replace)
        (unless (equal? new-to-replace to-replace-highlight)
          (set! to-replace-highlight new-to-replace)
          (maybe-queue-search-position-update)
          (define-values (txt start end) (get-highlighting-text-and-range new-to-replace))
          (when txt
            (send txt highlight-range
                  start end
                  dark-search-color
                  #f
                  'high
                  'hollow-ellipse))))

      (define/private (get-highlighting-text-and-range bubble)
        (let loop ([txt this]
                   [txt/pr (car bubble)])
          (cond
            [(number? txt/pr)
             (if (is-a? txt text:basic<%>)
                 (values txt txt/pr (+ txt/pr (cdr bubble)))
                 (values #f #f #f))]
            [else (loop (car txt/pr) (cdr txt/pr))])))
    
      (define/private (unhighlight-anchor)
        (unhighlight-range anchor-pos anchor-pos "red" #f 'dot)
        (unhighlight-range anchor-pos anchor-pos "red"))
    
      (define/private (highlight-anchor)
        (highlight-range anchor-pos anchor-pos "red" #f 'low 'dot)
        (highlight-range anchor-pos anchor-pos "red"))
    
      (super-new))))
