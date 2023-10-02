#lang racket/base
(require data/skip-list
         racket/pretty
         racket/class
         racket/list
         racket/match
         racket/dict
         racket/unit
         mred/mred-sig
         "guide-struct.rkt"
         "text-sig.rkt"
         "sig.rkt")

(provide text-indent-guides@)

(define-unit text-indent-guides@
  (import mred^)
  (export text-indent-guides^)

  ;; todo:
  ;;  - reverse the list sort order in the `guides` field of the
  ;;    `guide` struct? (can maybe get more sharing in the tails)
  ;;  - should we take cues for the colors from the syntax coloring?

  ;; the implementation splits the work of the guides into two pieces:
  ;;  - building of the `guides` (see the `guide` struct in guide-struct.rkt
  ;;    and the adjustable-skip-list in `guides`) that tracks where guides
  ;;    are on each line
  ;;  - drawing the `guides` in response to an invalidated region
  ;;    of the editor (see on-paint and following methods)

  (define indent-guides<%>
    (interface ()
      show-indent-guides!
      show-indent-guides?))

  (define indent-guides-mixin
    (mixin ((class->interface text%)) (indent-guides<%>)
      (inherit paragraph-start-position
               position-paragraph
               last-paragraph
               position-location
               find-snip
               find-position
               begin-edit-sequence
               end-edit-sequence
               in-edit-sequence?
               invalidate-bitmap-cache)

      (define on-delete-contract-start #f)
      (define on-delete-contract-end #f)
      (define initial-pending-lines-cache-key #f)
      (define initial-pending-lines-cache #f)
      (define/private (reset-initial-pending-lines-cache)
        (set! initial-pending-lines-cache-key #f)
        (set! initial-pending-lines-cache #f))

      (define find-indent-cache (make-string 0))

      ;; boolean?
      ;; #t => call recalculate-x-for-guides at the
      ;;       end of the enclosing edit-sequence
      (define recalculate-x-for-guides-after-edit-sequence #f)

      ;; para -o> guide
      (define guides #f)
      (define/public-final (get-guides) guides)
      (define/public-final (show-indent-guides! on?)
        (cond
          [on?
           (unless guides
             (set! guides (make-adjustable-skip-list))
             (recalculate-lines-guides 0 (last-paragraph)))]
          [else
           (when guides
             (set! guides #f)
             (invalidate-bitmap-cache))]))
      (define/public (show-indent-guides?) (and guides #t))

      (super-new)
      (show-indent-guides! #t)

      ;                                                    
      ;                                                    
      ;                                                    
      ;                                                    
      ;                                                    
      ;                     ;;;      ;;;                   
      ;                              ;;;                   
      ;    ;; ;;;  ;;; ;;;  ;;;   ;; ;;;    ;;;     ;;;;;  
      ;   ;;;;;;;  ;;; ;;;  ;;;  ;;;;;;;   ;;;;;   ;;;  ;; 
      ;   ;;; ;;;  ;;; ;;;  ;;;  ;;; ;;;  ;;; ;;;  ;;;     
      ;   ;;; ;;;  ;;; ;;;  ;;;  ;;; ;;;  ;;; ;;;  ;;;;;;  
      ;   ;;; ;;;  ;;; ;;;  ;;;  ;;; ;;;  ;;;;;;;   ;;;;;; 
      ;   ;;; ;;;  ;;; ;;;  ;;;  ;;; ;;;  ;;;          ;;; 
      ;   ;;;;;;;  ;;;;;;;  ;;;  ;;;;;;;   ;;;;;;  ;;  ;;; 
      ;    ;; ;;;   ;; ;;;  ;;;   ;; ;;;    ;;;;    ;;;;;  
      ;       ;;;                                          
      ;   ;;;;;;;                                          
      ;    ;;;;;                                           
      ;                                                    
      ;                                                    

    
      ;; recalculate-lines-guides rebuilds the content of `guides`
      ;; when the text's content changes; it is the top-level entry
      ;; point into the code that builds `guides` but it also
      ;; invalidates drawing regions that have to change
      (define/private (recalculate-lines-guides para-start para-end)
        (when guides
          (define bx (box 0))
          (define-values (para-stopped invalidate-right)
            (let loop ([para para-start]
                       [widest-width 0]

                       [previous-guide
                        (if (= para-start 0)
                            #f
                            (skip-list-ref guides (- para-start 1) #f))]
                       ;; pending-blanks-start :
                       ;;  (or/c #f        -- no pending blanks
                       ;;        natural?) -- which para the blanks go back to
                       [pending-blanks-start #f])
              (cond
                [(<= para (last-paragraph))
                 (define-values (width changed/blank?)
                   (recalculate-line-guide bx previous-guide para))
                 (cond
                   [(or (para . <= . para-end)
                        changed/blank?)
                    (define-values (new-previous-guide new-pending-blanks)
                      (match changed/blank?
                        [(? boolean?)
                         (when pending-blanks-start
                           (update-the-blanks para previous-guide pending-blanks-start))
                         (values (skip-list-ref guides para) #f)]
                        ['blank
                         (cond
                           [pending-blanks-start
                            (values previous-guide pending-blanks-start)]
                           [else
                            (values previous-guide para)])]))
                    (loop (+ para 1)
                          (max width widest-width)
                          new-previous-guide
                          new-pending-blanks)]
                   [else
                    ;; when we're outside of the edited region
                    ;; and the guide is the same as before, we're
                    ;; not going to find any more differences
                    (when pending-blanks-start
                      (update-the-blanks para previous-guide pending-blanks-start))
                    (values para widest-width)])]
                [else
                 (when pending-blanks-start
                   (update-the-blanks #f previous-guide pending-blanks-start))
                 (values (last-paragraph) widest-width)])))
          (position-location (paragraph-start-position para-start) #f bx #t #f #t)
          (define invalidate-top (unbox bx))
          (position-location (paragraph-start-position para-stopped) #f bx #f #f #t)
          (define invalidate-bottom (unbox bx))
          (invalidate-bitmap-cache 0 invalidate-top invalidate-right
                                   (- invalidate-bottom invalidate-top))

          ;; probably don't need to do this but just in case
          ;; there is a giant snip somewhere along the way
          (set! find-indent-cache (make-string 0))))

      ;; update-the-blanks : (or/c #f para) (or/c #f guide) para -> void
      ;; all of the lines from `pending-blanks-start` forward
      ;; to `(- guide-para 1)` are blank (or, if `guide-para` is #f, then all the way
      ;; to the end). Create guides for them based on the guides at
      ;; subsequent-guide-para and previous-guide
      (define/private (update-the-blanks subsequent-guide-para
                                         previous-guide
                                         pending-blanks-start)
        (define loop-termination-para
          (or subsequent-guide-para
              (+ (last-paragraph) 1)))
        (define subsequent-guide
          (if subsequent-guide-para
              (skip-list-ref guides subsequent-guide-para)
              #f))
        (define (common-prefix orig-l1 orig-l2)
          (let loop ([l1 orig-l1][l2 orig-l2])
            (cond
              [(or (empty? l1) (empty? l2)) '()]
              [else
               (cond
                 [(= (car l1) (car l2))
                  (cons (car l1) (loop (cdr l1) (cdr l2)))]
                 [else '()])])))
        (define proto-guide
          (cond
            [(or (not subsequent-guide)
                 (not previous-guide))
             (guide #f #f '())]
            [else
             (define subsequent-candidates
               (if (= (guide-indent subsequent-guide) 0)
                   (guide-guides subsequent-guide)
                   (append (guide-guides subsequent-guide) (list (guide-indent subsequent-guide)))))
             (define prefix-candidates
               (if (= 0 (guide-indent previous-guide))
                   (guide-guides previous-guide)
                   (append (guide-guides previous-guide) (list (guide-indent previous-guide)))))
             (guide #f
                    #f
                    (common-prefix prefix-candidates
                                   subsequent-candidates))]))

        (for ([para (in-range pending-blanks-start loop-termination-para)])
          (skip-list-set! guides para (struct-copy guide proto-guide))))

      ;; -> (values integer? (or/c 'blank boolean?))
      ;; the second result indicates if the guide changed (blanks count
      ;; as always changing just to make life simpler)
      ;; the first, integer result is is the larger of the previous
      ;; or new width (or just the width if nothing changed)
      ;; if the second result is a boolean, then
      ;;   the guides skip-list has a valid guide
      ;;   set at the position `para`.
      ;; if the second result is 'blank, then the
      ;;   guides skip-list was not updated and
      ;;   needs to be updated later on (when
      ;;   we find the end of the blanks)
      (define/private (recalculate-line-guide bx previous-guide para)
        (define-values (para-start indent) (find-indent para))
        (cond
          [indent
           (calculate-guide-x para-start indent bx)
           (define a-guide
             (guide indent
                    (unbox bx)
                    (if previous-guide
                        (forward-guides indent
                                        (guide-indent previous-guide)
                                        (guide-guides previous-guide))
                        '())))
           (define before (skip-list-ref guides para #f))
           (cond
             [(equal? before a-guide)
              (values (guide-x before) #f)]
             [else
              (skip-list-set! guides para a-guide)
              (cond
                [before
                 (values (max (unbox bx) (or (guide-x before) 0)) #t)]
                [else
                 (values (unbox bx) #t)])])]
          [else
           ;; if we have a blank line then we punt the decision
           ;; about what to do back to the caller of this function
           (cond
             [previous-guide
              (values (guide-x previous-guide) 'blank)]
             [else
              (values 0 'blank)])]))

      ;; natural natural (listof sorted-natural) -> (listof softed natural)
      ;; this calculates the guides for a new line that
      ;; has the indent `this-line-indent` and where the
      ;; previous lines's indent and guides are given
      (define/public (forward-guides this-line-indent
                                     previous-line-indent
                                     previous-line-guides)
        (define spots-to-consider
          (cond
            [(= previous-line-indent 0) '()]
            [(member previous-line-indent previous-line-guides) previous-line-guides]
            [else (append previous-line-guides (list previous-line-indent))]))
        (let loop ([spots-to-consider spots-to-consider])
          (cond
            [(null? spots-to-consider) '()]
            [else
             (define spot (car spots-to-consider))
             (if (<= this-line-indent spot)
                 '()
                 (cons spot (loop (cdr spots-to-consider))))])))

      ;; given a paragraph, this function calculates the indent; this is
      ;; mostly a matter of counting spaces, but is slightly complicated to
      ;; avoid allocation and use fast paths in the text% object
      (define/private (find-indent para)
        (define para-start (paragraph-start-position para))
        (let loop ([snip (find-snip para-start 'after-or-none)]
                   [indent 0])
          (cond
            [(is-a? snip string-snip%)
             (define count (send snip get-count))
             (unless (<= count (string-length find-indent-cache))
               (set! find-indent-cache (make-string (* 2 count))))
             (send snip get-text! find-indent-cache 0 count 0)
             (cond
               [(and (= count 1) (equal? (string-ref find-indent-cache 0) #\newline))
                ;; here we found a string snip with a newline so this means the
                ;; line has terminated and all of the characters in it where spaces
                (values para-start #f)]
               [else
                (let char-loop ([i 0])
                  (cond
                    [(< i count)
                     (if (equal? #\space (string-ref find-indent-cache i))
                         (char-loop (+ i 1))
                         (values para-start (+ indent i)))]
                    [else
                     (loop (send snip next) (+ indent count))]))])]
            [(not snip) (values para-start #f)]
            [else (values para-start indent)])))

      ;; when the sizing information has changed, this function
      ;; recomputes the editor coordinates that are stored in `guides`
      (define/private (recalculate-x-for-guides)
        (define bx (box 0))
        (for ([(para guide) (in-dict guides)])
          (define para-start (paragraph-start-position para))
          (when (guide-indent guide)
            (calculate-guide-x para-start (guide-indent guide) bx)
            (set-guide-x! guide (unbox bx)))))
      (define/private (trigger-recalculate-x-for-guides)
        (cond
          [(in-edit-sequence?)
           (set! recalculate-x-for-guides-after-edit-sequence #t)]
          [else
           (recalculate-x-for-guides)]))
    
      (define/private (calculate-guide-x para-start indent bx)
        (position-location (+ para-start indent) bx #f #t #t))

      ;; the methods below override callback to manage the guides state
      (define/augment (after-insert start len)
        (when guides
          (reset-initial-pending-lines-cache)
          (define para1 (position-paragraph start))
          (define para2 (position-paragraph (+ start len)))
          (skip-list-expand! guides para1 para2)
          (recalculate-lines-guides (scan-blank-backwards para1) para2))
        (inner (void) after-insert start len))
      (define/augment (on-delete start len)
        (when guides
          (begin-edit-sequence)
          (set! on-delete-contract-start (position-paragraph start))
          (set! on-delete-contract-end (position-paragraph (+ start len))))
        (inner (void) on-delete start len))
      (define/augment (after-delete start len)
        (when guides
          (reset-initial-pending-lines-cache)
          (skip-list-contract! guides on-delete-contract-start on-delete-contract-end)
          (recalculate-lines-guides (scan-blank-backwards on-delete-contract-start) on-delete-contract-start)
          (end-edit-sequence))
        (inner (void) after-delete start len))
      (define/augment (on-change)
        (when guides
          (reset-initial-pending-lines-cache)
          (trigger-recalculate-x-for-guides))
        (inner (void) on-change))
      (define/augment (on-reflow)
        (when guides
          (reset-initial-pending-lines-cache)
          (trigger-recalculate-x-for-guides))
        (inner (void) on-reflow))
      (define/augment (after-edit-sequence)
        (when recalculate-x-for-guides-after-edit-sequence
          (set! recalculate-x-for-guides-after-edit-sequence #f)
          (recalculate-x-for-guides))
        (inner (void) after-edit-sequence))

      (define/private (scan-blank-backwards para)
        ;; scan backwards to include any blank paragraphs above `para`
        (let loop ([para para])
          (define para-above-blank?
            (and (not (zero? para))
                 (not (guide-indent (skip-list-ref guides (- para 1))))))
          (cond
            [para-above-blank?
             (loop (- para 1))]
            [else para])))

    
      ;                                                              
      ;                                                              
      ;                                                              
      ;                                                              
      ;                                                              
      ;       ;;;                              ;;;                   
      ;       ;;;                                                    
      ;    ;; ;;;  ;;; ;; ;;;;;  ;;;  ;;;  ;;; ;;;  ;;; ;;    ;; ;;; 
      ;   ;;;;;;;  ;;;;; ;;;;;;;  ;;  ;;;  ;;; ;;;  ;;;;;;;  ;;;;;;; 
      ;   ;;; ;;;  ;;;   ;;; ;;;  ;;; ;;; ;;;  ;;;  ;;; ;;;  ;;; ;;; 
      ;   ;;; ;;;  ;;;      ;;;;  ;;; ; ; ;;;  ;;;  ;;; ;;;  ;;; ;;; 
      ;   ;;; ;;;  ;;;    ;; ;;;   ;;;; ;;;;;  ;;;  ;;; ;;;  ;;; ;;; 
      ;   ;;; ;;;  ;;;   ;;; ;;;   ;;;;  ;;;   ;;;  ;;; ;;;  ;;; ;;; 
      ;   ;;;;;;;  ;;;   ;;;;;;;   ;;;   ;;;   ;;;  ;;; ;;;  ;;;;;;; 
      ;    ;; ;;;  ;;;    ;;;;;;    ;;   ;;;   ;;;  ;;; ;;;   ;; ;;; 
      ;                                                          ;;; 
      ;                                                      ;;;;;;; 
      ;                                                       ;;;;;  
      ;                                                              
      ;                                                              

    
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (when guides
          (unless before?
            (define pen-before (send dc get-pen))
            (send dc set-pen (send the-pen-list find-or-create-pen "gray" 1 'solid))
            (define lt-position (find-position left top))
            (define top-para (position-paragraph lt-position))
            (define lb-position (find-position left bottom))
            (define bot-para (position-paragraph lb-position))
            (draw-the-lines
             (λ (x-in-editor-coordinates x y-start y-end)
               (draw-a-line dc dx dy x-in-editor-coordinates x y-start y-end))
             top-para bot-para)
            (send dc set-pen pen-before)))
        (super on-paint before? dc left top right bottom dx dy draw-caret))

      (define/public (draw-the-lines draw-a-line top-para bot-para)
        ;; this iterates through the paragraphs, figuring out when
        ;; lines start and end and drawing them when they end
        ;; it is complicated by the fact that it draws only the
        ;; area between `top` and `bottom`

        ;; these are the accumulator variables for the loop:
        ;; pending-lines : hash[column-number -o> (cons editor-x para)]
        ;; - hash tracks the lines that we are going to draw but
        ;;   haven't gotten to the end of yet. When the line ends, we
        ;;   draw it and remove it from the hash. When one starts,
        ;;   add it to the hash
        ;; - domain is the column number of the line
        ;; - range is the `x` (editor) coordinate we draw, plus
        ;;   the paragraph where the line started
        ;; pending-xs : (listof column-number)
        ;; - the content of this list is the same as the domain of
        ;;   pending-lines but it is sorted from smallest to largest
        (define pending-lines (make-hash))
        (define pending-xs (guide-guides (skip-list-ref guides top-para)))

        ;; initialize `pending-lines` hash, which is a bit complex in
        ;; the case that we're not starting from the top of the file.
        ;; so, in the fear that it might take a noticeable amount of time
        ;; we cache the result (in a cache of size 1)
        (cond
          [(equal? initial-pending-lines-cache-key (cons top-para pending-xs))
           (set! pending-lines (hash-copy initial-pending-lines-cache))]
          [else
           (set! initial-pending-lines-cache-key (cons top-para pending-xs))
           (let loop ([pending-xs (reverse pending-xs)]
                      [para top-para])
             (unless (null? pending-xs)
               (when (< para 0)
                 (error 'text-indent-guides.rkt::internal-error
                        "did not find the starting editor-coordinate x and para for lines at ~s"
                        pending-xs))
               (define g (skip-list-ref guides para))
               (cond
                 [(and (guide-indent g) (= (guide-indent g) (car pending-xs)))
                  (hash-set! pending-lines (guide-indent g) (cons (guide-x g) top-para))
                  (loop (cdr pending-xs) (- para 1))]
                 [else
                  (loop pending-xs (- para 1))])))
           (set! initial-pending-lines-cache (hash-copy pending-lines))])

        (for ([para (in-range top-para (+ bot-para 1))])
          (define guide (skip-list-ref guides para #f))
          (define new-pending-xs
            (let loop ([pending-xs pending-xs]
                       [guide-xs (guide-guides guide)])
              (cond
                [(null? guide-xs)
                 ;; the remaining pending-xs have ended, draw the actual lines
                 (draw-ended-pending-xs draw-a-line pending-xs pending-lines (- para 1))
                 '()]
                [(null? pending-xs)
                 ;; the remaining guide-xs are new lines starting
                 (unless (or (null? guide-xs)
                             (null? (cdr guide-xs)))
                   (error 'text-indent-guides.rkt::internal-error
                          (string-append
                           "expected only one line to start at a time"
                           "\n  para: ~s\n  guide-xs: ~s\n  guides:\n  ~a")
                          para
                          guide-xs
                          (regexp-replace*
                           #rx"\n"
                           (pretty-format (skip-list->list guides)
                                          #:mode 'write)
                           "\n  ")))
                 (when (pair? guide-xs)
                   (define x (guide-x (skip-list-ref guides (- para 1))))
                   (hash-set! pending-lines (car guide-xs) (cons x para)))
                 guide-xs]
                [else
                 (unless (= (car pending-xs) (car guide-xs))
                   (error 'text-indent-guides.rkt::internal-error
                          "pending-xs started with ~a but guide-xs started with ~a"
                          (car pending-xs)
                          (car guide-xs)))
                 (cons (car pending-xs) (loop (cdr pending-xs) (cdr guide-xs)))])))
          (set! pending-xs new-pending-xs))
        ;; here all of the remainding pending lines have finished (possibly because
        ;; we're redrawing only a portion the screen), so draw them.
        (draw-ended-pending-xs draw-a-line pending-xs pending-lines bot-para))

      (define/private (draw-ended-pending-xs draw-a-line finished-xs pending-lines para)
        (for ([finished-x (in-list finished-xs)])
          (match-define (cons x-in-editor-coordinates y-start) (hash-ref pending-lines finished-x))
          ;; this line no longer has the guide, end at the previous para
          (define line-to-start-drawing-at y-start)
          (draw-a-line x-in-editor-coordinates finished-x line-to-start-drawing-at para)
          (hash-remove! pending-lines finished-x)))

      (define/private (draw-a-line dc dx dy x-in-editor-coordinates x y-start y-end)
        (define by (box 0))
        (define sp (paragraph-start-position y-start))
        (position-location sp #f by #t #f #t)
        (define sy (unbox by))
        (define ep (paragraph-start-position y-end))
        (position-location ep #f by #f #f #t)
        (define ey (unbox by))
        (send dc draw-line
              ;; subtract 1 to not overlap the insertion point
              (+ dx -1 x-in-editor-coordinates)
              (+ dy sy)
              (+ dx -1 x-in-editor-coordinates)
              (+ dy ey (if (= y-end (last-paragraph)) -1 0)))))))
