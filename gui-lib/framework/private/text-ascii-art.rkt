#lang racket/base
(require racket/unit
         racket/class
         mred/mred-sig
         "text-sig.rkt"
         "interfaces.rkt"
         "../preferences.rkt"
         "unicode-ascii-art.rkt")
(provide text-ascii-art@)

(define-unit text-ascii-art@
  (import mred^)
  (export text-ascii-art^)

  (define ascii-art-enlarge-boxes<%> text:ascii-art-enlarge-boxes<%>)
  
  (define ascii-art-enlarge-boxes-mixin
    (mixin ((class->interface text%)) (ascii-art-enlarge-boxes<%>)
      (inherit get-overwrite-mode set-overwrite-mode
               get-start-position get-end-position set-position last-position
               get-character
               begin-edit-sequence end-edit-sequence
               position-paragraph paragraph-start-position)

      (define ascii-art-enlarge? (preferences:get 'framework:ascii-art-enlarge))
      (define/public (get-ascii-art-enlarge) ascii-art-enlarge?)
      (define/public (set-ascii-art-enlarge _e?)
        (define e? (and _e? #t))
        (preferences:set 'framework:ascii-art-enlarge e?)
        (set! ascii-art-enlarge? e?))

      (define/override (on-default-char c)
        (define kc (send c get-key-code))
        (define overwrite? (get-overwrite-mode))
        (cond
          [(not ascii-art-enlarge?) (super on-default-char c)]
          [(or (and (char? kc)
                    (not (member kc '(#\return #\tab #\backspace #\rubout))))
               (member (send c get-key-code)
                       going-to-insert-something))
           (begin-edit-sequence)
           (define pos (get-start-position))
           (define widen? (and (= pos (get-end-position))
                               (or (not overwrite?)
                                   (insertion-point-at-double-barred-char?))))
           (when widen?
             (define para (position-paragraph pos))
             (define delta-from-start (- pos (paragraph-start-position para)))
             (widen-unicode-ascii-art-box this pos)
             (define new-pos (+ (paragraph-start-position para) delta-from-start))
             (set-position new-pos new-pos))
           (unless overwrite? (set-overwrite-mode #t))
           (super on-default-char c)
           (unless overwrite? (set-overwrite-mode #f))
           (end-edit-sequence)]
          [else
           (super on-default-char c)]))

      (define/override (on-local-char c)
        (define kc (send c get-key-code))
        (define overwrite? (get-overwrite-mode))
        (cond
          [(not ascii-art-enlarge?) (super on-local-char c)]
          [(member kc '(numpad-enter #\return))
           (define pos (get-start-position))
           (cond
             [(= pos (get-end-position))
              (heighten-unicode-ascii-art-box this pos)
              (define pos-para (position-paragraph pos))
              (define pos-para-start (paragraph-start-position pos-para))
              (define next-para-start (paragraph-start-position (+ pos-para 1)))
              (define just-below-pos (+ next-para-start (- pos pos-para-start)))
              (define new-pos
                (let loop ([pos just-below-pos])
                  (cond
                    [(<= pos next-para-start)
                     pos]
                    [(equal? (get-character (- pos 1)) #\║)
                     pos]
                    [else (loop (- pos 1))])))
              (set-position new-pos new-pos)]
             [else
              (super on-local-char c)])]
          [else
           (super on-local-char c)]))

      (define/private (insertion-point-at-double-barred-char?)
        (define sp (get-start-position))
        (and (< sp (last-position))
             (equal? (get-character sp) #\║)))
    
      (super-new)))

  (define going-to-insert-something
    '(multiply
      add subtract decimal divide
      numpad0 numpad1 numpad2 numpad3 numpad4 numpad5 numpad6 numpad7 numpad8 numpad9)))
