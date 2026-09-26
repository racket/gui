#lang racket/base
(require racket/class
         racket/gui/base
         mrlib/hierlist
         rackunit)

;; A double-click on an item of a hierarchical list selects it with
;; `on-double-select`. Two single clicks at the same position, but on items
;; of different lists, are not a double-click; DrRacket's tests used to see
;; one when they clicked the same language in two successive "Choose
;; Language" dialogs, which closed the second dialog.

(define double-selected '())

(define (make-list name)
  (define f (new frame% [label name]))
  (define l
    (new (class hierarchical-list%
           (define/override (on-double-select i)
             (set! double-selected (cons (send i user-data) double-selected)))
           (super-new))
         [parent f]))
  (define i (send l new-item))
  (send i user-data name)
  (send (send i get-editor) insert name)
  i)

;; Deliver a left click at a fixed position to an item's editor, the way
;; the editor does, through its keymap
(define (click item time)
  (define editor (send item get-editor))
  (for ([type (in-list '(left-down left-up))])
    (send (send editor get-keymap) handle-mouse-event
          editor
          (new mouse-event%
               [event-type type]
               [left-down (eq? type 'left-down)]
               [x 5]
               [y 5]
               [time-stamp time]))))

(define a (make-list "a"))
(define b (make-list "b"))

(define t (current-milliseconds))
(click a t)
(click b (+ t 10))
(check-equal? double-selected '())

(set! double-selected '())
(click a (+ t 10000))
(click a (+ t 10010))
(check-equal? double-selected '("a"))
