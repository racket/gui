#lang typed/racket/base
;; owner: ryanc
(require typed/racket/class
         typed/racket/gui/base
         "private/notify.rkt")
(provide (prefix-out notify:
                     (combine-out (all-from-out "private/notify.rkt")
                                  menu-option/notify-box
                                  menu-group/notify-box
                                  check-box/notify-box
                                  choice/notify-box)))

;; GUI elements tied to notify-boxes
;; See private/notify.rkt for the non-gui parts of notify-boxes.

(: menu-option/notify-box (All (T) (-> (U (Instance Menu%) (Instance Popup-Menu%)) String (Instance (Notify-Box% Boolean))
                                  (Instance Checkable-Menu-Item%))))
(define (menu-option/notify-box parent label nb)
  (: menu-item : (Instance Checkable-Menu-Item%))
  (define menu-item
    (new checkable-menu-item%
         (label label)
         (parent parent)
         (demand-callback
          (lambda (i)
            (send i check (send nb get))))
         (callback
          (lambda _ 
            #;(send nb set (send menu-item is-checked?))
            (send nb set (not (send nb get)))))))
  menu-item)

(: check-box/notify-box (All (T) (-> (U (Instance Frame%) (Instance Dialog%) (Instance Pane%) (Instance Panel%))
                               String (Instance (Notify-Box% Boolean)) (Instance Check-Box%))))
(define (check-box/notify-box parent label nb)
  (define checkbox
    (new check-box%
         (label label)
         (parent parent)
         (value (send nb get))
         (callback
          (lambda (c e) (send nb set (send c get-value))))))
  (send nb listen (lambda (value) (send checkbox set-value value)))
  checkbox)

(: choice/notify-box (All (T) (-> (U (Instance Frame%) (Instance Dialog%) (Instance Pane%) (Instance Panel%))
                               String (Listof String) (Instance (Notify-Box% (U String))) (Instance Choice%))))
(define (choice/notify-box parent label choices nb)
  (define choice
    (new choice%
         (label label)
         (choices choices)
         (parent parent)
         ;; moved style 
         ;; moved (choices choices)
         (callback (lambda (c e) (send nb set (assert (send c get-string-selection)))))
         (style '(horizontal-label))))
  (send choice set-string-selection (send nb get))
  (send nb listen (lambda: ([value : String]) (send choice set-string-selection value)))
  choice)

(: menu-group/notify-box (All (T) (-> (U (Instance Menu%) (Instance Popup-Menu%)) (Pair String (Listof String)) (Instance (Notify-Box% (U String (Pair String (Listof String)))))
                                (Listof (Instance Checkable-Menu-Item%)))))
(define (menu-group/notify-box parent labels nb)
  (map (lambda: ([option : (U String (Pair String (Listof String)))])
         (: label String) ;; can be null form (Listof String)
         (define label (if (pair? option) (car option) option))
         (: menu-item (Instance Checkable-Menu-Item%))
         (define menu-item
           (new checkable-menu-item%
                (label label)
                (parent parent)
                (checked (eq? (send nb get) option)) ;; bug? 
                (callback
                 (lambda _ (send nb set option)))))
         (send nb listen
               (lambda (value) (send menu-item check (eq? value option))))
         menu-item)
       labels))
