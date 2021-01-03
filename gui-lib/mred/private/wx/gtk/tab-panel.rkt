#lang racket/base
(require racket/class
         ffi/unsafe
          "../../syntax.rkt"
         "window.rkt"
         "client-window.rkt"
         "utils.rkt"
         "panel.rkt"
         "types.rkt"
         "widget.rkt"
         "message.rkt"
         "../../lock.rkt"
         "../common/event.rkt")

(provide 
 (protect-out tab-panel%))

(define-gtk gtk_notebook_new (_fun -> _GtkWidget))

(define-gtk gtk_notebook_append_page (_fun _GtkWidget _GtkWidget (_or-null _GtkWidget) -> _void))
(define-gtk gtk_notebook_remove_page (_fun _GtkWidget _int -> _void))
(define-gtk gtk_notebook_set_scrollable (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_notebook_get_current_page (_fun _GtkWidget -> _int))
(define-gtk gtk_notebook_set_current_page (_fun _GtkWidget _int -> _void))
(define-gtk gtk_notebook_get_tab_label  (_fun _GtkWidget _GtkWidget -> _GtkWidget))
(define-gtk gtk_notebook_set_tab_reorderable (_fun _GtkWidget _GtkWidget _gboolean -> _void))

(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))

(define text-close-label? #t)

;; Used for test close label:
(define-gtk gtk_label_new (_fun _string -> _GtkWidget))
(define-gtk gtk_label_set_use_markup (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_label_set_use_underline (_fun _GtkWidget _gboolean -> _void))

;; Used for icon close label:
(define-gtk gtk_button_new (_fun -> _GtkWidget))
(define-gtk gtk_button_set_relief (_fun _GtkWidget _int -> _void))
(define-gtk gtk_button_set_image (_fun _GtkWidget  _GtkWidget -> _void))
(define-gtk gtk_image_new_from_stock (_fun _string _int -> _GtkWidget))
(define GTK_STOCK_CLOSE "gtk-close")
(define GTK_ICON_SIZE_MENU 1)
(define GTK_RELIEF_NONE 2)

(define-gtk gtk_widget_get_parent (_fun _GtkWidget -> _GtkWidget))
(define-gtk gtk_widget_set_focus_on_click (_fun _GtkWidget _gboolean -> _void)
  #:fail (lambda () (lambda (w focus?) (void))))

(define-gtk gtk_widget_ref (_fun _GtkWidget -> _void)
  #:fail (lambda () g_object_ref))
(define-gtk gtk_widget_unref (_fun _GtkWidget -> _void)
  #:fail (lambda () g_object_unref))

(define-struct page (bin-gtk label-gtk close-gtk))

(define-signal-handler connect-changed "switch-page"
  (_fun _GtkWidget _pointer _int -> _void)
  (lambda (gtk ignored i)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx page-changed i)))))

(define-signal-handler connect-reordered "page-reordered"
  (_fun _GtkWidget _pointer _int -> _void)
  (lambda (gtk child i)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx page-reordered child i)))))

(define-signal-handler connect-clicked "clicked"
  (_fun _GtkWidget _GtkWidget -> _void)
  (lambda (button-gtk gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx queue-close-clicked (gtk_widget_get_parent button-gtk))))))

(define tab-panel%
  (class (client-size-mixin (panel-container-mixin (panel-mixin window%)))
    (init parent
          x y w h
          style
          labels)
    
    (inherit set-size set-auto-size infer-client-delta get-gtk
             reset-child-freezes reset-child-dcs get-height)

    (define notebook-gtk (if gtk3?
			     (gtk_notebook_new)
			     (as-gtk-allocation (gtk_notebook_new))))
    (define gtk (if gtk3?
		    ;; For some reason, tabs in a hidden eventbox
		    ;; don't work right. Add a layer.
		    (let ([gtk (as-gtk-allocation (gtk_event_box_new))])
		      (gtk_container_add gtk notebook-gtk)
		      (gtk_widget_show notebook-gtk)
		      gtk)
		    notebook-gtk))
    ;; Reparented so that it's always in the current page's bin:
    (define client-gtk (gtk_fixed_new))

    (gtk_notebook_set_scrollable notebook-gtk #t)
    (define can-reorder? (and (memq 'can-reorder style) #t))
    (define can-close? (and (memq 'can-close style) #t))

    (super-new [parent parent]
               [gtk gtk]
               [client-gtk client-gtk]
               [extra-gtks (append
			    (if (eq? gtk notebook-gtk)
				null
				(list notebook-gtk))
			    (list client-gtk))]
               [no-show? (memq 'deleted style)])

    ; Once without tabs to set client-width delta:
    (infer-client-delta #t #f)
    
    (define empty-bin-gtk (gtk_hbox_new #f 0))
    (define current-bin-gtk #f)

    (define/private (select-bin bin-gtk)
      (set! current-bin-gtk bin-gtk)
      ;; re-parenting can change the underlying window, so
      ;; make sure no freeze in places:
      (reset-child-freezes)
      (gtk_box_pack_start bin-gtk client-gtk #t #t 0)
      ;; re-parenting can change the underlying window dc:
      (reset-child-dcs))

    (define/private (maybe-add-close label-gtk)
      (cond
       [can-close?
	(let ([hbox-gtk (gtk_hbox_new #f 0)]
	      [close-gtk (gtk_button_new)])
	  
	  (cond
	   [text-close-label?
	    ;; abuse of multiply symbol?
	    (define close-label-gtk (gtk_label_new "\xD7"))
	    (gtk_label_set_use_markup close-label-gtk #f)
	    (gtk_label_set_use_underline close-label-gtk #f)
	    (gtk_container_add close-gtk close-label-gtk)
	    (gtk_widget_show close-label-gtk)]
	   [else
	    ;; looks heavy for most purposes:
	    (define close-icon (gtk_image_new_from_stock GTK_STOCK_CLOSE
							 GTK_ICON_SIZE_MENU))
	    (gtk_button_set_image close-gtk close-icon)])
	  
	  (gtk_widget_set_focus_on_click close-gtk #f)
	  (gtk_button_set_relief close-gtk GTK_RELIEF_NONE)
	  (gtk_container_add hbox-gtk label-gtk)
	  (gtk_container_add hbox-gtk close-gtk)
	  (gtk_widget_show close-gtk)
	  (gtk_widget_show label-gtk)
	  (connect-clicked close-gtk gtk)
	  hbox-gtk)]
       [else label-gtk]))
	

    (define pages
      (for/list ([lbl labels])
        (let* ([bin-gtk (gtk_hbox_new #f 0)]
	       [label-gtk (gtk_label_new_with_mnemonic lbl)]
	       [close-gtk (maybe-add-close label-gtk)])
          (gtk_notebook_append_page notebook-gtk bin-gtk close-gtk)
	  (when can-reorder?
		(gtk_notebook_set_tab_reorderable notebook-gtk bin-gtk #t))
          (gtk_widget_show bin-gtk)
          (make-page bin-gtk label-gtk close-gtk))))

    (define/private (install-empty-page)
      (gtk_notebook_append_page notebook-gtk empty-bin-gtk #f)
      (gtk_widget_show empty-bin-gtk))

    (if (null? pages)
        (begin
          (select-bin empty-bin-gtk)
          (install-empty-page))
        (begin
          (select-bin (page-bin-gtk (car pages)))))
    (gtk_widget_show client-gtk)
    
    (connect-key-and-mouse notebook-gtk)
    (connect-focus notebook-gtk)

    ; With tabs to set client-width delta:
    (infer-client-delta #f #t)

    (set-auto-size)

    (define callback void)
    (define/public (set-callback cb) (set! callback cb))
    (define/private (do-callback)
      (callback this (new control-event%
                          [event-type 'tab-panel]
                          [time-stamp (current-milliseconds)])))

    (define/public (swap-in bin-gtk)
      (gtk_widget_ref client-gtk)
      (gtk_container_remove current-bin-gtk client-gtk)
      (select-bin bin-gtk)
      (gtk_widget_unref client-gtk))

    (define callback-ok? #t)

    (define/public (page-changed i)
      ; range check works around spurious callbacks:
      (when (< -1 i (length pages))
        (swap-in (page-bin-gtk (list-ref pages i)))
        (when callback-ok?
          (queue-window-event this (lambda () (do-callback))))))
    (connect-changed notebook-gtk)

    (define/public (page-reordered child new-pos)
      (unless (equal? child (list-ref pages new-pos))
        (define old-pages (for/hash ([page (in-list pages)]
                                     [i (in-naturals)])
                            (values (page-bin-gtk page) (cons i page))))
	(define move-page (cdr (hash-ref old-pages child)))
        (define new-pages (let loop ([l pages] [i 0])
                            (cond
                              [(= i new-pos) (cons move-page (remove move-page l))]
                              [(equal? (car l) move-page)
                               (loop (cdr l) i)]
                              [else
                               (cons (car l) (loop (cdr l) (add1 i)))])))
        (set! pages new-pages)
        (on-choice-reorder (for/list ([page (in-list new-pages)])
                             (car (hash-ref old-pages (page-bin-gtk page)))))))
    (define/public (on-choice-reorder moved-mapping) (void))
    (when can-reorder?
      (connect-reordered notebook-gtk))

    (define/public (queue-close-clicked close-gtk)
      (for ([page (in-list pages)]
	    [i (in-naturals)])
	   (when (equal? close-gtk (page-close-gtk page))
		 (queue-window-event this (lambda () (on-choice-close i))))))
    (define/public (on-choice-close pos) (void))
    
    (define/override (get-client-gtk) client-gtk)

    (public [append* append])
    (define (append* lbl)
      (atomically
       (set! callback-ok? #f)
       (do-append lbl)
       (set! callback-ok? #t)))

    (define/private (do-append lbl)
      (let ([page
             (let* ([bin-gtk (gtk_hbox_new #f 0)]
		    [label-gtk (gtk_label_new_with_mnemonic lbl)]
		    [close-gtk (maybe-add-close label-gtk)])
               (gtk_notebook_append_page notebook-gtk bin-gtk close-gtk)
               (when can-reorder?
                 (gtk_notebook_set_tab_reorderable notebook-gtk bin-gtk #t))
               (gtk_widget_show bin-gtk)
               (make-page bin-gtk label-gtk close-gtk))])
        (set! pages (append pages (list page)))
        (when (null? (cdr pages))
          (swap-in (page-bin-gtk (car pages)))
          (g_object_ref empty-bin-gtk)
          (gtk_notebook_remove_page notebook-gtk 0))))

    (define/private (do-delete i)
      (let ([page (list-ref pages i)])
        (when (ptr-equal? current-bin-gtk (page-bin-gtk page))
          (let ([cnt (length pages)])
            (if (= i (sub1 cnt))
                (if (null? (cdr pages))
                    (begin
                      (install-empty-page)
                      (set! pages null)
                      (gtk_notebook_set_current_page notebook-gtk 1)
                      (swap-in empty-bin-gtk))
                    (gtk_notebook_set_current_page notebook-gtk (sub1 i)))
                (gtk_notebook_set_current_page notebook-gtk (add1 i)))))
        (gtk_notebook_remove_page notebook-gtk i)
        (set! pages (remq page pages))))

    (define/public (delete i)
      (atomically
       (set! callback-ok? #f)
       (do-delete i)
       (set! callback-ok? #t)))

    (define/public (set choices)
      (atomically
       (set! callback-ok? #f)
       (for ([page (in-list pages)])
         (do-delete 0))
       (for ([lbl (in-list choices)])
         (append* lbl))
       (set! callback-ok? #t)))

    (define/public (set-label i str)
      (gtk_label_set_text_with_mnemonic (page-label-gtk (list-ref pages i)) 
                                        (mnemonic-string str)))

    (define/public (number) (length pages))
    (define/public (button-focus n)
      (if (= n -1)
          (get-selection)
          (direct-set-selection n)))

    (define/override (gets-focus?) #t)
    (define/override (set-focus)
      (gtk_widget_grab_focus notebook-gtk))

    (define/private (direct-set-selection i)
      (gtk_notebook_set_current_page notebook-gtk i))
    (define/public (set-selection i)
      (atomically
       (set! callback-ok? #f)
       (direct-set-selection i)
       (set! callback-ok? #t)))
    (define/public (get-selection)
      (gtk_notebook_get_current_page notebook-gtk))))
