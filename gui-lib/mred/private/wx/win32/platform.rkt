#lang racket/base
(require "init.rkt"
	 "button.rkt"
         "canvas.rkt"
         "check-box.rkt"
         "choice.rkt"
         "clipboard.rkt"
         "cursor.rkt"
         "dialog.rkt"
         "frame.rkt"
         "gauge.rkt"
         "gl-context.rkt"
         "group-panel.rkt"
         "item.rkt"
         "list-box.rkt"
         "menu.rkt"
         "menu-bar.rkt"
         "menu-item.rkt"
         "message.rkt"
         "panel.rkt"
         "printer-dc.rkt"
         "radio-box.rkt"
         "slider.rkt"
         "tab-panel.rkt"
         "window.rkt"
         "key.rkt"
         "procs.rkt"
         (only-in "../common/default-procs.rkt" luminance))
(provide (protect-out platform-values))

(define (platform-values)
  (values
   button%
   canvas%
   canvas-panel%
   check-box%
   choice%
   clipboard-driver%
   cursor-driver%
   dialog%
   frame%
   gauge%
   group-panel%
   item%
   list-box%
   menu%
   menu-bar%
   menu-item%
   message%
   panel%
   printer-dc%
   radio-box%
   slider%
   tab-panel%
   window%
   can-show-print-setup?
   show-print-setup
   id-to-menu-item
   file-selector
   is-color-display?
   get-display-depth
   has-x-selection?
   hide-cursor
   bell
   display-size
   display-origin
   display-count
   display-bitmap-resolution
   flush-display
   get-current-mouse-state
   fill-private-color
   cancel-quit
   get-control-font-face
   get-control-font-size
   get-control-font-size-in-pixels?
   get-double-click-time
   file-creator-and-type
   location->window
   shortcut-visible-in-label?
   unregister-collecting-blit
   register-collecting-blit
   find-graphical-system-path
   play-sound
   get-panel-background
   font-from-user-platform-mode
   get-font-from-user
   color-from-user-platform-mode
   get-color-from-user
   special-option-key
   special-control-key
   any-control+alt-is-altgr
   get-highlight-background-color
   get-highlight-text-color
   get-label-foreground-color
   get-label-background-color
   make-screen-bitmap
   make-gl-bitmap
   check-for-break
   key-symbol-to-menu-key
   needs-grow-box-spacer?
   graphical-system-type
   white-on-black-panel-scheme?))

(define (white-on-black-panel-scheme?)
  ;; if the background and foreground are the same
  ;; color, probably something has gone wrong;
  ;; in that case we want to return #f.
  (< (luminance (get-label-background-color))
     (luminance (get-label-foreground-color))))
