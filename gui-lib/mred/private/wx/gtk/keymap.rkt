#lang racket/base
(require ffi/unsafe
         "utils.rkt"
         "const.rkt"
         "types.rkt")

(provide 
 (protect-out get-alts))

(define _GdkKeymap (_cpointer 'GdkKeymap))

(define-gdk gdk_keymap_get_default (_fun -> _GdkKeymap))

(define-gdk gdk_keymap_translate_keyboard_state
  (_fun _GdkKeymap
        _uint ; hardware_keycode
        _int ; GdkModifierType state
        _int ; group
        (keyval : (_ptr o _uint atomic-interior))
        (effective_group : (_ptr o _int atomic-interior))
        (level : (_ptr o _int atomic-interior))
        (consumed_modifiers : (_ptr o _int atomic-interior))
        -> (r : _gboolean)
        -> (and r keyval)))
                
(define (get-alts event)
  (define (get-one-alt mask)
    (gdk_keymap_translate_keyboard_state (gdk_keymap_get_default)
                                         (GdkEventKey-hardware_keycode event)
                                         (let ([mods (GdkEventKey-state event)])
                                           (bitwise-ior (- mods (bitwise-and mods mask))
                                                        (bitwise-and mask (bitwise-not (bitwise-and mods mask)))))
                                         (GdkEventKey-group event)))
  (let ([alt-gr? (eq? (= (bitwise-and (GdkEventKey-state event) GDK_CONTROL_MASK)
                         GDK_CONTROL_MASK)
                      (= (bitwise-and (GdkEventKey-state event) GDK_MOD1_MASK)
                         GDK_MOD1_MASK))])
    (values (get-one-alt GDK_SHIFT_MASK)
            (and alt-gr?
                 (get-one-alt (bitwise-ior GDK_MOD1_MASK GDK_CONTROL_MASK)))
            (and alt-gr?
                 (get-one-alt (bitwise-ior GDK_SHIFT_MASK GDK_MOD1_MASK GDK_CONTROL_MASK)))
            (get-one-alt GDK_LOCK_MASK))))
