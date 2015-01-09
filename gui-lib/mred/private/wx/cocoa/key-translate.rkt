#lang racket/base
(provide key-translate
         make-initial-dead-key-state
         copy-dead-key-state
	 char->main-char-key+modifiers)

(require (for-syntax syntax/parse racket/syntax racket/base))
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/define
         "types.rkt"
         "utils.rkt")

;;; Bit operations
(define (<< x y) (arithmetic-shift x y))
(define (>> x y) (arithmetic-shift x (- y)))

;;; Libraries used
(define carboncore-lib
  (ffi-lib (string-append "/System/Library/Frameworks/CoreServices.framework/Versions/Current/"
                          "Frameworks/CarbonCore.framework/Versions/Current/CarbonCore")))
(define hitoolbox-lib
  (ffi-lib (string-append "/System/Library/Frameworks/Carbon.framework/Versions/Current/"
                          "Frameworks/HIToolbox.framework/Versions/Current/HIToolbox")))

(define-ffi-definer define-carboncore  carboncore-lib)
(define-ffi-definer define-hitoolbox   hitoolbox-lib)

;;; CORE FOUNDATION

(define _CFStringRef _NSString)

; (define _OSStatus _sint32)       ; already imported
(define-cpointer-type _CFDataRef)


;;; Unicode Characters

;;; Types from MacTypes.h
(define _UniChar              _uint16)
(define _UniCharCount         _ulong)
(define _UniCharPointer      (_ptr io _UniChar))
(define _UniCharCountPointer (_ptr io _UniCharCount))
(define _OptionBits           _uint32)

;;; TEXT INPUT SOURCES

; Most text input sources are keyboards.
(define _TISInputSourceRef (_cpointer 'TISInputSourceRef))

; Each physical key on a keyboard sends a keycode.
; Example: the key label labelled A on a US keyboard sends kVK_ANSI_A=0.

; A keyboard layout determines which character corresponds to a physical key.

; To get a layout, one must first get a reference to the input source:
(define-hitoolbox TISCopyCurrentKeyboardLayoutInputSource             (_fun -> _TISInputSourceRef))
(define-hitoolbox TISCopyCurrentASCIICapableKeyboardLayoutInputSource (_fun -> _TISInputSourceRef))
; Note: These days TISCopyCurrentKeyboardLayoutInputSource ought to work for all keyboards.

; The input source has several properties, one of is:
(define-hitoolbox kTISPropertyUnicodeKeyLayoutData _NSString)

; Getting the property is done by:
(define-hitoolbox TISGetInputSourceProperty
  (_fun (_inputSource : _TISInputSourceRef)
        (_propertyKey : _CFStringRef)           
        -> (_or-null _CFDataRef)))

; The value returned by TISGetInputSourceProperty is a CFDataRef,
; so one mus call CFDataGetBytePtr to get the actual layout.
(define-cf CFDataGetBytePtr (_fun _CFDataRef -> _pointer))

; The return value can be cast to:
(define _UCKeyboardLayout (_cpointer 'UCKeyboardLayout))

; Before translating key codes to characters, one must option
; the physical type of keyboard.
(define-hitoolbox LMGetKbdType (_fun -> _uint8))

; Given a layout and a keyboard type, one can translate
; keycodes to characters using UCKeyTranslate.

(define-carboncore UCKeyTranslate
  (_fun (keyboardLayoutPtr   : _UCKeyboardLayout)
        (virtualKeyCode      : _uint16)
        (keyAction           : _uint16)
        (modifierKeyState    : _uint32)
        (keyboardType        : _uint32)
        (keyTranslateOptions : _OptionBits)            ; uint32
        (deadKeyState        : (_box _uint32))
        (maxStringLength     : _UniCharCount)
        (actualStringLength  : (_box _UniCharCount))
        (unicodeString       : _pointer)
        -> _OSStatus))

; Meaning of parameters:
;   keyAction            what happened to the key?     - usually kUCKeyActionDown
;   modifierKeyState     which modifier keys are down? - shift, alt, ctrl, cmd or combinations
;   keyTranslateOptions  is previous input handled?    - used to disable/enable dead keys
;   deadKeyState         integer encoding of prev keys - none is encoded as 0
;   unicodeString        array into which the result is stored
;   actualStringLength   how many characters were stored in unicodeString

; See key-translate below for a more convenient interface for UCKeyTranslate.

;;;
;;; Constants and there symbolic representation.
;;;

; In order to define a lot of constants and keep their symbols
; around it is convenient a little help.

; SYNTAX (define-name/value-definer category)
;   Defines two hash-tables
;     category-name-to-value-ht    from symbolic name to value
;     category-value-to-name-ht    from value to symbolic name
;   It also defines
;     SYNTAX (define-category name val)
;       which defines name as val, provides val 
;       and store the pairing in the hashtables.
;   Example: See key actions below.
(define-syntax (define-name/value-definer stx)
  (syntax-parse stx
    [(_ prefix)
     (with-syntax ([prefix-name-to-value-ht (format-id stx "~a-name-to-value-ht" #'prefix)]
                   [prefix-value-to-name-ht (format-id stx "~a-value-to-name-ht" #'prefix)]
                   [prefix-name             (format-id stx "~a-name"             #'prefix)]
                   [prefix-value            (format-id stx "~a-value"            #'prefix)]
                   [define-prefix           (format-id stx "define-~a"           #'prefix)])
       #'(begin
           (define prefix-name-to-value-ht (make-hash))
           (define prefix-value-to-name-ht (make-hash))
           (define (prefix-name value) (hash-ref prefix-value-to-name-ht value #f))
           (define (prefix-value name) (hash-ref prefix-name-to-value-ht name #f))
           (provide prefix-name-to-value-ht 
                    prefix-value-to-name-ht
                    prefix-name
                    prefix-value)
           (define-syntax (define-prefix stx)
             (syntax-parse stx
               [(_ name expr)
                #'(begin
                    (provide name )
                    (define name expr)
                    (hash-set! prefix-name-to-value-ht 'name name)
                    (hash-set! prefix-value-to-name-ht name 'name))]))))]))

;;;
;;; Key Actions
;;;

(define-name/value-definer key-action)
(define-key-action kUCKeyActionDown    0) ; /* key is going down*/
(define-key-action kUCKeyActionUp      1) ; /* key is going up*/
(define-key-action kUCKeyActionAutoKey 2) ; /* auto-key down*/
(define-key-action kUCKeyActionDisplay 3) ; /* get information for key display (as in Key Caps) */

;;;
;;; Key Translate Options
;;;
; There is only one option. Should dead keys have an effect or not?
(define kUCKeyTranslateNoDeadKeysBit  0) ; /* Prevents setting any new dead-key states*/
(define kUCKeyTranslateNoDeadKeysFlag 1)
(define kUCKeyTranslateNoDeadKeysMask 1)


;;;
;;; EventModifiers (UInt16)
;;;

(define-name/value-definer event-modifier-bits)
(define-name/value-definer event-modifier-flag)

; The constants are "event modifiers". Some of them
; are traditional key modiers, such as a modifier-shift-key-bit.

; From (file dates 2008):
; /System/Library/Frameworks/Carbon.framework/Versions/A/
; Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h 

; The definitions indicate which bit controls what.
(define-event-modifier-bits modifier-active-flag-bit 0) ; activeFlagBit = 0,  /* activate window?
;                                                                  (activateEvt and mouseDown)
(define-event-modifier-bits modifier-btn-state-bit   7) ; btnStateBit  = 7,  state of mouse! button?
(define-event-modifier-bits modifier-cmd-key-bit     8) ;                     /* command key down?*/
(define-event-modifier-bits modifier-shift-key-bit   9) ; shiftKeyBit   = 9,  /* shift key down?*/
(define-event-modifier-bits modifier-alpha-lock-bit  10) ; alphaLockBit  = 10, /* alpha lock down?*/
(define-event-modifier-bits modifier-option-bit      11) ; optionKeyBit  = 11, /* option key down?*/
(define-event-modifier-bits modifier-control-key-bit 12) ; controlKeyBit = 12, /* control key down?*/
; NOTE: The following 3 modifiers are not supported on OS X
(define-event-modifier-bits modifier-right-shift-key-bit   13) ; /* right shift key down? */
(define-event-modifier-bits modifier-right-option-key-bit  14) ; /* right Option key down? */
(define-event-modifier-bits modifier-right-control-key-bit 15) ; /* right Control key down? */

; In actual use, we use the flags:
(define-event-modifier-flag modifier-active-flag       (<< 1  0))
(define-event-modifier-flag modifier-btn-state         (<< 1  7))
(define-event-modifier-flag modifier-cmd-key           (<< 1  8))
(define-event-modifier-flag modifier-shift-key         (<< 1  9))
(define-event-modifier-flag modifier-alpha-lock        (<< 1 10)) 
(define-event-modifier-flag modifier-option-key        (<< 1 11)) 
(define-event-modifier-flag modifier-control-key       (<< 1 12)) 
; NOTE: The following 3 modifiers are not supported on OS X
(define-event-modifier-flag modifier-right-shift-key   (<< 1 13))
(define-event-modifier-flag modifier-right-option-key  (<< 1 14))
(define-event-modifier-flag modifier-right-control-key (<< 1 15))

;;;
;;; Virtual Keycodes
;;; 

;/*
; *  Summary:
; *    Virtual keycodes
; *  
; *  Discussion:
; *    These constants are the virtual keycodes defined originally in
; *    Inside Mac Volume V, pg. V-191. They identify physical keys on a
; *    keyboard. Those constants with "ANSI" in the name are labeled
; *    according to the key position on an ANSI-standard US keyboard.
; *    For example, kVK_ANSI_A indicates the virtual keycode for the key
; *    with the letter 'A' in the US keyboard layout. Other keyboard
; *    layouts may have the 'A' key label on a different physical key;
; *    in this case, pressing 'A' will generate a different virtual
; *    keycode.
; */
(define-name/value-definer virtual-key-code)

(define-virtual-key-code kVK_ANSI_A                   #x00)
(define-virtual-key-code kVK_ANSI_S                   #x01)
(define-virtual-key-code kVK_ANSI_D                   #x02)
(define-virtual-key-code kVK_ANSI_F                   #x03)
(define-virtual-key-code kVK_ANSI_H                   #x04)
(define-virtual-key-code kVK_ANSI_G                   #x05)
(define-virtual-key-code kVK_ANSI_Z                   #x06)
(define-virtual-key-code kVK_ANSI_X                   #x07)
(define-virtual-key-code kVK_ANSI_C                   #x08)
(define-virtual-key-code kVK_ANSI_V                   #x09)
(define-virtual-key-code kVK_ANSI_B                   #x0B)
(define-virtual-key-code kVK_ANSI_Q                   #x0C)
(define-virtual-key-code kVK_ANSI_W                   #x0D)
(define-virtual-key-code kVK_ANSI_E                   #x0E)
(define-virtual-key-code kVK_ANSI_R                   #x0F)
(define-virtual-key-code kVK_ANSI_Y                   #x10)
(define-virtual-key-code kVK_ANSI_T                   #x11)
(define-virtual-key-code kVK_ANSI_1                   #x12)
(define-virtual-key-code kVK_ANSI_2                   #x13)
(define-virtual-key-code kVK_ANSI_3                   #x14)
(define-virtual-key-code kVK_ANSI_4                   #x15)
(define-virtual-key-code kVK_ANSI_6                   #x16)
(define-virtual-key-code kVK_ANSI_5                   #x17)
(define-virtual-key-code kVK_ANSI_Equal               #x18)
(define-virtual-key-code kVK_ANSI_9                   #x19)
(define-virtual-key-code kVK_ANSI_7                   #x1A)
(define-virtual-key-code kVK_ANSI_Minus               #x1B)
(define-virtual-key-code kVK_ANSI_8                   #x1C)
(define-virtual-key-code kVK_ANSI_0                   #x1D)
(define-virtual-key-code kVK_ANSI_RightBracket        #x1E)
(define-virtual-key-code kVK_ANSI_O                   #x1F)
(define-virtual-key-code kVK_ANSI_U                   #x20)
(define-virtual-key-code kVK_ANSI_LeftBracket         #x21)
(define-virtual-key-code kVK_ANSI_I                   #x22)
(define-virtual-key-code kVK_ANSI_P                   #x23)
(define-virtual-key-code kVK_ANSI_L                   #x25)
(define-virtual-key-code kVK_ANSI_J                   #x26)
(define-virtual-key-code kVK_ANSI_Quote               #x27)
(define-virtual-key-code kVK_ANSI_K                   #x28)
(define-virtual-key-code kVK_ANSI_Semicolon           #x29)
(define-virtual-key-code kVK_ANSI_Backslash           #x2A)
(define-virtual-key-code kVK_ANSI_Comma               #x2B)
(define-virtual-key-code kVK_ANSI_Slash               #x2C)
(define-virtual-key-code kVK_ANSI_N                   #x2D)
(define-virtual-key-code kVK_ANSI_M                   #x2E)
(define-virtual-key-code kVK_ANSI_Period              #x2F)
(define-virtual-key-code kVK_ANSI_Grave               #x32)
(define-virtual-key-code kVK_ANSI_KeypadDecimal       #x41)
(define-virtual-key-code kVK_ANSI_KeypadMultiply      #x43)
(define-virtual-key-code kVK_ANSI_KeypadPlus          #x45)
(define-virtual-key-code kVK_ANSI_KeypadClear         #x47)
(define-virtual-key-code kVK_ANSI_KeypadDivide        #x4B)
(define-virtual-key-code kVK_ANSI_KeypadEnter         #x4C)
(define-virtual-key-code kVK_ANSI_KeypadMinus         #x4E)
(define-virtual-key-code kVK_ANSI_KeypadEquals        #x51)
(define-virtual-key-code kVK_ANSI_Keypad0             #x52)
(define-virtual-key-code kVK_ANSI_Keypad1             #x53)
(define-virtual-key-code kVK_ANSI_Keypad2             #x54)
(define-virtual-key-code kVK_ANSI_Keypad3             #x55)
(define-virtual-key-code kVK_ANSI_Keypad4             #x56)
(define-virtual-key-code kVK_ANSI_Keypad5             #x57)
(define-virtual-key-code kVK_ANSI_Keypad6             #x58)
(define-virtual-key-code kVK_ANSI_Keypad7             #x59)
(define-virtual-key-code kVK_ANSI_Keypad8             #x5B)
(define-virtual-key-code kVK_ANSI_Keypad9             #x5C)

; /* keycodes for keys that are independent of keyboard layout*/
(define-virtual-key-code kVK_Return                   #x24)
(define-virtual-key-code kVK_Tab                      #x30)
(define-virtual-key-code kVK_Space                    #x31)
(define-virtual-key-code kVK_Delete                   #x33)
(define-virtual-key-code kVK_Escape                   #x35)
(define-virtual-key-code kVK_Command                  #x37)
(define-virtual-key-code kVK_Shift                    #x38)
(define-virtual-key-code kVK_CapsLock                 #x39)
(define-virtual-key-code kVK_Option                   #x3A)
(define-virtual-key-code kVK_Control                  #x3B)
(define-virtual-key-code kVK_RightShift               #x3C)
(define-virtual-key-code kVK_RightOption              #x3D)
(define-virtual-key-code kVK_RightControl             #x3E)
(define-virtual-key-code kVK_Function                 #x3F)
(define-virtual-key-code kVK_F17                      #x40)
(define-virtual-key-code kVK_VolumeUp                 #x48)
(define-virtual-key-code kVK_VolumeDown               #x49)
(define-virtual-key-code kVK_Mute                     #x4A)
(define-virtual-key-code kVK_F18                      #x4F)
(define-virtual-key-code kVK_F19                      #x50)
(define-virtual-key-code kVK_F20                      #x5A)
(define-virtual-key-code kVK_F5                       #x60)
(define-virtual-key-code kVK_F6                       #x61)
(define-virtual-key-code kVK_F7                       #x62)
(define-virtual-key-code kVK_F3                       #x63)
(define-virtual-key-code kVK_F8                       #x64)
(define-virtual-key-code kVK_F9                       #x65)
(define-virtual-key-code kVK_F11                      #x67)
(define-virtual-key-code kVK_F13                      #x69)
(define-virtual-key-code kVK_F16                      #x6A)
(define-virtual-key-code kVK_F14                      #x6B)
(define-virtual-key-code kVK_F10                      #x6D)
(define-virtual-key-code kVK_F12                      #x6F)
(define-virtual-key-code kVK_F15                      #x71)
(define-virtual-key-code kVK_Help                     #x72)
(define-virtual-key-code kVK_Home                     #x73)
(define-virtual-key-code kVK_PageUp                   #x74)
(define-virtual-key-code kVK_ForwardDelete            #x75)
(define-virtual-key-code kVK_F4                       #x76)
(define-virtual-key-code kVK_End                      #x77)
(define-virtual-key-code kVK_F2                       #x78)
(define-virtual-key-code kVK_PageDown                 #x79)
(define-virtual-key-code kVK_F1                       #x7A)
(define-virtual-key-code kVK_LeftArrow                #x7B)
(define-virtual-key-code kVK_RightArrow               #x7C)
(define-virtual-key-code kVK_DownArrow                #x7D)
(define-virtual-key-code kVK_UpArrow                  #x7E)

; /* ISO keyboards only*/
(define-virtual-key-code kVK_ISO_Section              #x0A)

; /* JIS keyboards only*/
(define-virtual-key-code kVK_JIS_Yen                  #x5D)
(define-virtual-key-code kVK_JIS_Underscore           #x5E)
(define-virtual-key-code kVK_JIS_KeypadComma          #x5F)
(define-virtual-key-code kVK_JIS_Eisu                 #x66)
(define-virtual-key-code kVK_JIS_Kana                 #x68)

;;;
;;; MacRoman character codes
;;;

; The following may or may not be useful at another time.
(define-name/value-definer mac-roman)

(define-mac-roman kNullCharCode                0)
(define-mac-roman kHomeCharCode                1)
(define-mac-roman kEnterCharCode               3)
(define-mac-roman kEndCharCode                 4)
(define-mac-roman kHelpCharCode                5)
(define-mac-roman kBellCharCode                7)
(define-mac-roman kBackspaceCharCode           8)
(define-mac-roman kTabCharCode                 9)
(define-mac-roman kLineFeedCharCode            10)
(define-mac-roman kVerticalTabCharCode         11)
(define-mac-roman kPageUpCharCode              11)
(define-mac-roman kFormFeedCharCode            12)
(define-mac-roman kPageDownCharCode            12)
(define-mac-roman kReturnCharCode              13)
(define-mac-roman kFunctionKeyCharCode         16)
(define-mac-roman kCommandCharCode             17)  ; /* glyph available only in system fonts*/
(define-mac-roman kCheckCharCode               18)  ; /* glyph available only in system fonts*/
(define-mac-roman kDiamondCharCode             19)  ; /* glyph available only in system fonts*/
(define-mac-roman kAppleLogoCharCode           20)  ; /* glyph available only in system fonts*/
(define-mac-roman kEscapeCharCode              27)
(define-mac-roman kClearCharCode               27)
(define-mac-roman kLeftArrowCharCode           28)
(define-mac-roman kRightArrowCharCode          29)
(define-mac-roman kUpArrowCharCode             30)
(define-mac-roman kDownArrowCharCode           31)
(define-mac-roman kSpaceCharCode               32)
(define-mac-roman kDeleteCharCode              127)
(define-mac-roman kBulletCharCode              165)
(define-mac-roman kNonBreakingSpaceCharCode    202)

;;;
;;; Useful Unicode key points
;;;

(define-name/value-definer unicode-key)

(define-unicode-key kShiftUnicode      #x21E7) ;/* Unicode UPWARDS WHITE ARROW*/
(define-unicode-key kControlUnicode    #x2303) ;/* Unicode UP ARROWHEAD*/
(define-unicode-key kOptionUnicode     #x2325) ;/* Unicode OPTION KEY*/
(define-unicode-key kCommandUnicode    #x2318) ;/* Unicode PLACE OF INTEREST SIGN*/
(define-unicode-key kPencilUnicode     #x270E) ;/* Unicode LOWER RIGHT PENCIL; 
;                                                         actually pointed left until Mac OS X 10.3*/
(define-unicode-key kPencilLeftUnicode #xF802) ;/* Unicode LOWER LEFT PENCIL; 
;                                                         available in Mac OS X 10.3 and later*/
(define-unicode-key kCheckUnicode      #x2713) ;/* Unicode CHECK MARK*/
(define-unicode-key kDiamondUnicode    #x25C6) ;/* Unicode BLACK DIAMOND*/
(define-unicode-key kBulletUnicode     #x2022) ;/* Unicode BULLET*/
(define-unicode-key kAppleLogoUnicode  #xF8FF) ;/* Unicode APPLE LOGO*/



;;;
;;; Racket interface to UCKeyTranslate
;;;

;; The physical keyboard typed is cached.
(define cached-keyboard-layout #f)

(define (get-current-keyboard-layout)
  (define keyboard     (TISCopyCurrentKeyboardLayoutInputSource))
  (define layout-data  (TISGetInputSourceProperty keyboard kTISPropertyUnicodeKeyLayoutData))
  (define layout       (CFDataGetBytePtr layout-data))
  (cpointer-push-tag!  layout 'UCKeyboardLayout) ; cast
  layout)

;; The strings used to store output from UCKeyTranslate is only allocated once:
(define max-string-length 255) 
(define output-chars (malloc _UniChar max-string-length))

;; Dead key state
;    A pointer to an unsigned 32-bit value, initialized to zero. 
;    The UCKeyTranslate function uses this value to store private 
;    information about the current dead key state.
(define (make-initial-dead-key-state)
  (box 0))

(define (copy-dead-key-state dks)
  (box (unbox dks)))



; key-translate : integer [<extra options>] -> string
;    Translates a virtual keycode into a string.
;    The default key action is kUCKeyActionDown.
;    The default dead key state is none.
;    The default translate options are to ignore dead keys, 
;    unless a dead key state was provided, if so 
;    dead keys are activated.
;    The keyboard layout is the one returned by get-current-keyboard-layout;
;    the default is to use a cached value. Override by
;    passing #f which means to refresh the cache, or
;    pass a layout to use.
(define (key-translate virtual-key-code 
                       #:key-action            [key-action         kUCKeyActionDown]
                       #:modifier-key-state    [modifier-key-state                0]  ; no modifier
                       #:keyboard-type         [keyboard-type        (LMGetKbdType)]
                       #:key-translate-options [key-translate-options            #f]  
                       #:dead-key-state        [dead-key-state                   #f]  ; no prev state
                       #:keyboard-layout       [layout-in                   'cached]) ; use cached
  (define actual-string-length (box 0))
  (set! key-translate-options
        (or key-translate-options                ; use user settings if provided
            (if dead-key-state                   ; otherwise if user has set dead-key-state,
                0                                ; then take dead-keys into account
                kUCKeyTranslateNoDeadKeysFlag))) ; else ignore dead keys
  
  (set! dead-key-state (or dead-key-state (make-initial-dead-key-state)))
  
  (define layout
    (case layout-in
      ; use cached
      [(cached)    (cond
                    [cached-keyboard-layout => values]
                    [else   (set! cached-keyboard-layout (get-current-keyboard-layout))
                            cached-keyboard-layout])]
      ; refresh cache
      [(#f)         (set! cached-keyboard-layout (get-current-keyboard-layout))
                    cached-keyboard-layout]
      ; use provided
      [else          layout-in]))
  
  (UCKeyTranslate layout 
                  virtual-key-code
                  key-action
                  (bitwise-and (>> modifier-key-state 8) #xFF)
                  keyboard-type
                  key-translate-options
                  dead-key-state
                  max-string-length
                  actual-string-length
                  output-chars)
  ; get the number of characters returned, and convert to string
  (define n (max 0 (min max-string-length (unbox actual-string-length))))
  (list->string (for/list ([i (in-range n)]) 
                  (integer->char (ptr-ref output-chars _UniChar i)))))

;;;
;;; Conversions back and forth between characters and key codes.
;;;

; Given a char it is useful to know which key and modifiers must
; be pressed to produce that character. Here a table is 
; made storing all characters that can be produced without
; using dead keys.

(define char-to-osx-key-code-ht (make-hash))
(define osx-key-code-to-char-ht (make-hash))
(define char-without-shift-ht   (make-hash))

;; All possible values for modifier combinations
(define all-modifier-combinations ; 16 in all
  (let ()
    (define no-modifier '(()))
    (define one-modifier
      '((shift) ; "simplest" modifier first
        (cmd) (control) (alt)))
    (define two-modifiers 
      '((cmd     shift)
        (control shift)
        (alt     shift)
        (cmd     control)
        (cmd     option)
        (control option)))
    (define four-modifiers  (list (apply append one-modifier)))
    (define three-modifiers (for/list ([m (reverse one-modifier)]) (remove m (car four-modifiers))))
    (append no-modifier one-modifier two-modifiers three-modifiers four-modifiers)))

;; symbolic modifier to numeric value
(define modifier-ht (make-hash))
(hash-set! modifier-ht 'cmd     modifier-cmd-key)
(hash-set! modifier-ht 'control modifier-control-key)
(hash-set! modifier-ht 'alt     modifier-option-key)
(hash-set! modifier-ht 'shift   modifier-shift-key)

; combine list of modifiers to a single integer
(define (modifier-symbol-list->integer ms)
  (for/sum ([m (in-list ms)])
    (hash-ref modifier-ht m 0)))

;; The "main char key" is the character produced when no modifier is pressed.
;; Example If #\> is produced by <shift>+<period> then the main char key of both > and <period> is #\.
(define char-to-main-char-key-ht         (make-hash)) ; char -> char
(define char-to-osx-keycode+modifiers-ht (make-hash)) ; char -> (list sym int (list sym) int)
(define osx-keycode-to-main-char-key-ht  (make-hash)) ; int  -> char

; hash-set-new! : hash-table key value -> void
;   only sets if key has no previous value
(define (hash-set-new! ht k v) (unless (hash-ref ht k #f) (hash-set! ht k v)))

;; fill in hash tables
(for* ([modsyms all-modifier-combinations]                            ; go through all physical key
       [(kvc n) virtual-key-code-name-to-value-ht])                   ; and modifier combinations
  (define modks (modifier-symbol-list->integer modsyms))
  (define s (key-translate n #:modifier-key-state modks))             ; translate to a char
  (unless (string=? s "")                                             ; unless key is dead
    (define c (string-ref s 0))
    (hash-set-new! char-to-osx-keycode+modifiers-ht c                 ; store where char is from
                   (list kvc n modsyms modks))                        ; (simplest is kept)
    (cond 
      [(null? modsyms)                                               ; also, if it is a main char
       (hash-set! char-to-main-char-key-ht c c)                       ;   store it as such
       (hash-set! osx-keycode-to-main-char-key-ht n c)]               ;   
      [else
       (define mck (hash-ref osx-keycode-to-main-char-key-ht n #f))   ; otherwise find 
       (when mck (hash-set-new! char-to-main-char-key-ht c mck))])))  ;   and store main char key

(define (char->main-char-key+modifiers c)
  (define mck (hash-ref char-to-main-char-key-ht c #f))
  (if mck
      (let ()
	(define k+ms (hash-ref char-to-osx-keycode+modifiers-ht c #f))
	(define mods (if (list? k+ms) (cadddr k+ms) #f))
	(values mck mods))
    (values #f #f)))
