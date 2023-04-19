#lang racket/base

;; Registers the window class:
(require "wndclass.rkt"
         "queue.rkt"
         "utils.rkt"
         "types.rkt"
         ffi/unsafe
         (only-in ffi/unsafe/com define-com-interface
                  _IUnknown _IUnknown_vt
                  string->iid QueryInterface))

(define pump-thread (win32-start-event-pump))

(define RO_INIT_MULTITHREADED 1)
(define-win-core-winrt RoInitialize
  (_wfun
   _int
   -> (r : _HRESULT)
   -> (when (negative? r)
        (error 'RoInitialize "failed: 0x~x"
               (bitwise-and #xFFFF r)))))

(define-cstruct _HSTRING_HEADER
  ([content (_array _byte 24)]))

(define-win-core-winrt WindowsCreateStringReference
  (_wfun
   (s : _string/utf-16)
   (_UINT32 = (string-length s))
   (_ptr o _HSTRING_HEADER)
   (h : (_ptr o _HSTRING))
   -> (r : _HRESULT)
   -> (if (negative? r)
          (error 'WindowsCreateStringReference "failed: 0x~x" (bitwise-and #xFFFF r))
          h)))

;; todo: use _hfun instead of _wfun
(define-win-core-winrt RoActivateInstance
  (_wfun _HSTRING
         (h : (_ptr o _IUnknown))
         -> (r : _HRESULT)
         -> (if (negative? r)
                (error 'RoActivateInstance "failed: 0x~x" (bitwise-and #xFFFF r))
                h)))
(log-error "initializing")
(void (RoInitialize RO_INIT_MULTITHREADED))
(log-error "initialized")

(define RuntimeClass_Windows_UI_ViewManagement_UISettings
  "Windows.UI.ViewManagement.UISettings")

(define str
  (WindowsCreateStringReference
   RuntimeClass_Windows_UI_ViewManagement_UISettings))
(log-error "str ~s" str)
(define uiSettingsAsInspectable (RoActivateInstance str))
(log-error "uiSettingsAsInspectable ~s" uiSettingsAsInspectable)
(define-com-interface
  (_IInspectable _IUnknown)
  ([GetIids (_fun -> _void)]
   [GetRuntimeClassName (_fun -> _void)]
   [GetTrustLevel (_fun -> _void)]))
(define-com-interface
  (_IUISettings3 _IInspectable)
  ([GetColorValue (_fun -> _void)]
   [add_ColorValuesChanged (_fun -> _void)]
   [remove_ColorValuesChanged (_fun -> _void)]))
(define iuisettings3-uuid "{03021be4-5254-4781-8194-5168f7d06d7b}")
(log-error "com object ~s"
(QueryInterface uiSettingsAsInspectable
                (string->iid iuisettings3-uuid)
                _IUISettings3-pointer))
