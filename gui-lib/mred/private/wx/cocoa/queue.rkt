#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/global
         ffi/unsafe/schedule
         ffi/unsafe/custodian
         racket/class
         racket/draw/private/dc
         "pool.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "../common/queue.rkt"
         "../common/handlers.rkt"
         "../../lock.rkt"
         "../common/freeze.rkt")

(provide 
 (protect-out app
              promote-to-gui!
              cocoa-start-event-pump
              cocoa-install-event-wakeup
              set-eventspace-hook!
              set-front-hook!
              set-menu-bar-hooks!
              set-mouse-or-key-hook!
              set-fixup-window-locations!
              post-dummy-event

              try-to-sync-refresh
              try-to-flush
              sync-cocoa-events
              set-screen-changed-callback!)

 ;; from common/queue:
 current-eventspace
 queue-event
 yield)

(import-class NSApplication NSAutoreleasePool NSColor NSProcessInfo NSArray NSMenu NSThread)

(unless (tell #:type _BOOL NSThread isMainThread)
  (error 'racket/gui
         "cannot instantiate in a non-main place on Mac OS"))

;; Extreme hackery to hide original arguments from
;; NSApplication, because NSApplication wants to turn 
;; the arguments into `application:openFile:' calls.
;; To hide the arguments, we replace the implementation
;; of `arguments' in the NSProcessInfo object.
(define (hack-argument-replacement self method)
  (tell NSArray 
        arrayWithObjects: #:type (_vector i _NSString) (vector (path->string (find-system-path 'exec-file)))
        count: #:type _NSUInteger 1))
(let ([m (class_getInstanceMethod NSProcessInfo (selector arguments))])
  (void (method_setImplementation m hack-argument-replacement)))

(define app (tell NSApplication sharedApplication))

(define got-file? #f)

(define-objc-class RacketApplicationDelegate NSObject ;; note: NSApplicationDelegate doesn't exist at run time
  []
  [-a _NSUInteger (applicationShouldTerminate: [_id app])
      (queue-quit-event)
      0]
  [-a _BOOL (openPreferences: [_id app])
      (queue-prefs-event)
      #t]
  [-a _BOOL (validateMenuItem: [_id menuItem])
      (cond 
       [(ptr-equal? (selector openPreferences:) 
                    (tell #:type _SEL menuItem action))
        (not (eq? (application-pref-handler) nothing-application-pref-handler))]
       [(ptr-equal? (selector openAbout:) 
                    (tell #:type _SEL menuItem action))
        #t]
       [else
        (super-tell #:type _BOOL validateMenuItem: menuItem)])]
  [-a _BOOL (openAbout: [_id sender])
      (if (eq? nothing-application-about-handler
               (application-about-handler))
          (tellv app orderFrontStandardAboutPanel: sender)
          (queue-about-event))
      #t]
  [-a _BOOL (application: [_id theApplication] openFile: [_NSString filename])
      (set! got-file? #t)
      (queue-file-event (string->path filename))
      (post-dummy-event)]
  [-a _void (applicationDidFinishLaunching: [_id notification])
      ;; Create an empty windows menu for right clicking in the dock
      (tell app setWindowsMenu: (tell (tell NSMenu alloc) init))
      (when (version-10.9-or-later?)
        (tell app addObserver:
              self
              forKeyPath: #:type _NSString "effectiveAppearance"
              options: #f
              context: #f))
      (unless got-file?
        (queue-start-empty-event))]
  [-a _void (observeValueForKeyPath: [_NSString keyPath] ofObject: ofObject change: change context: context)
      ;; we add an observer only for "effectiveAppearance",
      ;; so no check is needed to know why we got here
      (queue-dark-mode-event)
      (void)]
  [-a _BOOL (applicationShouldHandleReopen: [_id app] hasVisibleWindows: [_BOOL has-visible?])
      ;; If we have any visible windows, return #t to do the default thing.
      ;; Otherwise return #f, because we don't want any invisible windows resurrected.
      has-visible?]
  [-a _void (applicationDidChangeScreenParameters: notification)
      ;; Screen changes sometimes make the event loop get stuck;
      ;; hack: schedule a wake-up call in 5 seconds
      (let ([priviledged-custodian (make-custodian-at-root)])
        (parameterize ([current-custodian priviledged-custodian])
          (thread (lambda () (sleep 5.0)))))
      (unless (version-10.10-or-later?)
        ;; Also need to reset blit windows, since OS may move them incorrectly:
        (fixup-window-locations))]
  [-a _void (tryDockToFront: o)
      (try-dock-to-front)]
  [-a _void (retrySelfToFront: o)
      (tellv app activateIgnoringOtherApps: #:type _BOOL #t)])

(define fixup-window-locations void)
(define (set-fixup-window-locations! f) (set! fixup-window-locations f))

;; In case we were started in an executable without a bundle,
;; explicitly register with the dock so the application can receive
;; keyboard events.
(define-cstruct _ProcessSerialNumber
  ([highLongOfPSN _uint32]
   [lowLongOfPSN _uint32]))
(define kCurrentProcess 2)
(define kProcessTransformToForegroundApplication 1)
(define-appserv TransformProcessType (_fun _ProcessSerialNumber-pointer
                                           _uint32
                                           -> _OSStatus))
(define NSApplicationActivationPolicyRegular 0)
(define NSApplicationActivationPolicyAccessory 1)
(unless (register-process-global #"PLT_IS_FOREGROUND_APP" #f)
  (cond
   [(version-10.6-or-later?)
    ;; When a frame or root menu bar is created, we promote to
    ;; NSApplicationActivationPolicyRegular:
    (tellv app setActivationPolicy: #:type _int NSApplicationActivationPolicyAccessory)]
   [else
    (let ([v (TransformProcessType (make-ProcessSerialNumber 0 kCurrentProcess)
                                   kProcessTransformToForegroundApplication)])
      (unless (zero? v)
        (log-error (format "error from TransformProcessType: ~a" v))))]))

(define app-delegate (tell (tell RacketApplicationDelegate alloc) init))
(tellv app setDelegate: app-delegate)

(define (bring-to-front)
  (unless (register-process-global #"Racket-GUI-no-front" #f)
    (tellv app activateIgnoringOtherApps: #:type _BOOL #t)
    ;; It may not be that easy...
    (when (version-10.7-or-later?)
      (with-autorelease
          (import-class NSRunningApplication)
        (unless (tell #:type _BOOL (tell NSRunningApplication currentApplication) ownsMenuBar)
          ;; Looks like we haven't yet convinced the system to give us
          ;; the menu bar. Perform a menu-bar dance that is based on
          ;; http://stackoverflow.com/questions/7596643/when-calling-transformprocesstype-the-app-menu-doesnt-show-up
          (tellv app-delegate performSelector: #:type _SEL (selector tryDockToFront:) 
                 withObject: #f
                 afterDelay: #:type _double 0.1))))))
(define (promote-to-gui!)
  (when (version-10.6-or-later?)
    (tellv app setActivationPolicy: #:type _int NSApplicationActivationPolicyRegular))
  (bring-to-front))

(define (try-dock-to-front)
  ;; Phase 2 of the 10.9 menu-bar dance started above:
  (with-autorelease
   (import-class NSRunningApplication)
   (define docks (tell NSRunningApplication 
                       runningApplicationsWithBundleIdentifier: #:type _NSString "com.apple.dock"))
   (when (positive? (tell #:type _NSUInteger docks count))
     (define dock (tell docks firstObject))
     (define NSApplicationActivateIgnoringOtherApps 2)
     (tell #:type _BOOL dock 
           activateWithOptions: #:type _NSUInteger NSApplicationActivateIgnoringOtherApps)
     (tellv app-delegate performSelector: #:type _SEL (selector retrySelfToFront:) 
            withObject: #f 
            afterDelay: #:type _double 0.1))))

;; For some reason, nextEventMatchingMask:... gets stuck if the
;;  display changes, and it doesn't even send the 
;;  `applicationDidChangeScreenParameters:' callback. Unstick
;;  it by posting a dummy event, since we fortunately can receive
;;  a callback via CGDisplayRegisterReconfigurationCallback().
;; This seems to unstick things enough that `applicationDidChangeScreenParameters:'
;;  is called, but sometimes the event loop gets stuck after
;;  that, so there's an additional hack above.
(define-appserv CGDisplayRegisterReconfigurationCallback 
  (_fun (_fun #:atomic? #t #:async-apply (lambda (f) (f)) _uint32 _uint32 -> _void) _pointer -> _int32))
(define (on-screen-changed display flags)
  (screen-changed-callback flags)
  (post-dummy-event))
(define screen-changed-callback void)
(define (set-screen-changed-callback! c) (set! screen-changed-callback c))
(let ([v (CGDisplayRegisterReconfigurationCallback on-screen-changed #f)])
  (unless (zero? v)
    (log-error (format "error from CGDisplayRegisterReconfigurationCallback: ~a" v))))

(tellv app finishLaunching)

(when (version-10.9-or-later?)
  (define NSActivityIdleSystemSleepDisabled (arithmetic-shift 1 20))
  (define NSActivityUserInitiated #x00FFFFFF)
  (tellv
   (tell (tell NSProcessInfo processInfo)
         beginActivityWithOptions: #:type _uint64 (- NSActivityUserInitiated
                                                     NSActivityIdleSystemSleepDisabled)
         reason: #:type _NSString "Racket default")
   retain))

;; ------------------------------------------------------------
;; Create an event to post when Racket has been sleeping but is
;;  ready to wake up

(import-class NSEvent)
(define wake-evt
  (tell NSEvent 
        otherEventWithType: #:type _NSUInteger NSApplicationDefined
        location: #:type _NSPoint (make-NSPoint 0.0 0.0)
        modifierFlags: #:type _NSUInteger 0 
        timestamp: #:type _double 0.0
        windowNumber: #:type _NSUInteger 0
        context: #:type _pointer #f
        subtype: #:type _short 0
        data1: #:type _NSInteger 0
        data2: #:type _NSInteger 0))
(retain wake-evt)
(define (post-dummy-event)
  (tell #:type _void app postEvent: wake-evt atStart: #:type _BOOL YES))

;; This callback will be invoked by the CoreFoundation run loop
;; when data is available on `ready_sock', which is used to indicate
;; that Racket would like to wake up (and posting a Cocoa event
;; causes the event-getting function to unblock).
(define (socket_callback)
  (read2 ready_sock read-buf 1)
  (post-dummy-event))

;; ------------------------------------------------------------
;; Create a pipe's pair of file descriptors, used to communicate
;; from the Racket-sleep thread to the CoreFoundation run loop.

(define pipe2 (get-ffi-obj 'pipe #f (_fun _pointer -> _int)))
(define write2 (get-ffi-obj 'write #f (_fun _int _pointer _long -> _long)))
(define read2 (get-ffi-obj 'read #f (_fun _int _pointer _long -> _long)))
(define read-buf (make-bytes 1))
(define-values (ready_sock write_sock)
  (let ([s (malloc 'raw 2 _int)])
    (unless (zero? (pipe2 s))
      (error "pipe didn't create fds"))
    (let ([r (ptr-ref s _int 0)]
          [w (ptr-ref s _int 1)])
      (free s)
      (values r w))))

;; ------------------------------------------------------------
;; Register the event-posting callback on `ready_sock' with
;; the CoreFoundation run loop

(define _CFIndex _uint)
(define _CFStringRef _pointer) ; don't use NSString, because we don't want to acquire/release
(define-cstruct _CFSocketContext ([version _CFIndex]
                                  [info _pointer]
                                  [retain (_fun #:atomic? #t #:async-apply (lambda (f) (f))
                                                _pointer -> _pointer)]
                                  [release (_fun #:atomic? #t #:async-apply (lambda (f) (f))
                                                 _pointer -> _void)]
                                  [copyDescription (_fun #:atomic? #t #:async-apply (lambda (f) (f))
                                                         _pointer -> _NSString)]))
(define (sock_retain v) #f)
(define (sock_release v) (void))
(define (sock_copy_desc v) "sock")
(define sock-context (make-CFSocketContext 0 #f sock_retain sock_release sock_copy_desc))

(define _CFRunLoopRef _pointer)
(define _CFAllocatorRef _pointer)
(define _CFSocketRef _pointer)
(define _CFRunLoopSourceRef _pointer)
(define _CFSocketNativeHandle _int)
(define _CFOptionFlags _uint)
(define _CFSocketCallBack (_fun #:atomic? #t #:async-apply (lambda (f) (f)) -> _void))
(define-cf CFAllocatorGetDefault (_fun -> _pointer))
(define-cf CFSocketCreateWithNative (_fun _CFAllocatorRef
                                          _CFSocketNativeHandle
                                          _CFOptionFlags
                                          _CFSocketCallBack
                                          _CFSocketContext-pointer
                                          -> _CFSocketRef))
(define-cf CFSocketCreateRunLoopSource (_fun _CFAllocatorRef
                                             _CFSocketRef
                                             _CFIndex
                                             -> _CFRunLoopSourceRef))
(define-cf CFRunLoopAddSource (_fun _CFRunLoopRef
                                    _CFRunLoopSourceRef
                                    _CFStringRef
                                    -> _void))
(define-cf kCFRunLoopDefaultMode _CFStringRef)

(define kCFSocketReadCallBack 1)

(import-class NSRunLoop)
(let* ([rl (tell #:type _CFRunLoopRef (tell NSRunLoop currentRunLoop) getCFRunLoop)]
       [cfs (CFSocketCreateWithNative (CFAllocatorGetDefault) ready_sock kCFSocketReadCallBack
                                      socket_callback sock-context)]
       [source (CFSocketCreateRunLoopSource (CFAllocatorGetDefault) cfs 0)])
  (CFRunLoopAddSource rl source kCFRunLoopDefaultMode))

;; ------------------------------------------------------------
;; Another hack:
;; Install a run-loop observer that noticed when the core run loop
;; is exited multiple times during a single wait for a Cocoa event.
;; When that happens, it's a sign that something has gone wrong,
;; and we should interrupt the event wait and try again. This happens
;; when the user hides the application and then clicks on the dock
;; icon. (But why does that happen?)

(define _Boolean _BOOL)
(define-cf kCFRunLoopCommonModes _CFStringRef)
(define-cf CFRunLoopObserverCreate (_fun _pointer ; CFAllocatorRef
                                         _int ; CFOptionFlags
                                         _Boolean ; repeats?
                                         _CFIndex ; order
                                         (_fun #:atomic? #t #:async-apply (lambda (f) (f))
                                               _pointer _int _pointer -> _void)
                                         _CFStringRef ; CFRunLoopObserverContext
                                         -> _pointer))
(define-cf CFRunLoopAddObserver (_fun _pointer _pointer _CFStringRef -> _void))
(define-cf CFRunLoopGetMain (_fun -> _pointer))
(define kCFRunLoopExit (arithmetic-shift 1 7))
(define already-exited? #f)
(define sleeping? #f)
(define (exiting-run-loop x y z)
  (when sleeping?
    (if already-exited?
        (begin
          ;; should get out of the event loop:
          (post-dummy-event)
          ;; for good measure, alow let the scheduler know:
          (unsafe-signal-received))
        (set! already-exited? #t))))
(let ([o (CFRunLoopObserverCreate #f kCFRunLoopExit #t 0 exiting-run-loop #f)])
  (CFRunLoopAddObserver (CFRunLoopGetMain) o kCFRunLoopCommonModes))

;; ------------------------------------------------------------
;; Detecting menu-bar clicks:
;; In 10.13 and later, detecting a menu-bar click by NSSystemDefined
;; doesn't work, so we have to install a lower-level event tap.

(define mb-detect-box (box #f))
(define-cg CGEventTapCreate (_fun _uint32 _uint32 _uint32 _uint64
                                  (_fun #:atomic? #t
                                        #:async-apply (lambda (f) (f))
                                        #:keep mb-detect-box
                                        _pointer _uint32 _id _pointer -> _id)
                                  _pointer
                                  -> _pointer)
  #:fail (lambda () #f))
(define-cf CFMachPortCreateRunLoopSource (_fun _pointer _pointer _long -> _pointer))
(define-cf CFRunLoopGetCurrent (_fun -> _pointer))
(define-cg CGEventGetLocation (_fun _pointer -> _NSPoint)
  #:fail (lambda () #f))
(define-appkit NSEventTrackingRunLoopMode _CFStringRef)

(define in-menu-bar-detected? #f)

(define (menu-bar-tap-callback proxy type evt data)
  (when (in-menu-bar-range? (CGEventGetLocation evt) #t)
    (set! in-menu-bar-detected? #t))
  evt)

(define kCGSessionEventTap 1)
(define kCGAnnotatedSessionEventTap 2)
(define kCGHeadInsertEventTap 0)
(define kCGEventTapOptionDefault 0) ; => active
(define NX_LMOUSEDOWN 1)
(define NX_RMOUSEDOWN 3)
(define menu-bar-tap
  (and (version-10.13-or-later?)
       (not (version-10.14-or-later?))
       ;; 10.13: detecting `NSSystemDefined` with subtype 7 doesn't work,
       ;;        but it seems to work for all other OS versions
       (CGEventTapCreate kCGSessionEventTap #; kCGAnnotatedSessionEventTap
                         kCGHeadInsertEventTap
                         kCGEventTapOptionDefault
                         (bitwise-ior
                          (1 . arithmetic-shift . NX_LMOUSEDOWN)
                          (1 . arithmetic-shift . NX_RMOUSEDOWN))
                         menu-bar-tap-callback
                         (malloc-immobile-cell mb-detect-box))))
(when menu-bar-tap
  (define src (CFMachPortCreateRunLoopSource #f menu-bar-tap 0))
  (CFRunLoopAddSource (CFRunLoopGetCurrent) src kCFRunLoopCommonModes))

;; ------------------------------------------------------------
;; Cocoa event pump

(define-cocoa NSDefaultRunLoopMode _id) ; more specifically an _NSString, but we don't need a conversion

(import-class NSDate)
(define distantFuture (tell NSDate distantFuture))

(define eventspace-hook (lambda (e v) #f))
(define (set-eventspace-hook! proc) (set! eventspace-hook proc))

(define front-hook (lambda () (values #f #f)))
(define (set-front-hook! proc) (set! front-hook proc))

(define in-menu-bar-range? (lambda (p flipped?) #f))
(define (set-menu-bar-hooks! r?) 
  (set! in-menu-bar-range? r?))

(define events-suspended? #f)
(define was-menu-bar #f)

(define avoid-mouse-key-until #f)

(define mouse-or-key-hook void)
(define (set-mouse-or-key-hook! proc)
  (set! mouse-or-key-hook proc))

;; Check for menu-bar click to trigger `on-demand` callbacks.
;; Why not use a delegate on NSMenu? Because that's a less convenient
;; time to call arbitrary Racket code. It might be better to do that
;; using `call-as-nonatomic-retry-point` and `constrained-reply`, but
;; I'm not sure, and I'll stick with this for now.
(define (check-menu-bar-click evt)
  (if (if menu-bar-tap
          (and in-menu-bar-detected?
               (set! in-menu-bar-detected? #f)
               #t)
          (and evt
               (= NSSystemDefined (tell #:type _NSUInteger evt type))
               (= 7 (tell #:type _short evt subtype))
               (not (tell evt window))
               (in-menu-bar-range? (tell #:type _NSPoint evt locationInWindow) #f)))
      ;; Mouse down in the menu bar:
      (let-values ([(f e) (front-hook)])
        (when e
          ;; Avoid spiral of on-demand calls:
          (unless (and was-menu-bar
                       (eq? e (weak-box-value was-menu-bar)))
            ;; Don't handle further events until we've made an effort
            ;; at on-demand notifications.
            (set! was-menu-bar (make-weak-box e))
            (set! events-suspended? #t)
            (let* ([c (make-custodian)]
                   [t (parameterize ([current-custodian c])
                        (thread (lambda ()
                                  (sleep 2)
                                  ;; on-demand took too long, so wait
                                  ;; until the application can catch up
                                  (set! events-suspended? #f))))])
              (queue-event e (lambda ()
                               (send f on-menu-click)
                               (set! events-suspended? #f)
                               (custodian-shutdown-all c)))))))
      (set! was-menu-bar #f)))

(define NSAnyEventMask (sub1 (arithmetic-shift 1 (* 8 (ctype-sizeof _NSUInteger)))))

;; Call this function only in atomic mode:
(define (check-one-event wait? dequeue?)
  (pre-event-sync wait?)
  (clean-up-deleted)
  (let ([pool (tell (tell NSAutoreleasePool alloc) init)])
    (when (and events-suspended? wait?)
      (set! was-menu-bar #f)
      (set! events-suspended? #f))
    (when (and avoid-mouse-key-until
               ((current-inexact-milliseconds) . > . avoid-mouse-key-until))
      (set! avoid-mouse-key-until #f))
    (begin0
     (let ([evt (if events-suspended?
                    #f
                    (with-blocking-tell
                      (tell app nextEventMatchingMask: #:type _NSUInteger (if (and (not wait?)
                                                                                   avoid-mouse-key-until)
                                                                              (- NSAnyEventMask
                                                                                 MouseAndKeyEventMask)
                                                                              NSAnyEventMask)
                            untilDate: (if wait? distantFuture #f)
                            inMode: NSDefaultRunLoopMode
                            dequeue: #:type _BOOL dequeue?)))])
       (when evt (check-menu-bar-click evt))
       (and evt
            (or (not dequeue?)
                (let* ([w (tell evt window)]
                       [e (eventspace-hook evt w)])
                  (if e
                      (let ([mouse-or-key?
                             (bitwise-bit-set? MouseAndKeyEventMask
                                               (tell #:type _NSInteger evt type))])
                        (when mouse-or-key?
                          (mouse-or-key-hook w))
                        ;; If it's a mouse or key event, delay further
                        ;;  dequeue of mouse and key events until this
                        ;;  one can be handled.
                        (when mouse-or-key?
                          (set! avoid-mouse-key-until
                                (+ (current-inexact-milliseconds) 200.0)))
                        (retain evt)
                        (queue-event e (lambda () 
                                         (call-as-nonatomic-retry-point
                                          (lambda ()
                                            ;; in atomic mode
                                            (with-autorelease
                                             (tellv app sendEvent: evt)
                                             (release evt))))
                                         (when mouse-or-key?
                                           (set! avoid-mouse-key-until #f)))))
                      (tellv app sendEvent: evt)))
                #t)))
     (tellv pool release))))

;; Call this function only in atomic mode:
(define (dispatch-all-ready)
  (when (check-one-event #f #t)
    (dispatch-all-ready)))

(define (cocoa-start-event-pump)
  (thread (lambda ()
            (let loop ()
              ;; Wait 50 msecs between event polling, unless nothing
              ;; else is going on:
              (sync/timeout 0.05 (system-idle-evt))
              ;; Wait until event is ready --- but waiting is implemented
              ;; by polling:
              (sync queue-evt)
              ;; Something is ready, so dispatch:
              (atomically (dispatch-all-ready))
              ;; Periodically free everything in the default allocation pool:
              (queue-autorelease-flush)
              (loop)))))

(set-check-queue!
 ;; Called through an atomic callback:
 (lambda () (check-one-event #f #f)))

(define (try-to-sync-refresh)
  ;; atomically => outside of the event loop
  (atomically
   (pre-event-sync #t)))

(define (try-to-flush)
  (tell app nextEventMatchingMask: #:type _NSUInteger 0
        untilDate: #f
        inMode: NSDefaultRunLoopMode
        dequeue: #:type _BOOL #t)
  (void))

(set-platform-queue-sync!
 (lambda ()
   ;; in atomic mode
   (dispatch-all-ready)))

(define (sync-cocoa-events)
  (atomically
   (dispatch-all-ready)))

;; ------------------------------------------------------------
;; Install an alternate "sleep" function (in the Racket core)
;; that wakes up if any Cocoa event is ready.
  
;; Called through an atomic callback:
(define (sleep-until-event)
  (set! sleeping? #t)
  (set! already-exited? #f)
  ;; blocks until an event is ready, which can include a post from a
  ;; background `sleep` thread that that triggers `write_sock`:
  (check-one-event #t #f)
  (set! sleeping? #f))

(define (cocoa-install-event-wakeup)
  (post-dummy-event) ; why do we need this? 'nextEventMatchingMask:' seems to hang if we don't use it
  (unsafe-set-sleep-in-thread! sleep-until-event write_sock))
