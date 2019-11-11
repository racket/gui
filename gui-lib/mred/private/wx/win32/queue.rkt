#lang racket/base
(require ffi/unsafe
         racket/class
         ffi/unsafe/alloc
         ffi/unsafe/try-atomic
         ffi/unsafe/schedule
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "key.rkt"
         "wndclass.rkt"
         "../../lock.rkt"
         "../common/queue.rkt")

(provide (protect-out win32-start-event-pump)

         ;; from common/queue:
         current-eventspace
         queue-event
         queue-refresh-event
         yield)

;; ------------------------------------------------------------
;; Win32 event pump

(define _LPMSG _pointer)

(define-user32 GetQueueStatus (_wfun _UINT -> _DWORD))
(define-user32 GetMessageW (_wfun _LPMSG _HWND _UINT _UINT -> _BOOL))
(define-user32 PeekMessageW (_wfun _LPMSG _HWND _UINT _UINT _UINT -> _BOOL))
(define-user32 TranslateMessage (_wfun _LPMSG -> _BOOL))
(define-user32 DispatchMessageW (_wfun _LPMSG -> _LRESULT))
(define-user32 PostQuitMessage (_wfun _int -> _void))
(define-user32 EnumThreadWindows (_wfun _DWORD _fpointer _LPARAM -> _BOOL))
(define-user32 GetWindow (_wfun _HWND _UINT -> _HWND))
(define-kernel32 GetCurrentThreadId (_wfun -> _DWORD))
(define-user32 GetAncestor (_wfun _HWND _UINT -> _HWND))
(define-user32 IsChild (_wfun _HWND _HWND -> _BOOL))

(define _enum_proc (_wfun #:atomic? #t _HWND _LPARAM -> _BOOL))

(define free-msg
  ((deallocator)
   (lambda (msg) 
     (free msg))))

(define malloc-msg
  ((allocator free-msg)
   (lambda ()
     (define p (malloc _MSG 'raw))
     (cpointer-push-tag! p 'MSG)
     p)))

(define (events-ready?)
  ;; Check for events only since the last PeekMessage:
  (not (zero? (LOWORD (GetQueueStatus QS_ALLINPUT)))))

(define (install-wakeup fds)
  (pre-event-sync #t)
  (unsafe-poll-ctx-eventmask-wakeup fds QS_ALLINPUT))

(set-check-queue! events-ready?)
(set-queue-wakeup! install-wakeup)

(define other-peek-evt (make-semaphore))
(define peek-other-peek-evt (semaphore-peek-evt other-peek-evt))

(define (message-dequeue es hwnd)
  ;; Called in the eventspace for hwnd:
  (let ([t (eventspace-extra-table es)]
        [id (cast hwnd _HWND _intptr)])
    (atomically (hash-remove! t id))
    (let ([msg (malloc-msg)])
      (let loop ()
        (let ([v (PeekMessageW msg hwnd 0 0 PM_REMOVE)])
          ;; Since we called PeekMeessage in a thread other than the
          ;;  event-pump thread, set `other-peek-evt' so the pump
          ;;  knows to check again.
          (unless (sync/timeout 0 peek-other-peek-evt)
            (semaphore-post other-peek-evt))
          ;; Now handle the event:
          (when v
            (unless (generates-key-event? (cast msg _pointer _MSG-pointer))
              (TranslateMessage msg))
            (call-as-nonatomic-retry-point
             (lambda ()
               ;; in atomic mode:
               (DispatchMessageW msg)))
            ;; Maybe there's another event for this window:
            (loop))))
      (free-msg msg))))

(define (queue-message-dequeue es hwnd)
  ;; in atomic mode
  (let ([t (eventspace-extra-table es)]
        [id (cast hwnd _HWND _intptr)])
    (unless (hash-ref t id #f)
      (hash-set! t id #t)
      (queue-event es (lambda () (message-dequeue es hwnd))))))

;; For use only in the event-pump thread:
(define msg (malloc-msg))

(define known-hwnds (make-hash))

(define (check-window-event hwnd data)
  (hash-set! known-hwnds hwnd #t)
  ;; in atomic mode
  (let* ([root (let loop ([hwnd hwnd])
                 (let ([p (GetWindow hwnd GW_OWNER)])
                   (if p
                       (loop p)
                       hwnd)))]
         [wx (any-hwnd->wx root)])
    (if wx
        ;; One of our windows, so make sure its eventspace
        ;; asks for the message:
        (let ([v (PeekMessageW msg hwnd 0 0 PM_NOREMOVE)])
          (when v
            (queue-message-dequeue (send wx get-eventspace)
                                   hwnd)))
        ;; Not our window, so dispatch any available events
        (let loop ()
          (let ([v (PeekMessageW msg hwnd 0 0 PM_REMOVE)])
            (when v
              (TranslateMessage msg)
              (DispatchMessageW msg)
              (loop)))))
    #t))

(define check_window_event (function-ptr check-window-event _enum_proc))

(define minus-one (cast -1 _intptr _HWND))

(define (dispatch-all-ready)
  ;; in atomic mode
  (pre-event-sync #f)
  (clean-up-destroyed)

  ;; Windows uses messages above #x0400 to hilite items in the task
  ;; bar, implement input methods (such as pinyin input), etc. In any
  ;; case, these messages won't be handled by us, so they can't
  ;; trigger callbacks. Also, handle messages without a window.
  (let loop ()
    (let ([v (or (PeekMessageW msg #f WM_USER #xFFFF PM_REMOVE)
		 (PeekMessageW msg minus-one 0 0 PM_REMOVE))])
      (when v
        (TranslateMessage msg)
        (DispatchMessageW msg)
        (loop))))

  ;; Per-window checking lets us put an event in the right
  ;; eventspace:
  (hash-clear! known-hwnds)
  (EnumThreadWindows (GetCurrentThreadId) check_window_event 0)

  ;; Just in case, double-check for messages to windows that we
  ;; don't recognize from enumeration:
  (let loop ()
    (let ([v (PeekMessageW msg #f 0 0 PM_NOREMOVE)])
      (when v
	(let ([hwnd (MSG-hwnd msg)])
	  (unless (for/or ([k (in-hash-keys known-hwnds)])
		    (IsChild k hwnd))
	    (when (PeekMessageW msg hwnd 0 0 PM_REMOVE)
              (TranslateMessage msg)
              (DispatchMessageW msg)
              (loop))))))))

(define (win32-start-event-pump)
  (thread (lambda ()
            (let loop ()
              (unless (let ([any-tasks? (sync/timeout 0 boundary-tasks-ready-evt)])
                        (sync/timeout (and any-tasks? (* sometimes-delay-msec 0.001))
                                      queue-evt 
                                      other-peek-evt
                                      (if any-tasks?
                                          (wrap-evt (system-idle-evt)
                                                    (lambda (v) #f))
                                          boundary-tasks-ready-evt)))
                (atomically (pre-event-sync #t)))
              (as-entry dispatch-all-ready)
              (loop)))))
