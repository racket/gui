#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         racket/class
         racket/path
         "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "queue.rkt"
         "frame.rkt")

(provide (protect-out file-selector))

(import-class NSOpenPanel NSSavePanel NSURL NSArray
              NSMenu NSMenuItem
              NSTimer NSRunLoop)

(define-appkit NSModalPanelRunLoopMode _id)

;; used for fixup-panel-showing;
(define-objc-class RacketFileDialogDelegate NSObject
  [ns]
  [-a _void (windowDidMove: sender)
      (tellv ns setAlphaValue: #:type _CGFloat 1.0)])

(define (nsurl->string url)
  (string->path (tell #:type _NSString url path)))

(define (file-selector message directory filename 
                       extension
                       filters style parent)
  (promote-to-gui!)
  (force-global-flush-resume)
  (let ([ns (as-objc-allocation-with-retain
             (if (memq 'put style)
                (tell NSSavePanel savePanel)
                (tell NSOpenPanel openPanel)))]
        [parent (and parent
                     ; (not (version-12.0-or-later?))
                     (not (send parent get-sheet))
                     parent)])

    (let* ([globs (apply append
                         (map (lambda (f) (regexp-split #rx" *; *" (cadr f)))
                              filters))]
           ;; get suffixes from "*.foo" globs (and *only* such globs)
           [extensions
            (for/list ([g (in-list globs)]
                       #:when (and (regexp-match #rx"[*][.][^.]+$" g)
                                   (not (equal? g "*.*"))))
              (car (regexp-match #rx"[^.]+$" g)))]
           [extensions
            (if (memq 'packages style) (cons "app" extensions) extensions)]
           [extensions
            (if (and extension (not (equal? "" extension)))
              (cons extension extensions) extensions)])
      (unless (null? extensions)
        (when (memq 'put style)
          (tellv ns setCanSelectHiddenExtension: #:type _BOOL #t))
        (let ([allow-any? (member "*.*" globs)])
          (when (or (not allow-any?)
                    (memq 'put style))
            (let ([a (tell NSArray
                           arrayWithObjects: #:type (_list i _NSString) extensions
                           count: #:type _NSUInteger (length extensions))])
              (tellv ns setAllowedFileTypes: a))
            (tellv ns setAllowsOtherFileTypes: #:type _BOOL allow-any?)))))

    (cond
     [(memq 'multi style)
      (tellv ns setAllowsMultipleSelection: #:type _BOOL #t)]
     [(memq 'dir style)
      (tellv ns setCanChooseDirectories: #:type _BOOL #t)
      (tellv ns setCanChooseFiles: #:type _BOOL #f)])

    (when (or (memq 'put style)
              (memq 'dir style))
      (tellv ns setCanCreateDirectories: #:type _BOOL #t))

    (when message
      (tellv ns setMessage: #:type _NSString message))
    (when directory
      (let ([dir (if (string? directory)
                     directory
                     (path->string directory))])
        (if (version-10.6-or-later?)
            (tellv ns setDirectoryURL: (tell NSURL 
                                             fileURLWithPath: #:type _NSString dir
                                             isDirectory: #:type _BOOL #t))
            (tellv ns setDirectory: #:type _NSString dir))))
    (when filename
      (when (version-10.6-or-later?)
        (tellv ns setNameFieldStringValue: #:type _NSString (path->string
                                                             (file-name-from-path filename)))))
    
    (when (memq 'enter-packages style)
      (tellv ns setTreatsFilePackagesAsDirectories: #:type _BOOL #t))

    (let ([result 
           ;; We run the file dialog completely modally --- shutting out
           ;; all other eventspaces and threads. It would be nice to improve
           ;; on this, but it's good enough.
           (atomically
            (let ([front (get-front)]
                  [parent (and (version-10.6-or-later?)
                               parent)]
                  [completion (and (version-10.10-or-later?)
                                   parent
                                   ;; retain until done:
                                   (box null))]
                  [completion-result 0]
                  [orig-mb (tell app mainMenu)])
              (when orig-mb
                (tellv app setMainMenu: (make-standard-menu-bar)))
              (define finish-timer
                (cond
                  [(and parent
                        (version-12.0-or-later?))
                   (fixup-panel-showing ns)]
                  [else void]))
              (when parent
                (tellv ns beginSheetModalForWindow: (send parent get-cocoa-window)
                       completionHandler: #:type _pointer (and completion
                                                               (objc-block
                                                                (_fun #:atomic? #t #:keep completion _pointer _int -> _void)
                                                                (lambda (blk val)
                                                                  (set! completion-result val)
                                                                  (tellv app stopModal))
                                                                #:keep completion))))
              (begin0
               (if completion
                   ;; For 10.10, using `runModal` centers the sheet before
                   ;; running the model loop, so we have to use a completion
                   ;; handler as installed above plus `runModalForWindow:`
                   ;; (and this works despite the docs's claim that
                   ;; `runModalForWindow:` centers its argument).
                   (begin
                     (if (and (version-10.15-or-later?)
                              (not (version-12.0-or-later?)))
                         (tell ns runModal)
                         (tell app runModalForWindow: ns))
                     (set-box! completion #f)
                     completion-result)
                   ;; For 10.9 and earlier, runModel will do the hard part
                   ;; for us:
                   (tell #:type _NSInteger ns runModal))
               (when parent (tell app endSheet: ns))
               (when orig-mb (tellv app setMainMenu: orig-mb))
               (when front (tellv (send front get-cocoa-window)
                                  makeKeyAndOrderFront: #f))
               (finish-timer))))])
      (begin0
       (if (zero? result)
           #f
           (atomically
            (if (memq 'multi style)
                (let ([urls (tell ns URLs)])
                  (for/list ([i (in-range (tell #:type _NSUInteger urls count))])
                    (nsurl->string (tell urls objectAtIndex: #:type _NSUInteger i))))
                (let ([url (tell ns URL)])
                  (nsurl->string url)))))
       (release ns)))))

;; Would be better for names to be localized:
(define menu-names
  #hasheq((edit . "Edit")
          (undo . "Undo")
          (redo . "Redo")
          (cut . "Cut")
          (copy . "Copy")
          (paste . "Paste")))

(define (make-standard-menu-bar)
  (define mb (tell (tell NSMenu alloc) init))
  (define edit-item (tell (tell NSMenuItem alloc)
                          initWithTitle: #:type _NSString (hash-ref menu-names 'edit)
                          action: #:type _SEL #f
                          keyEquivalent: #:type _NSString ""))
  (define edit (tell (tell NSMenu alloc)
                     initWithTitle: #:type _NSString (hash-ref menu-names 'edit)))
  (tellv edit-item setSubmenu: edit)
  (define (add-item name sel shortcut)
    (tellv edit addItem: (tell (tell NSMenuItem alloc)
                               initWithTitle: #:type _NSString name
                               action: #:type _SEL sel
                               keyEquivalent: #:type _NSString shortcut)))
  (add-item (hash-ref menu-names 'undo) (selector undo:) "z")
  (add-item (hash-ref menu-names 'redo) (selector redo:) "Z")
  (tellv edit addItem: (tell NSMenuItem separatorItem))
  (add-item (hash-ref menu-names 'cut) (selector cut:) "x")
  (add-item (hash-ref menu-names 'copy) (selector copy:) "c")
  (add-item (hash-ref menu-names 'paste) (selector paste:) "v")
  (tellv mb addItem: (tell (tell NSMenuItem alloc)
                           initWithTitle: #:type _NSString "Application"
                           action: #:type _SEL #f
                           keyEquivalent: #:type _NSString ""))
  (tellv mb addItem: edit-item)
  mb)

;; Hack: On Monterey, a file dialog with a parent is first centered
;; and made visible, and then the sheet animation happens. Defeat that
;; initial visibility by setting the window's alpha to 0. Set alpha to
;; 1 when the window is moved, presumably to its sheet position. For the
;; very unlikely case that the sheet position is centered on the screen,
;; so no move happens, start a backup timer to show the window.
;; A second problem is that a save dialog doesn't get the keyboard focus
;; in the file-name text field. It would be nice if the timer could do
;; something about that, but I didn't find anything that worked.
(define (fixup-panel-showing ns)
  (define timer-keep (box null))
  (tellv ns setAlphaValue: #:type _CGFloat 0.0)
  (tellv ns center)
  (define delegate (tell RacketFileDialogDelegate alloc))
  (set-ivar! delegate ns ns)
  (tellv ns setDelegate: delegate)
  (define timer (tell NSTimer timerWithTimeInterval: #:type _double 2.0
                      repeats: #:type _BOOL #f
                      block: #:type _pointer (objc-block (_fun #:keep timer-keep #:atomic? #t _pointer -> _void)
                                                         (lambda (timer)
                                                           (tellv ns setAlphaValue: #:type _CGFloat 1.0)
                                                           (void))
                                                         #:keep timer-keep)))
  (tellv (tell NSRunLoop currentRunLoop) addTimer: timer forMode: NSModalPanelRunLoopMode)
  (lambda ()
    (tellv timer invalidate)
    (set! timer-keep null)))
