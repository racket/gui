(define (test-creation name class-expression)
  '(test
   name
   (lambda (x) x)
   (lambda ()
     (send-sexp-to-mred
      `(send (make-object ,class-expression "test") show #t))
     (wait-for-frame "test")
     (send-sexp-to-mred
      '(send (get-top-level-focus-window) show #f))
     #t)))

(test-creation
 'basic%-creation
 'frame:basic%)
(test-creation
 'basic-mixin-creation
 '(frame:basic-mixin frame%))

(test-creation
 'standard-menus%-creation
 'frame:standard-menus%)
(test-creation
 'standard-menus-mixin
 '(frame:standard-menus-mixin frame:basic%))

(test-creation
 'text%-creation
 'frame:text%)
(test-creation
 'text-mixin-creation
 '(frame:text-mixin frame:editor%))
(test-creation
 'text-mixin-creation
 '(frame:text-mixin (frame:editor-mixin frame:standard-menus%)))

(test-creation
 'searchable%-creation
 'frame:searchable%)
(test-creation
 'searchable-mixin
 '(frame:searchable-mixin frame:text%))

(test-creation
 'info-mixin-creation
 '(frame:info-mixin frame:searchable%))
(test-creation
 'text-info-mixin-creation
 '(frame:text-info-mixin (frame:info-mixin frame:searchable%)))
(test-creation
 'text-info%-creation
 'frame:text-info%)

(test-creation
 'text-info-file%-creation
 'frame:text-info-file%)
(test-creation
 'text-info-file-mixin-creation
 '(frame:file-mixin frame:text-info%))

(test-creation
 'pasteboard-mixin-creation
 '(frame:pasteboard-mixin frame:editor%))
(test-creation
 'pasteboard-mixin-creation
 '(frame:pasteboard-mixin (frame:editor-mixin frame:standard-menus%)))
(test-creation
 'pasteboard%-creation
 'frame:pasteboard%)

(test-creation
 'pasteboard-info-mixin-creation
 '(frame:info-mixin frame:searchable%))
(test-creation
 'pasteboard-info%-creation
 'frame:pasteboard-info%)

(test-creation
 'pasteboard-info-file-mixin-creation
 '(frame:file-mixin frame:pasteboard-info%))
(test-creation
 'pasteboard-info-file%-creation
 'frame:pasteboard-info-file%)

(define (test-open name class-expression)
  (test
   name
   (lambda (x) x)
   (lambda ()
     (send-sexp-to-mred
      `(begin
	 (preferences:set
	  'framework:file-dialogs
	  'common)
	 (send (make-object ,class-expression "test open") show #t)))
     (wait-for-frame "test open")
     (send-sexp-to-mred
      `(test:menu-select "File" "Open..."))
     (wait-for-frame "Open File")
     #t)))

(test-open "frame:editor open" 'frame:text%)
