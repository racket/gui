(module aligned-pasteboard mzscheme
  
  (require
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "click-forwarding-editor.ss" "mrlib")
   "geometry-managed-pasteboard.ss"
   "event-handling-pasteboard.ss"
   "locked-pasteboard.ss")
  
  (provide
   vertical-pasteboard%
   horizontal-pasteboard%)
  
  ;; contruct the basic mixin that both pasteboards will be created from
  (define (make-aligned-pasteboard type)
    (editor:basic-mixin
     (click-forwarding-editor-mixin
      (locked-pasteboard-mixin
       (event-handling-pasteboard-mixin
        (geometry-managed-pasteboard-mixin
         pasteboard% type))))))
  
  (define vertical-pasteboard%
    (make-aligned-pasteboard 'vertical))
  
  (define horizontal-pasteboard%
    (make-aligned-pasteboard 'horizontal))
  )
