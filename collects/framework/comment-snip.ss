(module comment-snip mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (provide snip-class comment-box-snip%)
  
  (define comment-box-snipclass%
    (class snip-class%
      (define/override (read stream-in)
        (let* ([snip (instantiate comment-box-snip% ())])
          (send (send snip get-editor) read-from-file stream-in)
          snip))
      (super-instantiate ())))
   
  (define snip-class (make-object comment-box-snipclass%))
  (send snip-class set-version 1)
  (send snip-class set-classname (format "~s" '(lib "comment-snip.ss" "framework")))
  (send (get-the-snip-class-list) add snip-class)
  
  (define comment-box-snip%
    (class* editor-snip% (readable-snip<%>)
      (inherit get-editor get-style)
      (define/override (write stream-out)
        (send (get-editor) write-to-file stream-out 0 'eof))
      (define/override (copy)
        (let ([snip (make-object comment-box-snip%)])
          (send snip set-editor (send (get-editor) copy-self))
          (send snip set-style (get-style))
          snip))
      (define/public (read-one-special index source line column position)
        (raise (make-exn:special-comment
                "msg"
                (current-continuation-marks)
                1)))
      (super-instantiate ())
      (inherit set-snipclass)
      (set-snipclass snip-class))))