(module comment-snip mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           "decorated-editor-snip.ss"
           (lib "string-constant.ss" "string-constants"))
  
  (provide snip-class comment-box-snip%)
  
  (define comment-box-snipclass%
    (class decorated-editor-snipclass%
      (define/override (make-snip stream-in) (instantiate comment-box-snip% ()))
      (super-instantiate ())))
   
  (define snip-class (make-object comment-box-snipclass%))
  (send snip-class set-version 1)
  (send snip-class set-classname (format "~s" '(lib "comment-snip.ss" "framework")))
  (send (get-the-snip-class-list) add snip-class)
  
  (define bm (let ([file (build-path (collection-path "icons") "semicolon.gif")])
               (and (file-exists? file)
                    (let ([bm (make-object bitmap% file)])
                      (and (send bm ok?)
                           bm)))))
  
  (define comment-box-snip%
    (class* decorated-editor-snip% (readable-snip<%>)
      (inherit get-editor get-style)
      (define/override (make-snip) (make-object comment-box-snip%))
      (define/override (get-corner-bitmap) bm)

      (define/override (make-menu)
        (let ([menu (make-object popup-menu%)])
          (make-object menu-item% 
            (string-constant convert-to-semicolon-comment)
            menu
            (lambda (x y)
              (let ([editor (find-containing-editor)])
                (when editor
                  (let ([this-pos (find-this-position)])
                    (when this-pos
                      (move-contents-with-semicolons-to-position editor (+ this-pos 1))
                      (send editor delete this-pos (+ this-pos 1))))))))))

      (inherit get-admin)
      ;; find-containing-editor : -> (union #f editor)
      (define/private (find-containing-editor)
        (let ([admin (get-admin)])
          (and admin
               (send admin get-editor))))

      ;; find-this-position : -> (union #f number)
      (define (find-this-position)
        (let ([ed (find-containing-editor)])
          (and ed
               (send ed get-snip-position this))))
      
      ;; move-contents-with-semicolons-to-position : (is-a? text%) number -> void
      (define (move-contents-with-semicolons-to-position to-ed pos)
        (let ([from-ed (get-editor)])
          (let loop ([snip (find-last-snip)])
            (cond
              [snip 
               (when (or (memq 'hard-newline (send snip get-flags))
                         (memq 'newline (send snip get-flags)))
                 (send to-ed insert ";" pos))
               (send from-ed release-snip)
               (send to-ed insert snip pos)
               (loop (send snip prev))]
              [else (void)]))))
      
      ;; find-last-snip : editor -> snip
      ;; returns the last snip in the editor
      (define (find-last-snip ed)
        (let loop ([snip (send ed find-first-snip)]
                   [acc (send ed find-first-snip)])
          (cond
            [snip (loop (send snip next) snip)]
            [else acc])))
        
      (define/public (read-one-special index source line column position)
        (raise (make-exn:special-comment
                "msg"
                (current-continuation-marks)
                1)))
      (super-instantiate ())
      (inherit set-snipclass)
      (set-snipclass snip-class))))