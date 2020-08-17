#lang racket/base

(require racket/unit
         racket/class
         racket/gui/base
         racket/snip
         "sig.rkt"
         (prefix-in base: racket/base))

(provide srcloc-snip@)

(define-unit srcloc-snip@
  (import [prefix frame: framework:frame^]
          [prefix group: framework:group^]
          [prefix text: framework:text^]
          [prefix editor: framework:editor^])
  (export (rename framework:srcloc-snip^ [-snip% snip%]))

  (define (select-srcloc srcloc)
    (let frame-loop ([frames (send (group:get-the-frame-group) get-frames)])
      (unless (null? frames)
        (let ([frame (car frames)])
          (cond
            [(and (is-a? frame frame:editor<%>)
                  (send frame find-editor
                        (lambda (editor)
                          (send editor port-name-matches? (srcloc-source srcloc)))))
             => (lambda (editor)
                  (show-editor frame editor)
                  (send (send frame get-interactions-text)
                        highlight-error
                        editor (srcloc-position srcloc) (+ (srcloc-position srcloc) (srcloc-span srcloc))))]
            [else
             (frame-loop (cdr frames))])))))

  (define (show-editor frame editor)
    (let* ([current-tab (send editor get-tab)]
           [frame (send current-tab get-frame)])
      (let loop ([tabs (send frame get-tabs)] [i 0])
        (unless (null? tabs)
          (if (eq? (car tabs) current-tab)
              (send frame change-to-nth-tab i)
              (loop (cdr tabs) (+ i 1)))))
      (send frame show #t)))

  ; honest attempt
  (define (source->datum source)
    (if (path? source)
        (path->bytes source)
        source))

  (define (datum->source source)
    (if (bytes? source)
        (bytes->path source)
        source))
  
  (define srcloc-snip-class%
    (class snip-class%
      (inherit set-version set-classname)
      (super-new)
      (set-version 1)
      (set-classname (format "~s" '((lib "srcloc-snip.rkt" "framework")
                                    (lib "wxme-srcloc-snip.rkt" "framework"))))
      ; serialize as (srcloc <source> <line> <column> <position> <span>) <text>
      (define/override (read f)
        (with-handlers ([exn? (lambda (exn) #f)])
          (let* ((bytes (send f get-unterminated-bytes))
                 (port (open-input-bytes bytes 'srcloc))
                 (datum (base:read port))
                 (srcloc (apply
                          (lambda (_ source line column position span)
                            (srcloc (datum->source source) line column position span))
                          datum))
                 (snip
                  (new -snip% [srcloc srcloc]))
                 (editor (send snip get-editor)))
            (send editor read-from-file f #t)
            (send snip activate-link)
            snip)))))

  (define snipclass (new srcloc-snip-class%))
  (send (get-the-snip-class-list) add snipclass)

  ;; class for snips embedded in markup
  (define markup-text%
    (text:wide-snip-mixin
     (text:basic-mixin
      (editor:standard-style-list-mixin
       (editor:basic-mixin
        text%)))))
  
  (define -snip%
    (class editor-snip%
      (init-field srcloc)
      (inherit set-snipclass
               use-style-background
               get-editor)
      (super-new [editor (new markup-text%)]
                 [with-border? #f])
      (set-snipclass snipclass)

      ; you must call this after having put something in the editor
      (define/public (activate-link)
        (let ((editor (get-editor)))
          (send editor set-clickback
                0 (send editor get-end-position)
                (lambda (t s e)
                  (select-srcloc srcloc))
                #f #f)
          (send editor lock #t)))
      
      (use-style-background #t)
      
      
      (define/override (copy)
        (let ((snip (new -snip% [srcloc srcloc])))
          (send (get-editor) copy-self-to (send snip get-editor))
          (send snip activate-link)
          snip))
      
      (define/override (write f)
        (let ((port (open-output-string))
              (sexpr `(srcloc ,(source->datum (srcloc-source srcloc))
                              ,(srcloc-line srcloc)
                              ,(srcloc-column srcloc)
                              ,(srcloc-position srcloc)
                              ,(srcloc-span srcloc))))
          (base:write sexpr port)
          (let ((bytes (get-output-bytes port)))
            (send f put (bytes-length bytes) bytes))
          (send (get-editor) write-to-file f))))))
