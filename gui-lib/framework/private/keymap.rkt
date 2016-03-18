#lang racket/base

(require string-constants
         racket/class
         racket/match
         racket/list
         racket/unit
         mred/mred-sig
         "../preferences.rkt"
         "sig.rkt"
         "keymap-global.rkt")

(provide keymap@)

(define-unit keymap@
  (import mred^
          [prefix finder: framework:finder^]
          [prefix handler: framework:handler^]
          [prefix frame: framework:frame^]
          [prefix editor: framework:editor^]
          [prefix text: framework:text^])
  (export (rename framework:keymap^
                  [-get-file get-file]))
  (init-depend mred^)

  ;; if I put this in main.rkt with the others, it doesn't happen
  ;; early enough... ? JBC, 2011-07-12
  (preferences:set-default 'framework:automatic-parens #f boolean?)


  (define user-keybindings-files (make-hash))
  
  (define (add-user-keybindings-file spec)
    (hash-ref
     user-keybindings-files
     spec
     (λ ()
       (let* ([path (spec->path spec)]
              [sexp (and (file-exists? path)
                         (parameterize ([read-accept-reader #t])
                           (call-with-input-file path read)))])
         (match sexp
           [`(module ,name ,lang ,x ...)
	    (cond
	     [(valid-keybindings-lang? lang)
	      (let ([km (dynamic-require spec '#%keymap)])
		(hash-set! user-keybindings-files spec km)
		(send user-keymap chain-to-keymap km #t))]
	     [else
	      (error 'add-user-keybindings-file
		     (string-constant user-defined-keybinding-malformed-file/found-lang)
		     (path->string path)
		     lang)])]
           [else (error 'add-user-keybindings-file 
                        (string-constant user-defined-keybinding-malformed-file)
                        (path->string path))])))))
  
  (define (valid-keybindings-lang? x)
    (member x
            (list `(lib "keybinding-lang.ss" "framework")
                  `(lib "keybinding-lang.rkt" "framework")
                  `(lib "framework/keybinding-lang.ss")
                  `(lib "framework/keybinding-lang.rkt")
                  `framework/keybinding-lang)))
  
  (define (spec->path p)
    (cond
      [(path? p) p]
      [else
       (let* ([mod-name ((current-module-name-resolver) p #f #f #t)]
              [str (symbol->string mod-name)]
              [pth (substring str 1 (string-length str))])
         (let-values ([(base name _) (split-path pth)])
           (let ([filenames
                  (sort
                   (filter (λ (x) (substring? (path->string name) x))
                           (map path->string (directory-list base)))
                   (λ (x y) (> (string-length x) (string-length y))))])
             (when (null? filenames)
               (error 'spec->path "could not convert ~s, found no filenames for ~s" p mod-name))
             (build-path base (car filenames)))))]))
  
  (define (substring? s1 s2)
    (and (<= (string-length s1) 
             (string-length s2))
         (string=? s1 (substring s2 0 (string-length s1)))))
  
  (define (remove-user-keybindings-file spec)
    (let/ec k
      (let ([km (hash-ref user-keybindings-files spec (λ () (k (void))))])
        (send global remove-chained-keymap km)
        (hash-remove! user-keybindings-files spec))))
  
  (define (remove-chained-keymap ed keymap-to-remove)
    (let ([ed-keymap (send ed get-keymap)])
      (when (eq? keymap-to-remove ed-keymap)
        (error 'keymap:remove-keymap "cannot remove initial keymap from editor"))
      (let p-loop ([parent-keymap ed-keymap])
        (unless (is-a? parent-keymap aug-keymap<%>)
          (error 'keymap:remove-keymap
                 "found a keymap that is not a keymap:aug-keymap<%> ~e" 
                 parent-keymap))
        (let c-loop ([child-keymaps (send parent-keymap get-chained-keymaps)])
          (cond
            [(null? child-keymaps) 
             (void)]
            [else
             (let ([child-keymap (car child-keymaps)])
               (cond
                 [(eq? child-keymap keymap-to-remove)
                  (send parent-keymap remove-chained-keymap child-keymap)
                  (c-loop (cdr child-keymaps))]
                 [else 
                  (p-loop child-keymap)
                  (c-loop (cdr child-keymaps))]))])))))
  
  (define (set-chained-keymaps parent-keymap children-keymaps)
    (for-each (λ (orig-sub) (send parent-keymap remove-chained-keymap))
              (send parent-keymap get-chained-keymaps))
    (for-each (λ (new-sub) (send parent-keymap chain-to-keymap new-sub #f))
              children-keymaps))
  
  (define aug-keymap<%> (interface ((class->interface keymap%))
                          get-chained-keymaps
                          get-map-function-table
                          get-map-function-table/ht))
  
  (define aug-keymap-mixin
    (mixin ((class->interface keymap%)) (aug-keymap<%>)
      (define chained-keymaps null)
      (define/public (get-chained-keymaps) chained-keymaps)
      
      (define/override (chain-to-keymap keymap prefix?)
        (super chain-to-keymap keymap prefix?)
        (set! chained-keymaps
              (if prefix?
                  (cons keymap chained-keymaps)
                  (append chained-keymaps (list keymap)))))
      
      (define/override (remove-chained-keymap keymap)
        (super remove-chained-keymap keymap)
        (set! chained-keymaps (remq keymap chained-keymaps)))
      
      (define function-table (make-hasheq))
      (define/public (get-function-table) function-table)
      (define/override (map-function keyname fname)
        (super map-function (canonicalize-keybinding-string keyname) fname)
        (hash-set! function-table (string->symbol keyname) fname))
      
      (define/public (get-map-function-table)
        (get-map-function-table/ht (make-hasheq)))
      
      (define/public (get-map-function-table/ht table)
        (for ([(keyname fname) (in-hash function-table)])
          (define cs (canonicalize-keybinding-string (format "~a" keyname)))
          (define key (string->symbol cs))
          (unless (hash-ref table key #f)
            (when (on-this-platform? cs)
              (hash-set! table key fname))))
        (for ([chained-keymap (in-list chained-keymaps)])
          (when (is-a? chained-keymap aug-keymap<%>)
            (send chained-keymap get-map-function-table/ht table)))
        table)
      
      (define/private (on-this-platform? cs)
        (define splits
          (for/list ([x (in-list (split-out #\; (string->list cs)))])
            (all-but-last (split-out #\: x))))
        (define (has-key? k) (ormap (λ (x) (member (list k) x)) splits))
        (cond
          [(eq? (system-type) 'windows)
           (cond
             [(and (regexp-match? #rx"c:m" cs)
		   (not (regexp-match? #rx"~c:m" cs))
		   (not (regexp-match? #rx"~g:" cs)))
              #f]
             [(or (has-key? #\a) (has-key? #\d))
              #f]
             [else #t])]
          [(eq? (system-type) 'macosx)
           (cond
             [(has-key? #\m)
              #f]
             [else #t])]
          [(eq? (system-type) 'unix)
           (cond
             [(or (has-key? #\a) (has-key? #\d))
              #f]
             [else #t])]
          [else 
           ;; just in case new platforms come along .... 
           #t]))
      
      (define/private (all-but-last l)
        (cond
          [(null? l) l]
          [(null? (cdr l)) '()]
          [else (cons (car l) (all-but-last (cdr l)))]))
      
      (super-new)))
  
  (define aug-keymap% (aug-keymap-mixin keymap%))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;                                                     ;;;;;;;;
  ;;;;;;;           canonicalize-keybinding-string            ;;;;;;;;
  ;;;;;;;                                                     ;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; canonicalize-keybinding-string : string -> string
  ;; The result can be used with string=? to determine
  ;; if two key bindings refer to the same key.
  ;; Assumes a well-formed keystring.
  (define (canonicalize-keybinding-string str)
    (define chars (map char-downcase (string->list str)))
    (define separated-keys
      (map
       canonicalize-single-keybinding-string
       (split-out #\; chars)))
    (join-strings ";" separated-keys))
  
  ;; join-strings : string (listof string) -> string
  ;; concatenates strs with sep between each of them
  (define (join-strings sep strs)
    (if (null? strs)
        ""
        (apply
         string-append
         (cons
          (car strs)
          (let loop ([sepd-strs (cdr strs)])
            (cond
              [(null? sepd-strs) null]
              [else (list*
                     sep
                     (car sepd-strs)
                     (loop (cdr sepd-strs)))]))))))
  
  ;; canonicalize-single-keybinding-string : (listof char) -> string
  (define (canonicalize-single-keybinding-string chars)
    (let* ([neg? (char=? (car chars) #\:)]
           [mods/key (split-out #\: (if neg? (cdr chars) chars))]
           [mods
            (let loop ([mods mods/key])
              (cond
                [(null? mods) null]
                [(null? (cdr mods)) null]
                [else (cons (car mods) (loop (cdr mods)))]))]
           [key (apply string (car (last-pair mods/key)))]
           [canon-key
            (cond
              [(string=? key "enter") "return"]
              [(string=? key "del") "delete"]
              [(string=? key "ins") "insert"]
              [else key])]
           [shift (if neg? #f 'd/c)]
           [control (if neg? #f 'd/c)]
           [alt (if neg? #f 'd/c)]
           [meta (if neg? #f 'd/c)]
           [command (if neg? #f 'd/c)]
           [lock 'd/c]
           [altgr 'd/c]
           [question-mark 'd/c]
           
           [do-key
            (λ (char val)
              (cond
                [(eq? val #t) (string char)]
                [(eq? val #f) (string #\~ char)]
                [(eq? val 'd/c) #f]))])
      
      (for-each (λ (mod)
                  (let ([val (not (char=? (car mod) #\~))])
                    (case (if (char=? (car mod) #\~)
                              (cadr mod)
                              (car mod))
                      [(#\s) (set! shift val)]
                      [(#\c) (set! control val)]
                      [(#\a) (set! alt val)]
                      [(#\d) (set! command val)]
                      [(#\m) (set! meta val)]
                      [(#\l) (set! lock val)]
                      [(#\g) (set! altgr val)]
                      [(#\?) (set! question-mark val)])))
                mods)
      
      (join-strings ":"
                    (filter
                     values
                     (list
                      (do-key #\? question-mark)
                      (do-key #\a alt)
                      (do-key #\c control)
                      (do-key #\d command)
                      (do-key #\m meta)
                      (do-key #\s shift)
                      (do-key #\l lock)
		      (and (eq? 'windows (system-type))
			   control
			   meta
			   (do-key #\g altgr))
                      canon-key)))))
  
  ;; split-out : char (listof char) -> (listof (listof char))
  ;; splits a list of characters at its first argument
  ;; if the last character is the same as the first character,
  ;; it is not split into an empty list, but returned.
  (define (split-out split-char chars)
    (let loop ([chars chars]
               [this-split null]
               [all-split null])
      (cond
        [(null? chars)
         (reverse (cons (reverse this-split) all-split))]
        [else (let ([char (car chars)])
                (cond
                  [(char=? split-char char)
                   (if (null? (cdr chars))
                       (loop null
                             (cons char this-split)
                             all-split)
                       (loop (cdr chars)
                             null
                             (cons (reverse this-split) all-split)))]
                  [else
                   (loop (cdr chars)
                         (cons char this-split)
                         all-split)]))])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;                                                     ;;;;;;;;
  ;;;;;;;         end canonicalize-keybinding-string          ;;;;;;;;
  ;;;;;;;                                                     ;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define make-meta-prefix-list keymap:make-meta-prefix-list)

  (define send-map-function-meta keymap:send-map-function-meta)

  (define region-click keymap:region-click)
  
  (define add-to-right-button-menu keymap:add-to-right-button-menu)
  (define add-to-right-button-menu/before keymap:add-to-right-button-menu/before)

  (define (call/text-keymap-initializer thunk)
    (let ([ctki (current-text-keymap-initializer)])
      (parameterize ([current-text-keymap-initializer
                      (λ (keymap)
                        (send keymap chain-to-keymap global #t)
                        (ctki keymap))])
        (thunk))))
  
  (define (setup-global kmap #:alt-as-meta-keymap [alt-as-meta-keymap #f])
    (keymap:setup-global kmap #:alt-as-meta-keymap alt-as-meta-keymap)
    (define (goto-line keystroke-edit event)
      (define keystroke-frame
        (and (is-a? keystroke-edit editor:basic<%>)
             (send keystroke-edit get-top-level-window)))
      (define edit
        (cond
          [(is-a? keystroke-frame frame:info<%>)
           (send keystroke-frame get-info-editor)]
          [(is-a? keystroke-edit text%) keystroke-edit]
          [else #f]))
      (when edit
        (define num-str
          (call/text-keymap-initializer
           (λ ()
             (get-text-from-user
              (string-constant goto-line)
              (string-constant goto-line)))))
        (when (string? num-str)
          (define possible-num (string->number num-str))
          (define line-num (and possible-num (inexact->exact possible-num)))
          (cond
            [(and (number? line-num)
                  (integer? line-num)
                  (<= 1 line-num (+ (send edit last-paragraph) 1)))
             (define pos (send edit paragraph-start-position (sub1 line-num)))
             (send edit set-position pos)]
            [else
             (message-box
              (string-constant goto-line)
              (format 
               (string-constant goto-line-invalid-number)
               num-str
               (+ (send edit last-line) 1)))])))
      #t)

    (let ([add-m (λ (name func)
                   (send kmap add-function name func)
                   (when alt-as-meta-keymap
                     (send alt-as-meta-keymap add-function name func)))]
          [map-meta (λ (key func)
                      (keymap:send-map-function-meta kmap key func
                                                     (regexp-match? has-control-regexp key)
                                                     #:alt-as-meta-keymap alt-as-meta-keymap))])
      (add-m "goto-line" goto-line)
      (map-meta "g" "goto-line")))
        
  (define setup-search
    (let* ([send-frame
            (λ (invoke-method)
              (λ (edit event)
                (let ([frame
                       (cond
                         [(is-a? edit editor<%>)
                          (let ([canvas (or (send edit get-active-canvas)
                                            (send edit get-canvas))])
                            (and canvas
                                 (send canvas get-top-level-window)))]
                         [(is-a? edit area<%>)
                          (send edit get-top-level-window)]
                         [else #f])])
                  (if frame
                      (invoke-method frame)
                      (bell)))
                #t))])
      (λ (kmap #:alt-as-meta-keymap [alt-as-meta-keymap #f])
        (let* ([map (λ (key func) 
                      (send kmap map-function key func))]
               [map-meta (λ (key func)
                           (send-map-function-meta kmap key func
                                                   (regexp-match? has-control-regexp key)
                                                   #:alt-as-meta-keymap alt-as-meta-keymap))]
               [add (λ (name func)
                      (send kmap add-function name func))]
               [add-m (λ (name func)
                        (send kmap add-function name func)
                        (when alt-as-meta-keymap
                          (send alt-as-meta-keymap add-function name func)))])
          
          (add-m "search forward" 
                 (send-frame (λ (f) (send f search 'forward))))
          (add "search backward" 
               (send-frame (λ (f) (send f search 'backward))))
          (add "replace & search forward" 
               (send-frame (λ (f) (send f replace&search 'forward))))
          (add "replace & search backward" 
               (send-frame (λ (f) (send f replace&search 'backward))))
          (add "unhide search and toggle focus"
               (send-frame (λ (f) (send f unhide-search-and-toggle-focus))))
          (add "hide-search" 
               (send-frame (λ (f) (send f hide-search))))
          
          (map "c:g" "hide-search")
          (map "f3" "unhide search and toggle focus")
          (map "c:s" "unhide search and toggle focus")
          (map "c:r" "search backward")
          (case (system-type)
            [(unix)
             (map-meta "%" "search forward")])))))
  
  (define setup-file
    (let* ([get-outer-editor ;; : text% -> text%
            ;; returns the outermost editor, if this editor is nested in an editor snip.
            (λ (edit)
              (let loop ([edit edit])
                (let ([admin (send edit get-admin)])
                  (cond
                    [(is-a? admin editor-snip-editor-admin<%>)
                     (loop (send (send (send admin get-snip) get-admin) get-editor))]
                    [else edit]))))]
           [save-file-as
            (λ (this-edit event)
              (let ([edit (get-outer-editor this-edit)])
                (parameterize ([finder:dialog-parent-parameter 
                                (and (is-a? edit editor:basic<%>)
                                     (send edit get-top-level-window))])
                  (let ([file (finder:put-file)])
                    (when file
                      (send edit save-file/gui-error file)))))
              #t)]
           [save-file
            (λ (this-edit event)
              (let ([edit (get-outer-editor this-edit)])
                (if (send edit get-filename)
                    (send edit save-file/gui-error)
                    (save-file-as edit event)))
              #t)]
           [load-file
            (λ (edit event)
              (define (fallback)
                (let ([fn (send edit get-filename)])
                  (handler:open-file
                   (and fn
                        (let-values ([(base name dir) (split-path fn)])
                          base)))))
              (cond
                [(is-a? edit editor:basic<%>)
                 (define fr (send edit get-top-level-window))
                 (cond
                   [(is-a? fr frame:standard-menus<%>)
                    (send fr file-menu:open-callback 
                          (send fr file-menu:get-open-item)
                          event)]
                   [else (fallback)])]
                [else (fallback)])
              #t)])
      (λ (kmap #:alt-as-meta-keymap [alt-as-meta-keymap #f])
        (let* ([map (λ (key func) 
                      (send kmap map-function key func))]
               [map-meta (λ (key func)
                           (send-map-function-meta kmap key func 
                                                   (regexp-match? has-control-regexp key)
                                                   #:alt-as-meta-keymap alt-as-meta-keymap))]
               [add (λ (name func)
                      (send kmap add-function name func))]
               [add-m (λ (name func)
                        (send kmap add-function name func)
                        (when alt-as-meta-keymap
                          (send alt-as-meta-keymap add-function name func)))])
          
          (add "save-file" save-file)
          (add "save-file-as" save-file-as)
          (add "load-file" load-file)
          
          (map "c:x;c:s" "save-file")
          (map "d:s" "save-file")
          (map "c:x;c:w" "save-file-as")
          (map "c:x;c:f" "load-file")))))
  
  (define (setup-editor kmap)
    (let ([add/map
           (λ (func op key)
             (send kmap add-function
                   func
                   (λ (editor evt)
                     (send editor do-edit-operation op)))
             (send kmap map-function
                   (string-append
                    (case (system-type)
                      [(macosx macos) "d:"]
                      [(windows unix) "c:"]
                      [else (error 'keymap.rkt "unknown platform: ~s" (system-type))])
                    key)
                   func))])
      (add/map "editor-undo" 'undo "z")
      (unless (eq? (system-type) 'macosx)
        (add/map "editor-redo" 'redo "y"))
      (add/map "editor-cut" 'cut "x")
      (add/map "editor-copy" 'copy "c")
      (add/map "editor-paste" 'paste "v")
      (add/map "editor-select-all" 'select-all "a")))
  
  (define (generic-setup keymap)
    (add-editor-keymap-functions keymap)
    (add-pasteboard-keymap-functions keymap)
    (add-text-keymap-functions keymap))
  
  (define user-keymap (make-object aug-keymap%))
  (define (get-user) user-keymap)
  
  (define global (make-object aug-keymap%))
  (define global-main (make-object aug-keymap%))
  (define global-alt-as-meta (make-object aug-keymap%))
  (send global chain-to-keymap global-main #f)
  (generic-setup global-main)
  (generic-setup global-alt-as-meta)
  (setup-global global-main #:alt-as-meta-keymap global-alt-as-meta)
  (define (get-global) global)
  
  (define file (make-object aug-keymap%))
  (define file-alt-as-meta (make-object aug-keymap%))
  (generic-setup file)
  (setup-file file #:alt-as-meta-keymap file-alt-as-meta)
  (define (-get-file) file)
  
  (define search (make-object aug-keymap%))
  (define search-alt-as-meta (make-object aug-keymap%))
  (generic-setup search)
  (setup-search search #:alt-as-meta-keymap search-alt-as-meta)
  (define (get-search) search)
  
  (define editor (make-object aug-keymap%))
  (setup-editor editor)
  (define (get-editor) editor)
  
  (preferences:set-default 'framework:alt-as-meta #f boolean?)
  (define (adjust-alt-as-meta on?)
    (send global-main remove-chained-keymap global-alt-as-meta)
    (send file remove-chained-keymap file-alt-as-meta)
    (send search remove-chained-keymap search-alt-as-meta)
    (when on?
      (send global-main chain-to-keymap global-alt-as-meta #f)
      (send file chain-to-keymap file-alt-as-meta #f)
      (send search chain-to-keymap search-alt-as-meta #f)))
  (preferences:add-callback 'framework:alt-as-meta
                            (λ (p v) (adjust-alt-as-meta v)))
  (adjust-alt-as-meta (preferences:get 'framework:alt-as-meta)))

