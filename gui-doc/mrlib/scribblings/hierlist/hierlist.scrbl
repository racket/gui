#lang scribble/doc
@(require "../common.rkt" (for-label mrlib/hierlist))

@title[#:style 'toc]{Hierarchical List Control}

@defmodule[mrlib/hierlist]

A @racket[hierarchical-list%] control is a list of items, some of
which can themselves be hierarchical lists. Each such sub-list has an
arrow that the user can click to hide or show the sub-list's items.

A short example to demonstrate this control:

@codeblock{
#lang racket/gui
(require mrlib/hierlist)

(define set-text-mixin
  (mixin (hierarchical-list-item<%>)
    ((interface () set-text))
    (inherit get-editor)
    (super-new)
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (send t insert str))))

; new class uses hierarchical-list% to show a directory
(define directory-list%
  (class hierarchical-list% (init [dir (current-directory-for-user)])
    (define the-dir dir)
    ; new-item : create new item for a file or directory
    (define (new-item parent directory subpath)
      (define item
        (if (file-exists? (build-path directory subpath))
            (send parent new-item set-text-mixin)
            (send parent new-list set-text-mixin)))
      (send item set-text (path->string subpath))
      (send item user-data (build-path directory subpath))
      item)
    ; Set the top level item, and populate it with an entry
    ; for each item in the directory.
    (define/public (set-directory dir)
      (send this delete-item top-dir-list) ; remove previous top item
      (set! top-dir-list (send this new-list set-text-mixin))
      (send top-dir-list set-text (path->string dir))
      ; add new-item for each member of dir
      (for ([i (directory-list dir)])
        (new-item top-dir-list dir i)))
    (super-new)
    ; top item in hierlist
    (define top-dir-list (send this new-list set-text-mixin))
    ; initialise directory-list% instance
    (set-directory the-dir)))

; Create frame
(define f (new frame% [label "frame"] [width 400] [height 400]))
; show frame onscreen
(send f show #t)
; create a directory-list%
(define my-dir (new directory-list%
                    [parent f]
                    [dir (find-system-path 'home-dir)]))
; change directory example - try this in the interactions window:
; (send my-dir set-directory (find-system-path 'doc-dir))
}

The list control supports the following default keystrokes:

@itemize[

 @item{Down: move to the next entry at the current level (skipping lower levels).}

 @item{Up: move to the previous entry at the current level (skipping lower levels).}

 @item{Left: move to the enclosing level (only valid at embedded levels).}

 @item{Right: move down in one level (only valid for lists).}

 @item{Return:  open/close the current selected level (only valid for lists).}

]


@local-table-of-contents[]

@include-section["list.scrbl"]
@include-section["item.scrbl"]
@include-section["compound-item.scrbl"]
@include-section["snips.scrbl"]

