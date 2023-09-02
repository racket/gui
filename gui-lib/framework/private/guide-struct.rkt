#lang racket/base
(require racket/class)
(provide (struct-out guide) get-guides draw-the-lines)

;; these are internal definitions used by text-indent-guides
;; exported here for use in the test suite

(struct guide (indent [x #:mutable] guides) #:transparent)
;; indent : natural? or #f
;;   indicates how many spaces start this line
;;   or #f to indicate that this is a blank line
;;   for blank lines, sizing information can't be
;;   gotten from the editor on this line, as there may
;;   not be actual characters where the guides are
;; x : integer? or #f
;;   this is the x (editor) coordinate for the guide
;;   that starts at this line
;;   if the guide is a blank line guide, then this is #f
;; guides : (sorted-listof natural?)
;;   indicates the positions (from the start of this para)
;;   where guides are coming down

;; these are private methods, but are made public
;; to make it possible to write unit tests
(define-local-member-name get-guides draw-the-lines)
