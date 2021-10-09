#lang info

(define collection 'multi)

(define deps '("gui-lib"
               "gui-doc"))
(define implies '("gui-lib"
                  "gui-doc"))

(define pkg-desc "Graphical user interface toolkit")

(define pkg-authors '(mflatt robby))

(define license
  '(Apache-2.0 OR MIT))
