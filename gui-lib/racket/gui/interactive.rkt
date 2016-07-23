#lang racket/gui

(let ([init-file (cleanse-path
                  (build-path
                   (find-system-path 'init-dir)
                   ; Cludge because (find-system-path init-path) will
                   ;   always return .racketrc, even in gracket
                   (case (system-type)
                     [(unix macosx) ".gracketrc"]
                     [(windows) "gracketrc.rktl"])))])
  (when (file-exists? init-file)
    (load init-file)))
