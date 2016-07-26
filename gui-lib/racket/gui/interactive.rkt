#lang racket/base

(require racket/gui/base)

(let ([init-file (cleanse-path (find-graphical-system-path 'init-file))])
  (when (file-exists? init-file)
    (load init-file)))
