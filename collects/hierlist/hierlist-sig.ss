
(module hierlist-sig mzscheme
  (import (lib "unitsig.ss"))

  (export hierlist^)
  (define-signature hierlist^
    (hierarchical-list%
     hierarchical-list-item<%>
     hierarchical-list-item%
     hierarchical-list-compound-item<%>
     hierarchical-list-compound-item%)))


