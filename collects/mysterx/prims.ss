;; prims.ss

(unit->unit/sig 
 (load-extension 
  (build-path (collection-path "mysterx") "dlls" "mxmain.dll"))
 ()
 mysterx:prims^)
