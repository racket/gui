
(letrec* ([d (let ([wd (current-load-relative-directory)])
	       (if (file-exists? (build-path wd "demo.ss"))
		   wd
		   (let ([dd (build-path wd "demo")])
		     (if (file-exists? (build-path dd "demo.ss"))
			 dd
			 wd))))]
	  [f (make-object mred:simple-menu-frame%)]
	  [p2 (make-object mred:horizontal-panel% (send f get-top-panel))]
	  [p1 (make-object mred:horizontal-panel% (send f get-top-panel))]
	  [loader-button
	   (lambda (p name file instr go)
	     (make-object mred:button% p
			  (lambda (b e)
			    (load (build-path d file))
			    (send edit lock #f)
			    (send edit load-file (build-path d instr))
			    (send edit lock #t)
			    (go))
			  name))]
	  [canvas (send f get-canvas)]
	  [edit (send f get-edit)])
  (send (send f get-top-panel) change-children reverse)
  (send p2 stretchable-in-y #f)
  (send p1 stretchable-in-y #f)
  (loader-button p1 "Phone Book" "phone.ss" "phone.mre" void)
  (loader-button p1 "NanoCAD" "ncad02x.ss" "ncad.mre" (lambda () (ncad:go)))
  (loader-button p1 "Morphing" "morph.ss" "morph.mre" void)
  (loader-button p1 "Proof Systems" "toyproof.ss" "toyproof.mre" void)
  (loader-button p1 "Turtles" "turtles.ss" "turtles.mre" void)
  (loader-button p2 "Minesweeper" "mines.ss" "mines.mre" void)
  (loader-button p2 "Console Graph" "graph.ss" "graph.mre" void)
  (loader-button p2 "Simple Pasteboard" "draw.ss" "draw.mre" (lambda () (draw:go)))
  (send edit set-auto-set-wrap #t)
  (send edit insert "Click on a button above.")
  (send edit lock #t)
  (send edit set-autowrap-bitmap null)
  (send canvas set-media edit)
  (send f show #t))


  