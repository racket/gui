(module interface mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred"))
  
  (provide
    aligned-pasteboard<%>
    aligned-pasteboard-parent<%>
    aligned-snip<%>)
  
  ;; the interface that must be implemented for a pasteboard to be contained in an aligned-pasteboard-parent<%>
  (define aligned-pasteboard<%>
    (interface (editor<%>)
      ;; temp fix
      ignore-resizing
      
      ;; get-alignment (-> (values symbol? symbol?))
      ;; get the pasteboards current alignment specification
      ;; status: possible future feature
      ;get-alignment
      
      ;; set-alignment (symbol? symbol? . -> . (void))
      ;; sets the alignement which determines how children are placed in the pasteboard
      ;; status: possible future feature
      ;set-alignment
      
      ;; spacing (case-> (number? . -> .(void)) (-> number?))
      ;; get or set the spacing in pixels placed between each child snip of the pasteboard
      ;; status: possible future feature
      ;spacing
      
      ;; realign (-> (void))
      ;; called by the parent to resize and position the pasteboard's children
      realign
   
      ;; set-aligned-min-sizes (-> void?)
      ;; calculates the minimum width and height of the of the pasteboard
      set-aligned-min-sizes
      
      ;; get-aligned-min-width: (-> number?)
      ;; the minimum width of the pasteboard determined by its children
      get-aligned-min-width
      
      ;; get-aligned-min-height: (-> number?)
      ;; the minimum width of the pasteboard determined by its children
      get-aligned-min-height
      ))
  
  ;; the interface that must be implemented by a class to be the parent of an aligned-pasteboard<%>
  (define aligned-pasteboard-parent<%>
    (interface ()
      ;; set-aligned-min-size: (-> (void))
      ;; sets the aligned min width and height of all aligned children
      set-aligned-min-sizes
      ))
  
  ;; the interface that must be implemented by a class to be inserted into an aligned-pasteboard<%> and
  ;; be stretched and shrunk according to the geometry managment. note: any snip may be insert... those
  ;; that do not implement aligned-snip<%> will simply not be stretched.
  (define aligned-snip<%>
    (interface ()
      ;; get-aligned-min-width (-> positive?)
      ;; get the minimum width of the snip
      get-aligned-min-width
      
      ;; get-aligned-min-height (-> positive?)
      ;; get the minmum height of the snip
      get-aligned-min-height
      
      ;; stretchable-width (case-> (boolean . -> . void?) (-> boolean?))
      ;; get or set the stretchablity of the pasteboards width
      stretchable-width
      
      ;; stretchable-height (case-> (boolean . -> . void?) (-> boolean?))
      ;; get or set the stretchablity of the pasteboards height
      stretchable-height
      ))
  )