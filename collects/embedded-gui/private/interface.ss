(module interface mzscheme
  
  (require (lib "class.ss"))
  
  (provide stretchable-snip<%>
           alignment<%>)
  
  (define alignment<%>
    (interface ()
      set-min-sizes
      align
      get-min-width
      get-min-height
      stretchable-width?
      stretchable-height?
      show))
  
  #| the interface that must be implemented by a class to be inserted into an
     aligned-pasteboard<%> and be stretched and shrunk according to the geometry managment.
  
     note: any snip may be insert... those
     that do not implement stretchable-snip<%> will simply not be stretched.
  |#
  (define stretchable-snip<%>
    (interface ()
      ;; (positive? positive? . ->  . void?)
      ;; called by the parent editor to stretch the snip to an specific size
      stretch
      
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