(module alignment-helpers mzscheme
  
  (require
   (lib "list.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   
   "interface.ss"
   "snip-lib.ss")
  
  (provide vacuous-max
           child-height
           child-width)
  
  (define (vacuous-max . n)
    (if (empty? n)
        0
        (apply max n)))
  
  (define (child-height item)
    (cond
      [(is-a? item snip%) (snip-min-height item)]
      [(is-a? item alignment<%>) (send item get-min-height)]))
  
  (define (child-width item)
    (cond
      [(is-a? item snip%) (snip-min-width item)]
      [(is-a? item alignment<%>) (send item get-min-width)]))
  )