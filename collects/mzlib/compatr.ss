(unit/sig
   mzlib:compat^
   (import mzlib:function^)
  
   (define 1+ add1)
   (define 1- sub1)
   (define #%1+ #%add1)
   (define #%1- #%sub1)
   
   (define =? =)
   (define <? <)
   (define >? >)
   (define <=? <)
   (define >=? >)

   (define atom? (lambda (v) (not (pair? v))))

   (define gentemp gensym)

   (define sort ; Chez argument order
     (lambda (less-than? l)
       (quicksort l less-than?)))

   (define bound? defined?)

   (define flush-output-port flush-output)

   (define real-time current-milliseconds)

   (define getprop (void))
   (define putprop (void))
   (let ([table (make-hash-table)])
     (letrec ([gp
	       (case-lambda
		[(k prop) (gp k prop #f)]
		[(k prop def)
		 (let ([al (hash-table-get table k (lambda () #f))])
		   (if al
		       (let ([v (assq prop al)])
			 (if v
			     (cdr v)
			     def))
		       def))])]
	      [pp
	       (lambda (k prop nv)
		 (let ([al (hash-table-get table k (lambda () '()))])
		   (let ([v (assq prop al)])
		     (if v
			 (set-cdr! v nv)
			 (hash-table-put! table k (cons (cons prop nv) al))))))])
       (set! getprop gp)
       (set! putprop pp)))

   ; Chez's new-cafe
   (define new-cafe 
     (letrec ([nc
	       (case-lambda
		[() (nc (current-eval))]
		[(eval)
		 (let/ec escape
			(let ([orig-exit (exit-handler)]
			      [orig-eval (current-eval)])
			  (dynamic-wind
			   (lambda ()
			     (current-eval eval)
			     (exit-handler
			      (lambda (v) (escape v))))
			   read-eval-print-loop
			   (lambda ()
			     (current-eval orig-eval)
			     (exit-handler orig-exit)))))])])
       nc))
   )
