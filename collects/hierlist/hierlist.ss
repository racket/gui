
(require-library "hierlists.ss" "hierlist")

(invoke-open-unit/sig (require-library "hierlistr.ss" "hierlist") mred (mred : mred^) (wx : wx^))

#|

;; Testing
(define f (make-object mred:frame% null "test"))
(define p (make-object mred:horizontal-panel% f))
(define c (make-object (class-asi mred:hierarchical-list%
				  (public
				   [item-opened
				    (lambda (i)
				      (let ([f (send i user-data)])
					(when f (f i))))]
				   [select
				    (lambda (i)
				      (printf "Selected: ~a~n"
					      (if i 
						  (send (send i get-buffer) get-flattened-text)
						  i)))]
				   [double-select
				    (lambda (s)
				      (printf "Double-click: ~a~n"
					      (send (send s get-buffer) get-flattened-text)))]))
		       p))

(define a (send c new-list))
(send (send a get-buffer) insert "First Item: List")
(send (send (send a new-item) get-buffer) insert "Sub1")
(send (send (send a new-item) get-buffer) insert "Sub2")
(define a.1 (send a new-list))
(send (send a.1 get-buffer) insert "Deeper List")
(send (send (send a.1 new-item) get-buffer) insert "Way Down")

(define b (send c new-item))
(send (send b get-buffer) insert "Second Item")

(define d (send c new-list))
(send (send d get-buffer) insert "dynamic")
(send d user-data (lambda (d)
		    (time (let loop ([i 30])
			    (unless (zero? i)
				    (send (send (send d new-item) get-buffer) insert (number->string i))
				    (loop (sub1 i)))))))

(define x (send c new-list))
(send (send x get-buffer) insert "x")

(define y (send c new-item))
(send (send y get-buffer) insert "y")

(send f show #t)

|#
