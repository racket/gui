
(define-macro defmacro
  (lambda (name args . body)
    `(define-macro ,name (lambda ,args ,@body))))

(define-macro letmacro
  (lambda (name args mbody . body)
    `(let-macro ,name (lambda ,args ,mbody)
		,body)))
