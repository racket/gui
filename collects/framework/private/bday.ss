(module bday mzscheme
  (provide mrf-bday?
           mf-bday?
           sk-bday?)
  
  ;; mf-bday? : -> boolean
  ;; Matthias's birthday
  (define (mf-bday?)
    (let ([date (seconds->date (current-seconds))])
      (and (= (date-month date) 10)
           (= (date-day date) 29))))
  
  ;; mrf-bday? : -> boolean
  ;; Matthew's birthday
  (define (mrf-bday?)
    (let ([d (seconds->date (current-seconds))])
       (and (= (date-month d) 11)
            (= (date-day d) 1))))
  
  ;; sk-bday? : -> boolean
  ;; Shriram's birthday
  (define (sk-bday?)
    (let ([date (seconds->date (current-seconds))])
      (and (= (date-month date) 4)
           (= (date-day date) 8)))))
