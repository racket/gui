(unit/sig framework:keys^
  (import)
  
  (define shifted-key-list
    '("?" ":" "~" "\"" "|"
      "<" ">" "{" "}" "[" "]" "(" ")"
      "!" "@" "#" "$" "%" "^" "&" "*" "_" "+"))
  (define (get-shifted-key-list) shifted-key-list))