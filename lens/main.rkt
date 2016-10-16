#lang racket

(require json)

(provide lens%)

(define lens%
  (class object%
  (init json)                

    (define auto-add-jcrcontent-exprs (hash-ref json 'auto-add-jcrcontent))
    (define suppress-fields (hash-ref json 'suppress-fields))
 
  (super-new)                
 
 
  (define/public (include-field field value)
    (not (member (symbol->string field) suppress-fields)))
  ))
