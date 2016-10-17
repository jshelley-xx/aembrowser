#lang racket

(require json)

(provide environment%)

(define environment%
  (class object%
    (init json)                

    (define name (hash-ref json 'name))
    (define use-by-default (hash-ref json 'use-by-default #f))
    (define initial-path (hash-ref json 'initial-path))
    (define authors (hash-ref json 'authors))
    
    (define publishers (hash-ref json 'publishers))
    
    (super-new)                
 
 
    (define/public (get-name) name)
    (define/public (get-initial-path) initial-path)
    (define/public (get-authors) authors)
    (define/public (get-use-by-default) use-by-default)
    (define/public (get-publishers) publishers)


    
  ))