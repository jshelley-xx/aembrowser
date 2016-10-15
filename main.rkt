#lang racket/gui

(require threading
         json
         "conf.rkt"
         "net.rkt"
         typed-stack
         )


  

(define (get-author)
  (define environments (load-conf-files "environments"))
  (hash-ref environments "local")
  (first (hash-ref (hash-ref environments "local") 'authors)))
  

(define (render-to-html data schema)
  (string-append
   "<html><head><title>results</title></head><body>"
   (hash-ref data 'sling:resourceType)
   "</body></html>")
  )


(define current-session '())
(define (start-session environment)
  (set! current-session (list environment (make-stack))))


(define (current-environment)
  (first current-session))

(define (indent indent-level s)
  (string-append (make-string (* 4 indent-level) #\ ) s))

(define (build-line key value show-comma is-wrapper)
  (define s (if (and (not is-wrapper) (symbol? key) (string? value)) "\"" ""))
  (define comma (if show-comma "," ""))
  ;(printf "~a ~a ~a\n" key value (string? key))
  (if (symbol? key)
      (format "\"~a\" : ~a~a~a~a" (symbol->string key) s value s comma)
      (format "~a" value)
      ))



(define (hydrate-list the-list payload name indent-level is-last)

  (define open (list (indent indent-level (build-line name (if (hash? payload) "{" "[") #f #t))))
  (define close (list (indent indent-level (build-line null (if (hash? payload) "}" "]") (not is-last) #t))))

  (define response open)
  
  (for ([k (in-naturals)]
        [key (sort (hash-keys payload) symbol<?)])
    (define value (hash-ref payload key))

    (cond
      [(hash? value)
       (set! response
             (append
              response
              (hydrate-list the-list value key (add1 indent-level) (< (add1 k) (hash-count payload)))))]
      [else
       (set! response
             (append
              response
              (list
               (indent (add1 indent-level)
                       (build-line key value (< (add1 k) (hash-count payload)) #f)))))

       ]))

  
  (set! response (append response close))
  
  (map (lambda (s) (if (> (string-length s) 200) (substring s 0 200) s)) response)

  
  )
                     
    
        
  





(define (populate-panels server panel results path)

  (define result-panel
    (new vertical-panel% [parent panel]
         [alignment '(left top)]))

  (define info-bar
    (new vertical-panel%
         [parent result-panel]
         [alignment '(left top)]
         [min-height 10]
         [stretchable-height #f]
         ))


  (define start-at
    (new text-field%
         [parent info-bar]
         [label "Url: "]
         [init-value
          (string-append "http://" (hash-ref server 'host) ":" (number->string (hash-ref server 'port)) path)]
         [min-width 600]
         [stretchable-width #f]))
  
  (define new-list
    (new list-box%
         [parent result-panel]
         [choices (list "Loading...")]
         [label #f]
         [style '(multiple vertical-label)]))
  
  (thread
   (lambda ()
     ;(printf "Loading: ~a\n" (string-append "http://" (hash-ref server 'host) ":" (number->string (hash-ref server 'port))))
     (define-values
       (status headers payload)
       (fetch-json server (string-append path ".infinity.json")))
     (hash-set! results server (list status headers payload))
     (if (empty? payload)
           (send new-list set (list (third status)))
           (let ([list-contents (hydrate-list new-list payload #f 0 #t)])
             (println list-contents)
           (send new-list set list-contents))))))




(define (make-stack-frame path authors-panel publishers-panel)
  (define stack-frame (make-hash))
  (hash-set! stack-frame "path" path)
  (hash-set! stack-frame "authors-panel" authors-panel)
  (hash-set! stack-frame "publishers-panel" publishers-panel)
  (hash-set! stack-frame "authors-results" (make-hash))
  (hash-set! stack-frame "publishers-results" (make-hash))
  stack-frame)


(define (clear-panel panel)
  (map
   (lambda (child) 
     (send panel delete-child child))
   (send panel get-children)))
   



(define (push-path path authors-panel publishers-panel)

  (define stack-frame (make-stack-frame path authors-panel publishers-panel))
  (push! (second current-session) stack-frame)

  (map clear-panel (list authors-panel publishers-panel))

  (let*
      ([author-servers (hash-ref (current-environment) 'authors)]
       [publish-servers (hash-ref (current-environment) 'publishers)])
    
       (for ([server (in-list author-servers)])
         (populate-panels server authors-panel (hash-ref stack-frame "authors-results") path))
    
       (for ([server (in-list publish-servers)])
         (populate-panels server publishers-panel (hash-ref stack-frame "publishers-results") path)))
  1)


  


(define (test)
  (define-values (status headers payload)
    (fetch-json (get-author) "/content/geometrixx/en/products/triangle/jcr:content.infinity.json"))

  ;(define current-response (open-url (open-input-string (render-to-html payload))))
 (render-to-html payload 1)
  )




(define (go)
  (define environments (load-conf-files "environments"))
  
  (define frame
    (new frame%
         [label "AEM Browser"]))
  

  (define top-panel
    (new horizontal-panel%
         [parent frame]))
  
  (define bottom-panel
    (new horizontal-panel%
         [parent frame]
         [min-height 800]))
  

  (define environment-group-box
    (new group-box-panel%
         [parent top-panel]
         [label "Environment"]))



  (define env-panel
    (new vertical-panel%
         [parent environment-group-box]
         [alignment '(left top)]))




  (define environment-dropdown
    (new choice%
         [parent env-panel]
         [label "Environment: "]
         [choices (hash-keys environments)]))
  
  
  (define start-at-panel
    (new horizontal-panel%
         [parent env-panel]))

  (define notes-panel
    (new vertical-panel% [parent top-panel]
         [alignment '(left top)]))
  

  
  (define authors-panel
    (new vertical-panel%
         [parent bottom-panel]))

  (define publishers-panel
    (new vertical-panel%
         [parent bottom-panel]))
  
  
  
  
  (define start-at
    (new text-field%
         [parent start-at-panel]
         [label "Start at: "]
         [init-value "/content/geometrixx/en/products/triangle/jcr:content"]
         [min-width 800]
         [stretchable-width #f]))
  
  (new button% [parent start-at-panel]
       [label "Start Session"]
       [callback
        (lambda (button event)
          (let*
              ([env-name (send environment-dropdown get-string-selection)]
               [new-env (hash-ref environments env-name)]
               [path (send start-at get-value)])
            (start-session new-env)
            (push-path path authors-panel publishers-panel)
            
            ))])
  

  
  (new text-field%
       [parent notes-panel]
       [label "Notes: "]
       [style '(multiple vertical-label)])
  
  

  


  

  
  
  
  
  (send frame show #t)
  (send frame maximize #t)
  frame
  )