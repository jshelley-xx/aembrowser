#lang racket/gui

(require threading
         json
         "conf.rkt"
         "net.rkt"
         "session.rkt"
         "formatting.rkt"
         typed-stack
         )


  





(define (hydrate-list the-list payload name indent-level is-last lens)

  (define open (list (list (list name #f) (indent indent-level (build-line name (if (hash? payload) "{" "[") #f #t)))))
  (define close (list (list (list name #f) (indent indent-level (build-line null (if (hash? payload) "}" "]") (not is-last) #t)))))

  (define used-payload (make-hash))
  (for ([key (hash-keys payload)])
    (when (send lens include-field key (hash-ref payload key))
        (hash-set! used-payload key (hash-ref payload key))))
    

  
  (define response open)


  (for ([k (in-naturals)]
        [key (sort (hash-keys used-payload) symbol<?)])
    (define value (hash-ref used-payload key))

    

    (cond
      [(hash? value)
       (set! response
             (append
              response
               (hydrate-list the-list value key (add1 indent-level) (= (add1 k) (hash-count used-payload)) lens)))
       ]
      [else
       (set! response
             (append
              response
              (list
              (cons
                (list key value)
                (list
                 (let ([tmp (indent (add1 indent-level) (build-line key value (< (add1 k) (hash-count used-payload)) #f))])
                   (if (> (string-length tmp) 200) (substring tmp 0 200) tmp))
                 )))))
       ]))

  
  (set! response (append response close))
  
  response
  )
                     
    
        
  





(define (populate-panels server path panel results path-stack)

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


  (define list-context-menu
    (new popup-menu%	 
         [title "Options"]))
  


  
  
  (define new-list
    (new list-box%
         [parent result-panel]
         [choices (list "Loading...")]
         [label #f]
         [style '(single)]
         [callback
          (lambda (cmp evt)
            (let-values ([(mouse-coords modifier-state) (get-current-mouse-state)])
              (send
               (send cmp get-top-level-window)
               popup-menu
               list-context-menu
               (send mouse-coords get-x)
               (- (send mouse-coords get-y) (send cmp get-y)))))]))
  
  


    (define push-path-option
      (new menu-item%
         [parent list-context-menu]
         [label "Go here!"]
         [callback
          (lambda (cmp evt)
            (let*
                ([selected-index (send new-list get-selection)]
                 [next-path (second (send new-list get-data selected-index))]
                 [bottom-panel (send (send panel get-parent) get-parent)])
              
              (let-values
                  ([(authors-panel publishers-panel) (new-results-panel bottom-panel next-path)])
                (show-results-for-path bottom-panel next-path)
                (push-path next-path authors-panel publishers-panel path-stack))))
            

            ]
         ))

  (define copy-submenu
      (new menu%
         [parent list-context-menu]
         [label "Copy"]
         ))


      (define copy-all-contents-option
      (new menu-item%
         [parent copy-submenu]
         [label "Copy All"]
         [callback
          (lambda (cmp evt)
            (let
                ([list-values 
                  (for/list ([k (in-range 0 (send new-list get-number))])
                    (send new-list get-string k)
                    )])
              (send the-clipboard set-clipboard-string (string-join list-values "\r\n") 0)
            ))]
         ))

      (define copy-name-option
      (new menu-item%
         [parent copy-submenu]
         [label "Copy Name"]
         [callback
          (lambda (cmp evt)
              (send the-clipboard set-clipboard-string (symbol->string (first (send new-list get-data (send new-list get-selection)))) 0)
            )]
         ))

      (define copy-value-option
      (new menu-item%
         [parent copy-submenu]
         [label "Copy Value"]
         [callback
          (lambda (cmp evt)
              (send the-clipboard set-clipboard-string (second (send new-list get-data (send new-list get-selection))) 0)
            )]
         ))



  
  
  (thread
   (lambda ()
     (define-values
       (status headers payload)
       (fetch-json server (string-append path ".infinity.json")))
     (hash-set! results server (list status headers payload))
     (if (empty? payload)
           (send new-list set (list (third status)))
           (let ()
             (send new-list clear)
             (for ([row (hydrate-list new-list payload #f 0 #t (get-current-lens))])
               (send new-list append (second row) (first row))))))))









(define (clear-panel panel)
  (map
   (lambda (child) 
     (send panel delete-child child))
   (send panel get-children)))
   



(define (push-path path authors-panel publishers-panel path-stack)

  (define stack-frame (make-stack-frame path authors-panel publishers-panel))

  (push-stack-frame stack-frame)

  
  (unless 
   (for/or ([k (in-range 0 (send path-stack get-number))]
            #:when (equal? (send path-stack get-string k) path))
     (send path-stack set-selection k)
     #t)
    (let ()
      (if (and (= (send path-stack get-number) 1) (equal? (send path-stack get-string 0) "Start Session to build path stack..."))
         (send path-stack set (list path))
         (send path-stack append path))

      (send path-stack set-selection (sub1 (send path-stack get-number)))
     
     
      (map clear-panel (list authors-panel publishers-panel))
     
      (let*
          ([author-servers (send (current-environment) get-authors)]
           [publish-servers (send (current-environment) get-publishers)])
        
        (for ([server (in-list author-servers)])
          (populate-panels server path authors-panel (hash-ref stack-frame "authors-results") path-stack))
        
        (for ([server (in-list publish-servers)])
          (populate-panels server path publishers-panel (hash-ref stack-frame "publishers-results") path-stack)))))
  1)


  



(define results-panel-registry (make-hash))
 
(define (show-results-for-path bottom-panel path)
  (send bottom-panel change-children (lambda (children) (list)))
  (for ([(k v) results-panel-registry]
    #:when (equal? v path))
    (send bottom-panel add-child k)))

  


  
(define (new-results-panel bottom-panel path)
  (define maybe-exists
     (for/or ([(k v) results-panel-registry]
              #:when (equal? v path)) k))
  
  (if maybe-exists
      (values (first (send maybe-exists get-children)) (second (send maybe-exists get-children)))
      (let ()
        (define bottom-results-panel
          (new horizontal-panel%
               [parent bottom-panel]))
        
        (define authors-panel
          (new vertical-panel%
               [parent bottom-results-panel]))
        
        (define publishers-panel
          (new vertical-panel%
               [parent bottom-results-panel]))
        
        (hash-set! results-panel-registry bottom-results-panel path)
        (values authors-panel publishers-panel))))


(define (select-default-environment environments)
  (unless
      (for/or ([(k v) environments]
             #:when (send v get-use-by-default))
        v)
    (for/or ([(k v) environments])
      v)
    ))



(define (new-session environment go-box path-stack bottom-panel)
  (start-session environment)
  (send go-box set-value (send environment get-initial-path))
  (send path-stack clear)
  (clear-panel bottom-panel)

  )


(define (go)
  (define environments (load-environments))
  

  
  (define lenses (load-lenses))
  
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

  (define environment-row
    (new horizontal-panel%
         [parent env-panel]))

  



  (define lens-dropdown
    (new choice%
         [parent environment-row]
         [label "Lens: "]
         [choices (hash-keys lenses)]))
  
  
  (define start-at-panel
    (new horizontal-panel%
         [parent env-panel]))

  (define path-stack-panel
    (new vertical-panel%
         [parent top-panel]))
  
  (define path-stack
    (new list-box%
         [label "Path Stack"]
         [style '(single vertical-label)]
         [min-width 400]
         [choices (list "Start Session to build path stack...")]	 
         [parent path-stack-panel]
         [callback
          (lambda (cmp event)
              (show-results-for-path bottom-panel (send path-stack get-string-selection))
            )]))


  
  (define notes-panel
    (new vertical-panel% [parent top-panel]
         [alignment '(left top)]))
  

  
  
  
  
  
  (define start-at
    (new text-field%
         [parent start-at-panel]
         [label "Start at: "]
         [init-value ""]
         [min-width 600]
         [stretchable-width #f]))

  (define environment-dropdown
    (new choice%
         [parent environment-row]
         [label "Environment: "]
         [choices (hash-keys environments)]
         [callback
          (lambda (cmp event)
            (let*
              ([env-name (send environment-dropdown get-string-selection)]
               [new-env (hash-ref environments env-name)])
               (new-session new-env start-at path-stack bottom-panel))
              
              
            )]
         ))

  
  (new-session (select-default-environment environments) start-at path-stack bottom-panel)
  
  (new button% [parent start-at-panel]
       [label "Navigate to Path"]
       [callback
        (lambda (button event)
          (let*
              ([lens-name (send lens-dropdown get-string-selection)]
               [lens (hash-ref lenses lens-name)]
               [path (send start-at get-value)])
          (set-current-lens lens)
          (let-values
              ([(authors-panel publishers-panel) (new-results-panel bottom-panel path)])
            (show-results-for-path bottom-panel path)
            (push-path path authors-panel publishers-panel path-stack))
          
          ))])
  



  
  (new text-field%
       [parent notes-panel]
       [label "Notes: "]
       [min-width 400]
       [style '(multiple vertical-label)])
  
  

  


  

  
  
  
  
  (send frame show #t)
  (send frame maximize #t)
  frame
  )