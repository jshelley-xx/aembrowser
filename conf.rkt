#lang racket

(require json "lens/main.rkt")

(provide load-conf-files load-environments load-lenses)

(define (create-if-needed path)
  (unless (directory-exists? path)
    (make-directory path))
  path)
  


(define (get-app-folder path name)
  (create-if-needed
   (build-path path name)))

(define (get-home-folder)
  (create-if-needed
   (build-path
    (find-system-path 'home-dir)
    ".aembrowser")))
  
(define (get-search-bases)
  (list (current-directory) (get-home-folder)))


;finds all json files under all search paths, with latter paths trumping earlier paths 
(define (find-conf-files category)
  (define search-paths (map (lambda (p) (get-app-folder (get-app-folder p "conf") category)) (get-search-bases)))
  (define all-files (make-hash))
  (for ([d (in-list search-paths)])
    ;(printf "loading directory ~a\n" d)
    (for ([f (in-directory d)])
      ;(printf "\tadding: ~a\n" (path->complete-path f))
      (when (path-has-extension? f ".json")
        (hash-set! all-files (file-name-from-path f) (path->string (path->complete-path f))))

      ))
    (hash-values all-files))


(define (load-conf-files [type "environments"])
  (define response (make-hash))
  (for ([f (find-conf-files type)])
   (call-with-input-file f
     (lambda (in)
       (hash-set!
        response
        (path->string (path-replace-extension (file-name-from-path f) #"")) (read-json in)))))
  response)




(define (load-environments)
  (load-conf-files "environments"))




(define (load-lenses)
  (define lense-files (load-conf-files "lenses"))
  (define lenses (make-hash))
  (for ([(k v) lense-files])
    (hash-set! lenses k (new lens% [json v])))
  lenses
  )