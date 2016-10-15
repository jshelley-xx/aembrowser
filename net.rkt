#lang racket

(provide fetch-json)

(require net/http-client
         net/head
         json
         net/base64)

(define (make-auth-header u p)
  (string-append "Authorization: Basic " (string-trim (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 (string-append  u ":" p))))) ))


;returns three values: status, headers and the input pipe
(define (fetch-json server path)
  (with-handlers ([
                   (lambda (v) #t)
                   (lambda (exn) (values (list "fail" "-1" "Unable to contact server!" exn) null null))])
  (define-values (status headers in) 
    (http-sendrecv
     (hash-ref server 'host)
     path
     #:port (hash-ref server 'port)
     #:headers (list
                "Content-Type: application/json"
                "Accept: application/json"
                (make-auth-header
                 (hash-ref server 'user )
                 (hash-ref server 'password)))))
  (values (string-split (bytes->string/utf-8 status) " ") headers (read-json in))))
