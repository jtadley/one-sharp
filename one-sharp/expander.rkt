#lang racket
(require (for-syntax syntax/parse))
(provide (rename-out [one-sharp-mod-beg #%module-begin])
         #%top-interaction)

(define-syntax (one-sharp-mod-beg stx)
  (syntax-parse stx
    [(_ parsed-program) #`(#%module-begin
                           'parsed-program)]))