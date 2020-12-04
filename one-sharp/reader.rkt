#lang racket
(require one-sharp/parser)
(provide (rename-out [parse-1#-exp read]
                     [parse-1# read-syntax]))

(define (parse-1# src in)
  #`(module one-sharp one-sharp/expander
                       #,(parse src in)))

(define (parse-1#-exp in)
  (syntax->datum (parse-1# #f in)))