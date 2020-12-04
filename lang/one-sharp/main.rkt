#lang racket
(module reader racket
  (require one-sharp/reader)
  (provide read-syntax read get-info)

  ;################
  ; DrRacket util
  ;################

  (define (get-info in mod line col pos)
    (lambda (key default)
      (case key
        [(color-lexer)
         ;color:start-colorer:get-token
         (dynamic-require 'one-sharp/colorer 'colorer)]
        [(drracket:opt-out-toolbar-buttons)
         ; opt out of all usual DrRacket tool bar buttons
         '(drracket:syncheck debug-tool macro-stepper)]
        [else default]))))