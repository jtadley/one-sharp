#lang racket
(module reader racket
  (require one-sharp/reader one-sharp/unparser)
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
        [(drracket:keystrokes)
         ; #lang's keystroke for formatting text when C-i is pressed
         ; if parser throws an exception, the same text should be returned
         `(("c:%" ,(λ (text% event%)
                     (define lang-lit "#lang")
                     (define one-sharp-lit "one-sharp")
                     (define lang-code (send text% get-text))
                     (define current-pos (send text% get-start-position))
                     ; formatting the text
                     ; need to remove #lang line, so let's remove that "properly"
                     (define removed-lang
                       (substring (string-trim (substring (string-trim lang-code)
                                                          (string-length lang-lit)))
                                  (string-length one-sharp-lit)))
                     (define formatted-code (format removed-lang))
                     (define final-code
                       (if (string=? "" lang-code)
                           ""
                           (string-append lang-lit " " one-sharp-lit formatted-code)))
                     ; now we edit the text% object
                     (send text% begin-edit-sequence)
                     (send text% erase)
                     (send text% insert final-code 0 'same #f)
                     (send text% set-position current-pos 'same #f #t 'default)
                     (send text% end-edit-sequence))))]
        [else default]))))