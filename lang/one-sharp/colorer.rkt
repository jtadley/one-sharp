#lang racket
(require one-sharp/lexer syntax/parse)
(provide (rename-out [1#-colorer colorer]))

(define color1 'string)
(define color2 'keyword)

; colorer: input-port number (U boolean `((instr number number) . boolean)) -> colorer-vals
; lookup 'start-colorer' in Racket docs
; Colors instructions alternatively (first color1, then color2).
; 1's are "okay", #'s could be colored red if there are more than 5 of em.
; Any text other than 1# is colored red
; Any text prefixed by a ';' is colored 'comment
(define 1#-colorer
  (λ (in offset mode)
    (define tok (lex #f in))
    (define pos (syntax-position tok))
    (define end-pos (if pos (+ 0 pos (syntax-span tok)) 0))
    (syntax-parse tok
      #:datum-literals(comment eof unknown whitespace)
      [(eof) (values eof 'eof #f #f #f 0 mode)]
      [(comment str) (values (syntax->datum #'str) 'comment #f pos end-pos 0 mode)]
      [(whitespace) (values 'whitespace 'no-color #f pos end-pos 0 mode)]
      [(unknown a) (values (string (syntax->datum #'a)) 'error #f pos end-pos 0 mode)]
      [_ (match mode
           [`((instr ,ones ,sharps) . ,bool); we have atleast seen one '1'
            (syntax-parse tok
              [#\1 (values "1" (if bool color2 color1) #f pos end-pos 0
                           `((instr ,(if (zero? sharps) (add1 ones) 1) 0) . ,bool))]
              [#\#
               (define new-bool (if (= sharps 0) (not bool) bool))
               (define col (if new-bool color1 color2))
               (values "#" col #f pos end-pos 0
                       (if (= sharps 4)
                           new-bool
                           `((instr ,ones ,(add1 sharps)) . ,new-bool)))])]
           [bool; we expect to see a '1'
            (syntax-parse tok
              [#\1 (values "1" (if bool color2 color1) #f pos end-pos 0 `((instr 1 0) . ,bool))]
              [#\# (values "#" 'error #f pos end-pos 0 mode)])])])))
(module+ test
  (require rackunit)
  (define test-color-port (open-input-string "
        ;startWithACommentNewLine!
1
  1 #   ;1
1 ####a #
;lastLineNoNewLine!"))
  (define (check-color? curr-mode exp-token exp-color exp-paren exp-pos exp-pos-end exp-mode)
    (define-values (act-token act-color act-paren act-pos act-pos-end _ act-mode)
      (1#-colorer test-color-port 0 curr-mode))
    (check-equal? act-token exp-token)
    (check-eqv? act-color exp-color)
    (check-eqv? act-paren exp-paren)
    (check-eqv? act-pos exp-pos)
    (check-eqv? act-pos-end exp-pos-end)
    (check-equal? act-mode exp-mode))
  
  (check-color? #f 'whitespace 'no-color #f 1 10 #f)
  (check-color? #f "startWithACommentNewLine!" 'comment #f 10 37 #f)
  (check-color? #f "1" color1 #f 37 38 `((instr 1 0) . #f))
  (check-color? `((instr 1 0) . #f) 'whitespace 'no-color #f 38 41 `((instr 1 0) . #f))
  (check-color? `((instr 1 0) . #f) "1" color1 #f 41 42 `((instr 2 0) . #f))
  (check-color? `((instr 2 0) . #f) 'whitespace 'no-color #f 42 43 `((instr 2 0) . #f))
  (check-color? `((instr 2 0) . #f) "#" color1 #f 43 44 `((instr 2 1) . #t))
  (check-color? `((instr 2 1) . #t) 'whitespace 'no-color #f 44 47 `((instr 2 1) . #t))
  (check-color? `((instr 2 1) . #t) "1" 'comment #f 47 50 `((instr 2 1) . #t))
  (check-color? `((instr 2 1) . #t) "1" color2 #f 50 51 `((instr 1 0) . #t))
  (check-color? `((instr 1 0) . #t) 'whitespace 'no-color #f 51 52 `((instr 1 0) . #t))
  (check-color? `((instr 1 0) . #t) "#" color2 #f 52 53 `((instr 1 1) . #f))
  (check-color? `((instr 1 1) . #f) "#" color2 #f 53 54 `((instr 1 2) . #f))
  (check-color? `((instr 1 2) . #f) "#" color2 #f 54 55 `((instr 1 3) . #f))
  (check-color? `((instr 1 3) . #f) "#" color2 #f 55 56 `((instr 1 4) . #f))
  (check-color? `((instr 1 4) . #f) "a" 'error #f 56 57 `((instr 1 4) . #f))
  (check-color? `((instr 1 4) . #f) 'whitespace 'no-color #f 57 58 `((instr 1 4) . #f))
  (check-color? `((instr 1 4) . #f) "#" color2 #f 58 59 #f)
  (check-color? #f 'whitespace 'no-color #f 59 60 #f)
  (check-color? #f "lastLineNoNewLine!" 'comment #f 60 79 #f)
  (check-color? #f eof 'eof #f #f #f #f))