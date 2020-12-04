#lang racket
(provide (rename-out [lex-1# lex]))

; lex-1#: any input-port -> (U one sharp unknown comment eof)
; where
;  one: #'#\1
;  sharp: #'#\#
;  unknown: #'(unknown Char)
;  comment: #'(comment String)
;  whitespaces: (whitespace)
;  eof: (eof)
; where all of the above contain appropriate source locations
; tokenizes the first token found in the given input port
(define (lex-1# src in)
  ;read whitespace
  (define-values (line column position) (port-next-location in))
  (define whitespaces (bytes-length (car (regexp-match #px"^\\s*" in))))
  (cond
    [(not (zero? whitespaces)) (datum->syntax #f '(whitespace)
                                              `(,src ,line ,column ,position ,whitespaces))]
    [else
     (define curr-char (read-char in))
     (cond
       [(eof-object? curr-char) (datum->syntax #f '(eof) #f)]
       [(or (char=? #\1 curr-char)
            (char=? #\# curr-char))
        (datum->syntax #f curr-char `(,src ,line ,column ,position 1))]
       [(char=? #\; curr-char)
        (define comment (if (eof-object? (peek-char in)) "" (read-line in)))
        ; if comment is at the last line, it doesn't end with a "\n" (that's what I get for using read-line)
        (define last-line? (eof-object? (peek-char in)))

        ; span computed as follows: semi-colon + comment + (if last line, 0, else 1 cus of \n)
        (datum->syntax #f `(comment ,comment) `(,src ,line ,column ,position ,(+ 1 (string-length comment) (if last-line? 0 1))))]
       [else (datum->syntax #f `(unknown ,curr-char) `(,src ,line ,column ,position 1))])]))

(module+ test
  (require rackunit)
  
  (define (syntax~? s1 s2)
    (check-equal? (syntax->datum s1) (syntax->datum s2))
    (check-eqv? (syntax-position s1) (syntax-position s2))
    (check-eqv? (syntax-span s1) (syntax-span s2)))
  
  (define test-input-port (open-input-string "
        ;startWithACommentNewLine!
1
  1 #   ;1
1 ####a #
;lastLineNoNewLine!"))
  
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(whitespace) '(#f #f #f 1 9)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(comment "startWithACommentNewLine!") '(#f #f #f 10 27)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\1 '(#f #f #f 37 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(whitespace) '(#f #f #f 38 3)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\1 '(#f #f #f 41 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(whitespace) '(#f #f #f 42 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\# '(#f #f #f 43 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(whitespace) '(#f #f #f 44 3)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(comment "1") '(#f #f #f 47 3)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\1 '(#f #f #f 50 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(whitespace) '(#f #f #f 51 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\# '(#f #f #f 52 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\# '(#f #f #f 53 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\# '(#f #f #f 54 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\# '(#f #f #f 55 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(unknown #\a) '(#f #f #f 56 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(whitespace) '(#f #f #f 57 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f #\# '(#f #f #f 58 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(whitespace) '(#f #f #f 59 1)))
  (syntax~? (lex-1# #f test-input-port) (datum->syntax #f '(comment "lastLineNoNewLine!") '(#f #f #f 60 19)))
  (check-equal? (syntax->datum (lex-1# #f test-input-port)) '(eof)))