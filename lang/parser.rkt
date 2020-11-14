#lang racket
;; Based off of https://github.com/dyoo/brainfudge and Racket docs
;; This module parses 1# and binds `ast` to 1# AST

(require syntax/readerr syntax/parse)

(provide (rename-out [parse-1sharp-exp read]
                     [parse-1sharp read-syntax])
         get-info)

;; parse-1sharp: any input-port -> syntax
;; reader called to parse 1# programs (1#'s Reader, i.e, read-syntax)
(define (parse-1sharp src in)
  (with-syntax ([AST (parse-1sharp-init src in)])
    #'(module huh racket
        (provide ast)
        (define ast 'AST))))

; lex-1#: any input-port -> (U one sharp unkown comment eof)
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

#;(define (parse-1#-internal src in)
    (define tokens
      (let lex ()
        (define tok (lex-1# src in))
        (if (eof-object? tok) empty (cons tok (lex)))))
    (define 1? (syntax-parser [#\1 #t] [_ #f]))
    (define sharp? (syntax-parser [#\# #t] [_ #f]))
    (define comment? (compose string? syntax->datum))
    (define unknown? (syntax-parser [(unknown _) #t] [_ #f]))
    (define (parse tokens)
      (cond
        [(empty? tokens) empty]
        [(empty? tokens) empty])
      (if (empty? tokens)
          empty
          (let )))
    (define (get-ones tokens)
      (match tokens
        [()]))
    (define (till-next-instr tokens) 2)
    tokens)

;; parse-1sharp-init: any input-port -> [Listof (U `(I ,number ,number ,source) string)]
;; initial call to the parser to start the main parsing
(define (parse-1sharp-init src in)
  ; read whitespace
  (regexp-match #px"^\\s*" in)
  (define-values (line column position) (port-next-location in))
  (define curr-char (peek-char in))
  (cond
    [(char=? #\; curr-char) (read-char in) (cons (read-line in) (parse-1sharp-init src in))]
    [(char=? #\# curr-char) (raise-read-error (string-append "A 1sharp instruction can not start with a #")
                                              src line column position 1)]
    [else (parse-1sharp-internal src in)]))

;; parse-1sharp-internal: any input-port -> [Listof (U `(I ,number ,number ,source) string)]
;; main parsing funciton. Once a '1' is found, calls parse-1s
(define (parse-1sharp-internal src in)
  ; read whitespace
  (regexp-match #px"^\\s*" in)
  (define-values (line column position) (port-next-location in))
  (define curr-char (peek-char in))
  (cond
    [(eof-object? curr-char) empty] ; this is the only point eof
    [(char=? #\; curr-char) (read-char in) (cons (read-line in) (parse-1sharp-internal src in))]
    [(char=? #\1 curr-char) (parse-1s src in line column position)]
    [else ; only place we take care of unknown characters
     (raise-read-error (string-append "A 1sharp instruction can not contain a "
                                      (list->string (list #\' curr-char #\')))
                       src line column position 1)]))
 
;; parse-1s: any input-port number number number -> [Listof (U `(I ,number ,number ,source) string)]
;; parser that expects a '1' at the beginning.
;; Collects all '1's until an unknown character is found, then calls parse-#s
(define (parse-1s src in ln col pos)
  (match-let ([`(,1s ,comments ,span)
               (let 1s ([n 0]
                        [comments d-list-empty]
                        [span 0])
                 (define white-span (bytes-length (car (regexp-match #px"^\\s*" in))))
                 (define curr-char (peek-char in))
                 (cond
                   [(and (char? curr-char) (char=? #\; curr-char))
                    (read-char in)
                    (define comment (read-line in))
                    (1s n
                        (d-list-snoc comment comments)
                        (+ span white-span 2 (string-length comment)))]
                   [(and (char? curr-char) (char=? #\1 curr-char))
                    (read-char in)
                    (1s (add1 n) comments (+ span white-span 1))]
                   [else `(,n ,comments ,span)]))])
    (comments (parse-#s src in ln col pos 1s span))))

;; parse-#s: any input-port number number number number number -> [Listof (U `(I ,number ,number ,source) string)]
;; parser that expects any non-whitespace character other than '1' at the beginning.
;; Collects all '#'s until an unknown character is found, then calls main parser.
(define (parse-#s src in ln col pos ones span)
  (match-let ([`(,sharps ,comments-pre-instr ,comments-post-instr ,span ,post-span)
               (let sharps ([n 0]
                            [comments-pre-instr d-list-empty]
                            [comments-post-instr d-list-empty]
                            [span span]
                            [post-span 0])
                 (define white-span (bytes-length (car (regexp-match #px"^\\s*" in))))
                 (define curr-char (peek-char in))
                 (cond
                   [(and (char? curr-char) (char=? #\; curr-char))
                    (read-char in)
                    (define line (read-line in))
                    (define comment (if (eof-object? line) "" line))
                    (define char-span (+ white-span 2 (string-length comment)))
                    (sharps n
                            comments-pre-instr
                            (d-list-snoc comment comments-post-instr)
                            span
                            (+ post-span char-span))]
                   [(and (char? curr-char) (char=? #\# curr-char))
                    (read-char in)
                    (sharps (add1 n)
                            (d-list-append comments-pre-instr comments-post-instr)
                            d-list-empty
                            (+ post-span span white-span 1)
                            0)]
                   [else `(,n ,comments-pre-instr ,comments-post-instr ,span ,post-span)]))])
    (if (<= 1 sharps 5)
        (comments-pre-instr (cons `(I ,ones ,sharps ,(source ln col pos span))
                                  (comments-post-instr (parse-1sharp-internal src in))))
        (raise-read-error (string-append "A 1sharp instruction '#' count should be in range [1,5], found "
                                         (number->string sharps) " '#'s.")
                          src ln col pos span))))

(define (parse-1sharp-exp in)
  (syntax->datum (parse-1sharp #f in)))

;; d-list util
;; Allows 'n' snocs to be in O(n)
(define d-list-empty (λ (ls) ls))
(define (d-list-snoc x xs) (λ (ls) (xs (cons x ls))))
(define (d-list-append xs ys) (λ (ls) (xs (ys ls))))

;; source struct (not using Racket struct cus then we get 3D syntax
(define (source ln col pos span) `(source ,ln ,col ,pos ,span))

;################
; DrRacket util
;################

; colorer: input-port number (U boolean `((instr number number) . boolean)) -> colorer-vals
; check color:start-colorer:get-token
; Colors instructions alternatively (first 'keyword, then 'string).
; 1's are "okay", #'s could be colored red if there are more than 5 of em.
; Any text other than 1# is colored red
; Any text prefixed by a ';' is colored 'comment
(define colorer
  (λ (in offset mode)
    (define tok (lex-1# #f in))
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
              [#\1 (values "1" (if bool 'keyword 'string) #f pos end-pos 0
                           `((instr ,(if (zero? sharps) (add1 ones) 1) 0) . ,bool))]
              [#\#
               (define new-bool (if (= sharps 0) (not bool) bool))
               (define col (if new-bool 'string 'keyword))
               (values "#" col #f pos end-pos 0
                       (if (= sharps 4)
                           new-bool
                           `((instr ,ones ,(add1 sharps)) . ,new-bool)))])]
           [bool; we expect to see a '1'
            (syntax-parse tok
              [#\1 (values "1" (if bool 'keyword 'string) #f pos end-pos 0 `((instr 1 0) . ,bool))]
              [#\# (values "#" 'error #f pos end-pos 0 mode)])])])))
(module+ test
  (define test-color-port (open-input-string "
        ;startWithACommentNewLine!
1
  1 #   ;1
1 ####a #
;lastLineNoNewLine!"))
  (define (check-color? curr-mode exp-token exp-color exp-paren exp-pos exp-pos-end exp-mode)
    (define-values (act-token act-color act-paren act-pos act-pos-end _ act-mode)
      (colorer test-color-port 0 curr-mode))
    (check-equal? act-token exp-token)
    (check-eqv? act-color exp-color)
    (check-eqv? act-paren exp-paren)
    (check-eqv? act-pos exp-pos)
    (check-eqv? act-pos-end exp-pos-end)
    (check-equal? act-mode exp-mode))
  
  (check-color? #f 'whitespace 'no-color #f 1 10 #f)
  (check-color? #f "startWithACommentNewLine!" 'comment #f 10 37 #f)
  (check-color? #f "1" 'string #f 37 38 `((instr 1 0) . #f))
  (check-color? `((instr 1 0) . #f) 'whitespace 'no-color #f 38 41 `((instr 1 0) . #f))
  (check-color? `((instr 1 0) . #f) "1" 'string #f 41 42 `((instr 2 0) . #f))
  (check-color? `((instr 2 0) . #f) 'whitespace 'no-color #f 42 43 `((instr 2 0) . #f))
  (check-color? `((instr 2 0) . #f) "#" 'string #f 43 44 `((instr 2 1) . #t))
  (check-color? `((instr 2 1) . #t) 'whitespace 'no-color #f 44 47 `((instr 2 1) . #t))
  (check-color? `((instr 2 1) . #t) "1" 'comment #f 47 50 `((instr 2 1) . #t))
  (check-color? `((instr 2 1) . #t) "1" 'keyword #f 50 51 `((instr 1 0) . #t))
  (check-color? `((instr 1 0) . #t) 'whitespace 'no-color #f 51 52 `((instr 1 0) . #t))
  (check-color? `((instr 1 0) . #t) "#" 'keyword #f 52 53 `((instr 1 1) . #f))
  (check-color? `((instr 1 1) . #f) "#" 'keyword #f 53 54 `((instr 1 2) . #f))
  (check-color? `((instr 1 2) . #f) "#" 'keyword #f 54 55 `((instr 1 3) . #f))
  (check-color? `((instr 1 3) . #f) "#" 'keyword #f 55 56 `((instr 1 4) . #f))
  (check-color? `((instr 1 4) . #f) "a" 'error #f 56 57 `((instr 1 4) . #f))
  (check-color? `((instr 1 4) . #f) 'whitespace 'no-color #f 57 58 `((instr 1 4) . #f))
  (check-color? `((instr 1 4) . #f) "#" 'keyword #f 58 59 #f)
  (check-color? #f 'whitespace 'no-color #f 59 60 #f)
  (check-color? #f "lastLineNoNewLine!" 'comment #f 60 79 #f)
  (check-color? #f eof 'eof #f #f #f #f))



(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       ;color:start-colorer:get-token
       colorer]
      [(drracket:opt-out-toolbar-buttons)
       ; opt out of all usual DrRacket tool bar buttons
       '(drracket:syncheck debug-tool macro-stepper)]
      [else default])))