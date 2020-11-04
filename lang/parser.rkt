#lang racket
;; Based off of https://github.com/dyoo/brainfudge and Racket docs
;; This module parses 1# and binds `ast` to 1# AST

(require syntax/readerr)

(provide (rename-out [parse-1sharp-exp read]
                     [parse-1sharp read-syntax])
         get-info)

;; parse-1sharp: any input-port -> syntax
;; reader called to parse 1# programs
(define (parse-1sharp src in)
  (with-syntax ([AST (parse-1sharp-init src in)])
    #'(module huh racket
        (provide ast)
        (define ast AST))))

;; parse-1sharp-init: any input-port -> [Listof (U `(I ,number ,number ,source) string)]
;; initial call to the parser to start the main parsing
(define (parse-1sharp-init src in)
  ; read whitespace
  (regexp-match #px"^\\s*" in)
  (define-values (line column position) (port-next-location in))
  (define curr-char (peek-char in))
  (cond
    [(char=? #\; curr-char) (read-char in)`(cons ,(read-line in) ,(parse-1sharp-init src in))]
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
    [(eof-object? curr-char) 'empty] ; this is the only point eof
    [(char=? #\; curr-char) (read-char in)`(cons ,(read-line in) ,(parse-1sharp-internal src in))]
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
        (comments-pre-instr `(cons (list 'I ,ones ,sharps ,(source ln col pos span))
                                   ,(comments-post-instr (parse-1sharp-internal src in))))
        (raise-read-error (string-append "A 1sharp instruction '#' count should be in range [1,5], found "
                                         (number->string sharps) " '#'s.")
                          src ln col pos span))))

(define (parse-1sharp-exp in)
  (syntax->datum (parse-1sharp #f in)))

;; d-list util
;; Allows 'n' snocs to be in O(n)
(define d-list-empty (λ (ls) ls))
(define (d-list-snoc x xs) (λ (ls) (xs `(cons ,x ,ls))))
(define (d-list-append xs ys) (λ (ls) (xs (ys ls))))

;; source struct (not using Racket struct cus then we get 3D syntax
(define (source ln col pos span) `(list 'source ,ln ,col ,pos ,span))

;; DrRacket util
(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       ;color:start-colorer:get-token
       (λ (in)
         (define-values (line column pos) (port-next-location in))
         (define char (read-char in))
         (cond
           [(eof-object? char) (values char 'eof #f #f #f)]
           [(char-whitespace? char) (values (string char) 'white-space #f pos (add1 pos))]
           [(char=? #\1 char)(values (string char) 'keyword #f pos (add1 pos))]
           [(char=? #\# char)(values (string char) 'keyword #f pos (add1 pos))]
           [(char=? #\; char)
            (define line (read-line in))
            (define comment (if (eof-object? line) "" line))
            (define comment-length (string-length comment))
            (values (string-append ";" comment) 'comment #f pos (+ pos 1 comment-length))]
           [else (values char 'error #f pos (add1 pos))]))]
      [else default])))