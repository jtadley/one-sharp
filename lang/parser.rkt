#lang racket
;; Based off of https://github.com/dyoo/brainfudge
;; This module parses 1# and gives us an AST:
#; "The reader-level parse must produce a module form
    as a syntax object. As always, the second sub-form after
    module specifies the module language that controls the meaning
    of the module’s body forms."

(require syntax/readerr)

(provide (rename-out [parse-1sharp-exp read]
                     [parse-1sharp read-syntax]))

;; parse-1sharp: any input-port -> syntax
(define (parse-1sharp src in)
  (with-syntax ([AST (parse-1sharp-internal src in)])
    #'(module huh racket
        (provide ast)
        (define ast AST))))

;; parse-1sharp-internal: any input-port -> [Listof (U `(I ,number ,number) string)]
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
     (raise-read-error (string-append "A 1sharp instruction can not start with a "
                                      (list->string (list #\' curr-char #\')))
                       src line column position 1)]))
 
;; parse-1s: any input-port number number number -> [Listof (U `(I ,number ,number) string)]
;; Invariant: We atleast have one "1" in the beginning
(define (parse-1s src in ln col pos)
  (match-let ([`(,1s ,comments) (let 1s ([n 0]
                                         [comments (λ (ls) ls)]) ;dlist: Adds all comments to the given list. O(n)
                                  (regexp-match #px"^\\s*" in)
                                  (define curr-char (peek-char in))
                                  (cond
                                    [(and (char? curr-char) (char=? #\; curr-char))
                                     (read-char in)
                                     (define comment (read-line in))
                                     (1s n (λ (ls) (comments `(cons ,comment ,ls))))]
                                    [(and (char? curr-char) (char=? #\1 curr-char)) (read-char in)(1s (add1 n) comments)]
                                    [else `(,n ,comments)]))])
    (comments (parse-#s src in ln col pos 1s))))

;; parse-#s: any input-port number number number number -> [Listof (U `(I ,number ,number) string)]
;; Invariant: We don't have a "1" in the beginning. Could have an eof
(define (parse-#s src in ln col pos ones)
  (match-let ([`(,sharps ,comments) (let sharps ([n 0]
                                                 [comments (λ (ls) ls)])
                                      (regexp-match #px"^\\s*" in)
                                      (define curr-char (peek-char in))
                                      (cond
                                        [(and (char? curr-char) (char=? #\; curr-char))
                                         (read-char in)
                                         (define comment (read-line in))
                                         (sharps n (λ (ls) (comments `(cons ,comment ,ls))))]
                                        [(and (char? curr-char) (char=? #\# curr-char)) (read-char in)(sharps (add1 n) comments)]
                                        [else `(,n ,comments)]))])
    (if (<= 1 sharps 5)
        (comments `(cons (list 'I ,ones ,sharps) ,(parse-1sharp-internal src in)))
        (raise-read-error (string-append "A 1sharp instruction '#' count should be in range [1,5], found "
                                         (number->string sharps) " '#'s.")
                          src ln col pos (+ ones sharps)))))

(define (parse-1sharp-exp in)
  (syntax->datum (parse-1sharp #f in)))