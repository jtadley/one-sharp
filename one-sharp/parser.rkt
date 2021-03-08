#lang racket
(require one-sharp/lexer syntax/parse syntax/readerr)
(provide (rename-out [parse-1# parse]))

; Loc is a (list Any Number Number Number Number)
;;               src line   col    pos    span

; parser: any input-port -> [Listof (U (instr Number Number Loc) (comment String Loc)))]
; returns a list of comments and instructions found in the given input port
; if comments are found between an instruction, they are hoisted above it.
(define (parse-1# src in)
  ; helpers
  (define 1? (syntax-parser [#\1 #t] [_ #f]))
  (define sharp? (syntax-parser [#\# #t] [_ #f]))
  (define comment? (syntax-parser #:datum-literals(comment) [(comment _) #t] [_ #f]))
  (define extract-comment (λ (loc) (syntax-parser #:datum-literals(comment) [(comment c) `(comment ,(syntax->datum #'c) ,loc)] [_ #f])))
  (define get-unknown (syntax-parser #:datum-literals(unknown) [(unknown c) (syntax->datum #'c)] [_ #f]))
  (define unknown? (compose char? get-unknown))
  (define whitespace? (syntax-parser #:datum-literals(whitespace) [(whitespace) #t] [_ #f]))
  (define eof? (syntax-parser #:datum-literals(eof) [(eof) #t] [_ #f]))

  ; apply lex repeatedly, collect comments, 1s, and #s. If we see an unknown, throw an error.
  (define tokens
    (let lexer ()
      (define tok (lex src in))
      (cond
        [(eof? tok) empty]
        [(unknown? tok) (raise-syntax-error 'bad-syntax
                                            "found an unexpected character"
                                            (datum->syntax #f (list->string `(,(get-unknown tok)))
                                                           `(,src
                                                             ,(syntax-line tok)
                                                             ,(syntax-column tok)
                                                             ,(syntax-position tok)
                                                             ,(syntax-span tok))))]
        [(whitespace? tok) (lexer)]
        [else (cons tok (lexer))])))
  
  ; collect instructions and comments  
  (define (parse tokens)
    (cond
      [(empty? tokens) empty]
      [else (define-values (1s first-1 comments maybe-sharps) (parse-1s tokens))
            (define-values (sharps last-# pre-comments post-comments rest) (parse-#s maybe-sharps))
            (cond
              [(zero? (+ 1s sharps)) ; no instruction was found, but might have comments
               (append comments post-comments)]
              [(and (zero? 1s) (> sharps 0)) ; no 1s were found, but we found #s (only possible for the very first instruction)
               (define first-#  (car tokens))
               (define pos (syntax-position first-#))
               (define span (add1 (- (syntax-position last-#) pos)))
               (raise-syntax-error 'bad-instr
                                   "A 1sharp instruction should start with atleast one 1"
                                   (datum->syntax #f (build-string sharps (λ (_) #\#))
                                                  `(,src ,(syntax-line first-#) ,(syntax-column first-#) ,pos ,span)))]
              [else ; we found an instruction
               (define starting-line (syntax-line first-1))
               (define starting-col (syntax-column first-1))
               (define starting-pos (syntax-position first-1))
               (define ending-pos (add1 (syntax-position last-#)))
               (define span (- ending-pos starting-pos))
               (cond
                 [(not (< 0 sharps 6)) (raise-syntax-error 'bad-instr
                                                           (format "A 1sharp instruction's '#' count should be in range [1,5], found ~a #s" sharps)
                                                           (datum->syntax #f (format "~a~a"
                                                                                     (build-string 1s (λ (_) #\1))
                                                                                     (build-string sharps (λ (_) #\#)))
                                                                          `(,src ,starting-line ,starting-col ,starting-pos ,span)))]
                 [else (append comments
                               pre-comments
                               (cons `(instr ,1s ,sharps (,src ,starting-line ,starting-col ,starting-pos ,span))
                                     (append post-comments (parse rest))))])])]))
  (define (parse-1s tokens)
    (cond
      [(or (empty? tokens)
           (sharp? (car tokens))) (values 0 #f empty tokens)]
      [else (define-values (1s first-1 comments rest) (parse-1s (cdr tokens)))
            (match (car tokens)
              [(? comment?) (define comment (car tokens))
                            (values 1s first-1 (cons ((extract-comment `(,src
                                                                         ,(syntax-line comment)
                                                                         ,(syntax-column comment)
                                                                         ,(syntax-position comment)
                                                                         ,(syntax-span comment))) comment) comments) rest)]
              [(? 1?) (values (add1 1s) (car tokens) comments rest)])]))
  (define (parse-#s tokens)
    (cond
      [(or (empty? tokens)
           (1? (car tokens))) (values 0 #f empty empty tokens)]
      [else (define-values (sharps last-# pre-comments post-comments rest) (parse-#s (cdr tokens)))
            (match (car tokens)
              [(? comment?)
               (define _comment (car tokens))
               (define comment ((extract-comment `(,src
                                                   ,(syntax-line _comment)
                                                   ,(syntax-column _comment)
                                                   ,(syntax-position _comment)
                                                   ,(syntax-span _comment))) _comment))
               (define new-pre-comments (if last-# (cons comment pre-comments) pre-comments))
               (define new-post-comments (if (not last-#) (cons comment post-comments) post-comments))
               (values sharps last-# new-pre-comments new-post-comments rest)]
              [(? sharp?) (values (add1 sharps) (or last-# (car tokens)) pre-comments post-comments rest)])]))
  (parse tokens))

(module+ test
  (require rackunit)
  
  (define test-input-port (open-input-string "
        ;startWithACommentNewLine!
1
  1 #   ;1
1 #### #
;lastLineNoNewLine!"))
  (define syntax~? check-equal?)
  (define parsed-test-input-port (parse-1# #f test-input-port))
  (define expected-parse-tree `((comment "startWithACommentNewLine!" (#f #f #f 10 27))
                                (instr 2 1 (#f #f #f 37 7))
                                (comment "1" (#f #f #f 47 3))
                                (instr 1 5 (#f #f #f 50 8))
                                (comment "lastLineNoNewLine!" (#f #f #f 59 19))))

  (for ([act-node parsed-test-input-port]
        [exp-node expected-parse-tree])
    (syntax~? act-node exp-node))

  ; testing an instruction with no 1's
  (define no-sharp (open-input-string "#####"))
  (check-exn
   (regexp "atleast one")
   (lambda ()
     (parse-1# #f no-sharp)))

  ; testing an instruction with more than 5 sharps
  (define 6-sharp (open-input-string "1 ## ## ##"))

  (check-exn
   (regexp "\\[1,5\\], found 6")
   (lambda ()
     (parse-1# #f 6-sharp)))

  ; testing a program with unknown character
  (define unknown (open-input-string "1 ## ## s##"))

  (check-exn
   (regexp "the character 's'")
   (lambda ()
     (parse-1# #f unknown)))

  ; testing hoisted comments
  (define comments (open-input-string "
;comment1
1
;comment2
1
;comment3
#
;comment4
#
;comment5"))

  (define exp-parse-tree `((comment "comment1" (#f #f #f 2 10))   ; commentn + semi + newline
                           (comment "comment2" (#f #f #f 14 10))
                           (comment "comment3" (#f #f #f 26 10))
                           (comment "comment4" (#f #f #f 38 10)) 
                           (instr 2 2 (#f #f #f 12 37))           ; 2x1\n + #\n + # + 3x10 (each comment line spans 10 chars) 
                           ; last comment is not hoisted
                           (comment "comment5" (#f #f #f 50 9)))) ; commentn + semi (no newline here)
  (for ([act-node (parse-1# #f comments)]
        [exp-node exp-parse-tree])
    (syntax~? act-node exp-node)))
