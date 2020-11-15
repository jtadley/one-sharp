#lang racket
(require one-sharp/lexer syntax/parse syntax/readerr)
(provide (rename-out [parse-1# parse]))

(define (parse-1# src in)
  (define 1? (syntax-parser [#\1 #t] [_ #f]))
  (define sharp? (syntax-parser [#\# #t] [_ #f]))
  (define comment? (syntax-parser #:datum-literals(comment) [(comment _) #t] [_ #f]))
  (define get-unknown (syntax-parser #:datum-literals(unknown) [(unknown c) (syntax->datum #'c)] [_ #f]))
  (define unknown? (compose char? get-unknown))
  (define whitespace? (syntax-parser #:datum-literals(whitespace) [(whitespace) #t] [_ #f]))
  (define eof? (syntax-parser #:datum-literals(eof) [(eof) #t] [_ #f]))

  (define tokens
    (let lexer ()
      (define tok (lex src in))
      (cond
        [(eof? tok) empty]
        [(unknown? tok) (raise-read-error (format "1sharp programs can't contain the character '~a'" (get-unknown tok))
                                          src
                                          (syntax-line tok)
                                          (syntax-column tok)
                                          (syntax-position tok)
                                          (syntax-span tok))]
        [(whitespace? tok) (lexer)]
        [else (cons tok (lexer))])))
    
  (define (parse tokens)
    (cond
      [(empty? tokens) empty]
      [else (define-values (1s first-1 comments maybe-sharps) (parse-1s tokens))
            (define-values (sharps last-# pre-comments post-comments rest) (parse-#s maybe-sharps))
            (cond
              [(zero? (+ 1s sharps)) (append comments post-comments)]
              [(and (zero? 1s) (> sharps 0))
               (define first-#  (car tokens))
               (define pos (syntax-position first-#))
               (define span (add1 (- (syntax-position last-#) pos)))
               (raise-syntax-error #f
                                   "A 1sharp instruction should start with atleast one 1"
                                   (datum->syntax #f (build-string sharps (λ (_) #\#))
                                                  `(,src ,(syntax-line first-#) ,(syntax-column first-#) ,pos ,span)))]
              [else
               (define starting-line (syntax-line first-1))
               (define starting-col (syntax-column first-1))
               (define starting-pos (syntax-position first-1))
               (define ending-pos (add1 (syntax-position last-#)))
               (define span (- ending-pos starting-pos))
               (cond
                 [(not (< 0 sharps 6)) (raise-syntax-error #f
                                                           (format "A 1sharp instruction '#' count should be in range [1,5], found ~a #s" sharps)
                                                           (datum->syntax #f (format "~a~a"
                                                                                     (build-string 1s (λ (_) #\1))
                                                                                     (build-string sharps (λ (_) #\#)))
                                                                          `(,src ,starting-line ,starting-col ,starting-pos ,span)))]
                 [else (append comments
                               pre-comments
                               (cons (datum->syntax #f `(instr ,1s ,sharps)
                                                    `(,src ,starting-line ,starting-col ,starting-pos ,span))
                                     (append post-comments (parse rest))))])])]))
  (define (parse-1s tokens)
    (cond
      [(or (empty? tokens)
           (sharp? (car tokens))) (values 0 #f empty tokens)]
      [else (define-values (1s first-1 comments rest) (parse-1s (cdr tokens)))
            (match (car tokens)
              [(? comment?) (values 1s first-1 (cons (car tokens) comments) rest)]
              [(? 1?) (values (add1 1s) (car tokens) comments rest)])]))
  (define (parse-#s tokens)
    (cond
      [(or (empty? tokens)
           (1? (car tokens))) (values 0 #f empty empty tokens)]
      [else (define-values (sharps last-# pre-comments post-comments rest) (parse-#s (cdr tokens)))
            (match (car tokens)
              [(? comment?)
               (define comment (car tokens))
               (define new-pre-comments (if last-# (cons comment pre-comments) pre-comments))
               (define new-post-comments (if (not last-#) (cons comment post-comments) post-comments))
               (values sharps last-# new-pre-comments new-post-comments rest)]
              [(? sharp?) (values (add1 sharps) (or last-# (car tokens)) pre-comments post-comments rest)])]))
  (parse tokens))