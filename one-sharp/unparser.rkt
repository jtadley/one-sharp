#lang racket
(require one-sharp/parser)
(provide (rename-out [unparse-1# unparse]
                     [format-1# format]))

; Loc is a (list Any Number Number Number Number)
;;               src line   col    pos    span

; AST is a [Listof (U (instr Number Number Loc) (comment String Loc)))]
; unparser: AST -> String 
; produces the code string when given an AST.
(define (unparse-1# ast)
  (define (node->string node)
    (match node
      [`(instr ,ones ,sharps  ,_) (string-append (build-string ones (λ (_) #\1))
                                                 (build-string sharps (λ (_) #\#))
                                                 "\n")] 
      [`(comment ,cmnt-string ,_) (string-append ";" cmnt-string "\n")]))
  (define comment? (λ (node) (and (symbol? (car node)) (symbol=? 'comment (car node)))))
  (define instr? (λ (node) (and (symbol? (car node)) (symbol=? 'instr (car node)))))
  
  ; idea is to convert an `instr node as is, but add a '\n' before each comment block
  ; except for the very first comment block (which is represented by `init-comments`)
  ; this is so I can manually add a newline before the very first comment or instruction
  ; the newline is necessary so I can then add a "#lang one-sharp"
  (define reducer (λ (a b) (string-append b (node->string a))))
  (define init-comments (foldl reducer "" (takef ast comment?)))
  
  ; group-comments : AST -> [Listof (U (instr Number Number Loc) [Listof (comment String Loc)]))] -> 
  (define (group-comments ast)
    (match ast
      ['() '()]
      [`((comment ,c ,_) . ,rest) `(,(takef ast comment?) . ,(group-comments (dropf ast comment?)))]
      [`(,instr . ,rest) `(,instr . ,(group-comments rest))]))
  ; combine, and remove white space from the right of the string
  (string-trim (string-append
                "\n"
                init-comments
                (foldr (λ (e r) (string-append (if (instr? e) (node->string e)
                                                   (foldl reducer "\n" e))
                                               r))
                       ""
                       (group-comments (dropf ast comment?)))) #:left? #f))

; uses parser + unparser to format given code
; format-1#: String -> String
(define (format-1# code)
  (with-handlers ([exn:fail:syntax? (λ (_) code)])
    (unparse-1# (parse #f (open-input-string code)))))

(module+ test
  (require rackunit one-sharp/parser)

  (define empty-str (parse #f (open-input-string "")))
  (check-equal? (unparse-1# empty-str) "")
  
  (define test-input-port (open-input-string "
        ;startWithACommentNewLine!
1
  1 #   ;1
1 #### #
;lastLineNoNewLine!"))
  (define parsed-test-input-port (parse #f test-input-port))
  (define expected-code
    "
;startWithACommentNewLine!
11#

;1
1#####

;lastLineNoNewLine!")
  (check-equal? (unparse-1# parsed-test-input-port) expected-code)

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

  (define exp-code
    "
;comment1
;comment2
;comment3
;comment4
11##

;comment5")
  (check-equal? (unparse-1# (parse #f comments)) exp-code)

  (define instr (open-input-string "
1#1###1##1#####1#"))
  (define exp-instr
    "
1#
1###
1##
1#####
1#")
  (check-equal? (unparse-1# (parse #f instr)) exp-instr))
