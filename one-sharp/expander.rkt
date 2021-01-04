#lang racket
(require (for-syntax syntax/parse one-sharp/parser))
(provide (rename-out [one-sharp-mod-beg #%module-begin])
         #%top-interaction)

(define-syntax (one-sharp-mod-beg stx)
  (syntax-parse stx
    [(_ parsed-program) (define pvec (list->vector (filter (λ (node) (equal? (car node) 'instr)) (syntax->datum #'parsed-program))))
                        (or (analyze-jumps pvec)
                            #`(#%module-begin
                               'parsed-program))]))

; analyze-jumps: [VectorOf (instr Number Number Loc)] -> (U false sytax)
(define-for-syntax (analyze-jumps pvec)
  (define length (vector-length pvec))
  (let loop ([curr-instr 0])
    (cond
      [(= curr-instr length) #f]
      [else (define node (vector-ref pvec curr-instr))
            (define ones (cadr node))
            (define shas (caddr node))
            (define loc (cadddr node))
            (cond
              [(= shas 3) (or (analyze-forward-jump node curr-instr length) (loop (add1 curr-instr)))]
              [(= shas 4) (or (analyze-backward-jump node curr-instr) (loop (add1 curr-instr)))]
              [(= shas 5) (or (analyze-match-jumps node curr-instr length) (loop (add1 curr-instr)))]
              [else (loop (add1 curr-instr))])])))

(define-for-syntax (analyze-forward-jump instr instr-pos program-length)
  (define jump (+ (cadr instr) instr-pos))
  (if (> jump program-length)
      (raise-jump-error (format "1sharp instruction jumps ahead of the program scope (jump to instruction ~a when only ~a instructions are available)"
                                jump program-length)
                        instr)
      #f))

(define-for-syntax (analyze-backward-jump instr instr-pos)
  (define jump (- instr-pos (cadr instr)))
  (if (< jump 0)
      (raise-jump-error (format "1sharp instruction jumps behind the program scope (jumping to instruction at position ~a)" jump)
                        instr)
      #f))

(define-for-syntax (analyze-match-jumps instr instr-pos program-length)
  (define jump (+ 3 instr-pos)) ; biggest jump that can be made in a 'match' instruction
  (if (> jump program-length)
      (raise-jump-error (format "1sharp instruction might jump ahead of the program scope (might jump to instruction ~a when only ~a instructions are available)"
                                jump program-length)
                        instr)
      #f))

(define-for-syntax (raise-jump-error msg instr)
  (raise-syntax-error 'bad-jump msg (datum->syntax #f (format "~a~a"
                                                       (build-string (cadr instr) (λ (_) #\1))
                                                       (build-string (caddr instr) (λ (_) #\#)))
                                            (cadddr instr))))

(module+ test
  (require (for-syntax rackunit one-sharp/parser))
  
  (define-for-syntax test-input-port (open-input-string "
        ;startWithACommentNewLine!
1
  1 #   ;1
1 #### #
;lastLineNoNewLine!"))
  (define-for-syntax parsed-test-input-port (list->vector (filter (λ (node) (equal? (car node) 'instr)) (parse #f test-input-port))))
  (begin-for-syntax (check-exn
                     (regexp "ahead")
                     (lambda ()
                       (analyze-jumps parsed-test-input-port))))

  (define-for-syntax good (open-input-string "1#1##1###"))
  (begin-for-syntax (check-false (analyze-jumps (list->vector (parse #f good)))))


  (define-for-syntax bad-jump (open-input-string "1 ###  1#### 111### 1##"))
  (begin-for-syntax (check-exn
                     (regexp "ahead")
                     (lambda ()
                       (analyze-jumps (list->vector (parse #f bad-jump))))))

  (define-for-syntax bad-back (open-input-string "1 ###  1#### 111####1##"))
  (begin-for-syntax (check-exn
                     (regexp "behind")
                     (lambda ()
                       (analyze-jumps (list->vector (parse #f bad-back))))))

  (define-for-syntax good-back (open-input-string "1 ###  1#### 11####1##"))
  (begin-for-syntax (check-false
                     (analyze-jumps (list->vector (parse #f good-back)))))

  (define-for-syntax comments (open-input-string "
;comment1
1
;comment2
1
;comment3
#
;comment4
#
;comment5
##"))
  (begin-for-syntax (check-exn
                     (regexp "behind")
                     (lambda ()
                       (analyze-jumps (list->vector (filter (λ (node) (equal? (car node) 'instr)) (parse #f comments))))))))