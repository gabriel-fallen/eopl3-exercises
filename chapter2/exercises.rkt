#lang racket

(require racket/generator)
(require rackunit/chk) ; for testing

;;; Exercise 2.3

;; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)

; Utilities

(define (diff-tree->int dt)
  (case (first dt)
    [(one) 1]
    [(diff) (let ([l (diff-tree->int (second dt))]
                   [r (diff-tree->int (third dt))])
               (- l r))]))

; Simple tests
(check-equal? (diff-tree->int '(one)) 1)
(check-equal? (diff-tree->int '(diff (one) (diff (one) (one)))) 1)
(check-equal? (diff-tree->int '(diff (diff (one) (one)) (one))) -1)

; Now reverse conversion from integers to diff-trees.
; I'll do a dumb thing of representing
; - positive integers as (0 + 1 + 1 + ...)
; - negative integers as (0 - 1 - 1 - ...)
; Recall that x + 1 ≡ x - (-1).

(define (int->diff-tree x)
  (define zero '(diff (one) (one))) ; a constant value
  (define minus-one (list 'diff zero '(one)))
  (cond
    [(eq? x 0) zero]
    [(< 0 x) (sequence-fold
          (λ (dt n) (list 'diff dt minus-one))
          zero ; the initial value
          (in-range 0 x))]
    [(> 0 x) (sequence-fold
          (λ (dt n) (list 'diff dt '(one))) ; inlined predecessor
          zero ; the initial value
          (in-range x 0 1))]))

(for ([x (in-range -5 5)])
  (check-equal? (diff-tree->int (int->diff-tree x)) x)) ; poor-man's property-based testing 😂


;; 1. Show that every number has infinitely many representations in this system.

; Let's prove that construcively — write a function that generates (potentially) infinitely many
; representations for a given number

; We'll do a dumb thing here and generate additional representations for a given number in the form of
; x, x-0, x-0-0, ...

(define (mz dt)
  (list 'diff dt (zero))) ; referencing zero function defined below

(define (gen-representations dt)
  (in-generator
   (let loop ([x dt])
     (yield x)
     (loop (mz x)))))

(define (inf-representations x)
  (gen-representations (int->diff-tree x)))

; print several representations
(define (print-representations x n)
  (for [(i (in-range n))
        (r (inf-representations x))]
    (printf "~s\n" r)))


;; 2. Turn this representation of the integers into an implementation by writing zero, is-zero?, successor, and predecessor

(define zero (λ () '(diff (one) (one))))

(check-equal? (diff-tree->int (zero)) 0)

;; same-diff-trees? — compare if two given diff-trees have the same shape
(define (same-diff-trees? dt1 dt2)
  (case (first dt1)
    [(one) (equal? 'one (first dt2))]
    [(diff) (case (first dt2)
              [(one) #f]
              [(diff) (and (same-diff-trees? (second dt1) (second dt2))
                           (same-diff-trees? (third dt1) (third dt2)))])]))

; FIXME: I think diff tree evaluates to zero iff it's balanced...
; which is wrong
(define (is-zero? dt)
  (case (first dt)
    [(one) #f]
    [(diff) (same-diff-trees? (second dt) (third dt))]))

(check-not-false (is-zero? (zero)))
(check-not-false (is-zero? (list 'diff (zero) (zero))))
(check-false (is-zero? '(one)))
(check-false (is-zero? (list 'diff (zero) ('one))))

(define minus-one (list 'diff (zero) '(one)))

; (x + 1) ≡ (x - (-1)) 😁
(define (successor dt)
  (list 'diff dt minus-one))


(check-equal? (diff-tree->int (successor (zero))) 1)
(check-equal? (diff-tree->int (successor (successor (zero)))) 2)
(check-equal? (diff-tree->int (successor '(one))) 2)


(define (predecessor dt)
  (list 'diff dt '(one)))

(check-equal? (diff-tree->int (predecessor (zero))) -1)
(check-equal? (diff-tree->int (predecessor (predecessor (zero)))) -2)
(check-equal? (diff-tree->int (predecessor '(one))) 0)


;; 3. Write a procedure diff-tree-plus that does addition in this representation.

; The same idea as with successor:
; x + y ≡ x - (-y)

(define (diff-tree-negate dt)
  (list 'diff (zero) dt))

(define (diff-tree-plus dt1 dt2)
  (list 'diff dt1 (diff-tree-negate dt2)))

(check-not-false (is-zero? (diff-tree-plus (zero) (zero))))
(check-equal? (diff-tree->int (diff-tree-plus (successor (zero)) (predecessor (zero)))) 0)
(check-not-false (is-zero? (diff-tree-plus (successor (zero)) (predecessor (zero)))))



;;; Exercise 2.11

;; The ribcage environment representation

;; Env ::= () | ( (Var-list . Val-list) . Env )

(define empty-env (λ () '()))


(define empty-env? null?)

(check-not-false (empty-env? (empty-env)))


(define (extend-env var val env)
  `( ((,var) . (,val)) . ,env))

(define (extend-env* vars vals env)
  `( (,vars . ,vals) . ,env))


(define (apply-env env var)
  (if (empty-env? env)
      (error 'apply-env "Empty environment")
      (let ([vars (caar env)]
            [vals (cdar env)]
            [env1 (cdr env)])
        (let ([i (index-of vars var)])
          (if (not i) ; i is #f meaning the var wasn't found in the list of vars
              (apply-env env1 var)
              (list-ref vals i))))))

; some tests
(define test-env (extend-env* '(w z) '(#f foo) (extend-env* '(x y z) '(11 12 14) (empty-env))))

(check-equal? (apply-env test-env 'y) 12)
(check-equal? (apply-env test-env 'z) 'foo)
(check-equal? (apply-env test-env 'w) #f)



;;; Exercise 2.18
;;; aka (list) Zipper

;; NodeInSequence ::= (Int Listof(Int) Listof(Int))
;;  implement number->sequence, current-element, move-to-left, move-to-right, insert-to-left, insert-to-right, at-left-end?, and at-right-end?

(define (number->sequence x)
  (list x '() '()))


(define (current-element z)
  (first z))


(for ([x (in-range -5 5)])
  (check-equal? (current-element (number->sequence x)) x))


; (move-to-left '(3 (2 1) (4))) ≡ '(2 (1) (3 4))
(define (move-to-left z)
  (let ([x (first z)]
        [l (second z)]
        [r (third z)])
    (list (first l) (rest l) (cons x r))))

(check-equal? (move-to-left '(3 (2 1) (4))) '(2 (1) (3 4)))


; (move-to-right '(3 (2 1) (4))) ≡ '(4 (3 2 1) ())
(define (move-to-right z)
  (let ([x (first z)]
        [l (second z)]
        [r (third z)])
    (list (first r) (cons x l) (rest r))))

(check-equal? (move-to-right '(3 (2 1) (4))) '(4 (3 2 1) ()))


(define (insert-to-left y z)
  (let ([x (first z)]
        [l (second z)]
        [r (third z)])
    (list x (cons y l) r)))


(define (insert-to-right y z)
  (let ([x (first z)]
        [l (second z)]
        [r (third z)])
    (list x l (cons y r))))


(define (at-left-end? z)
  (null? (second z)))


(define (at-right-end? z)
  (null? (third z)))



;;; Exercise 2.29

;; I'll use built-in Racket structures for cases but won't gather them into a single union-type
;; because the match constract works regardles, it's dynamic typing anyway.

; TODO: write checks for constructors' argumets' types

(struct var-exp (var) #:transparent) ; TODO: var is identifier?

(struct lambda-exp (bound-vars body) #:transparent) ; TODO: (list-of identifier?) and lc-exp?

(struct app-exp (rator rands) #:transparent) ; TODO: lc-exp? and (list-of lc-exp?)

(define (lc-exp? exp)
  (or (var-exp? exp) (lambda-exp? exp) (app-exp? exp)))


(define (unparse-lc-exp exp)
  (match exp
    [(var-exp var) var]
    [(lambda-exp vars body) (list 'lambda vars (unparse-lc-exp body))]
    [(app-exp rator rands) (list (unparse-lc-exp rator) (map unparse-lc-exp rands))]
    [_ (error 'unparse-lc-exp "Malformed Lc-exp: ~s" exp)]))


(define (parse-lc-exp term)
  (match term
    [(list rator (list rands ...)) (app-exp (parse-lc-exp rator) (map parse-lc-exp rands))]
    [(list 'lambda (list vars ...) body) (lambda-exp vars (parse-lc-exp body))]
    [var #:when (symbol? var) (var-exp var)]))


(check-equal? (parse-lc-exp (unparse-lc-exp (var-exp 'foo))) (var-exp 'foo))

(let* ([v (var-exp 'foo)]
       [exp (lambda-exp '(bar foo) v)])
  (check-equal? (parse-lc-exp (unparse-lc-exp exp)) exp))

(let* ([u (var-exp 'baz)]
       [v (var-exp 'foo)]
       [l (lambda-exp '(bar foo) v)]
       [exp (app-exp l (list u))])
  (check-equal? (parse-lc-exp (unparse-lc-exp exp)) exp))



;;; Exercise 2.31

(struct const-exp (num) #:transparent)

(struct diff-exp (operand1 operand2) #:transparent)

;; Manual "monadic" recursive-descent parser

(define (parse-prefix-exp lst)
  (define (parser lst)
    (match lst
      [(list '- vs ...) (let* ([r1 (parser vs)]
                               [o1 (first r1)]
                               [rst (second r1)]
                               [r2 (parser rst)]
                               [o2 (first r2)])
                          (list (diff-exp o1 o2) (second r2)))]
      [(list x xs ...) #:when (integer? x) (list (const-exp x) xs)]
      [_ (error 'parser "Malformed exp: ~s" lst)]))
  (let ([r (parser lst)])
    (if (not (empty? (second r)))
        (error 'parse-prefix-exp "Unbalanced expression, the rest is: ~s" (second r))
        (first r))))

(let ([exp (diff-exp (const-exp 10) (const-exp -2))])
  (check-equal? (parse-prefix-exp '(- 10 -2)) exp))












