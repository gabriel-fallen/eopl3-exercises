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









