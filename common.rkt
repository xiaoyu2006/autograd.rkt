#lang racket/base

(require racket/match)

;;; Cache function
;; f has to be a pure function
(define (cached f)
  (let ([outputs (make-hash)])
    (lambda args
      (hash-ref! outputs args
                 (lambda ()
                   (apply f args))))))

(define-syntax define/cached
  (syntax-rules ()
    [(define/cached (name args ...) body ...)
     (define name
       (cached (lambda (args ...) body ...)))]))


;;; Variable that differentiate regarding to
(struct var (name val) #:prefab)

(define-syntax define/var
  (syntax-rules ()
    [(define/var name val)
     (define name
       (var (quote name) val))]))

;;; Computational *Tree*

(define/cached (evaluate f)
  (match f
    [(var _name val) val]                             ; unwarp var
    [(list '+ a b) (+ (evaluate a) (evaluate b))]    ; add
    [(list 'n a) (- (evaluate a))]                   ; neg NOTE: (- a b) = (add a (neg b))
    [(list '* a b) (* (evaluate a) (evaluate b))]    ; mul
    [(list 'i a) (/ 1 (evaluate a))]                 ; inv NOTE: (/ a b) = (mul a (inv b)) 
    [(list 'e a b) (expt (evaluate a) (evaluate b))] ; exp
    [n #:when (number? n) n]                         ; constant
    ))

(provide (struct-out var)
         cached define/cached define/var evaluate)


