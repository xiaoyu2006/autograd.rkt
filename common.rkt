#lang racket/base

(require racket/match)

;;; Cache utility

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


;;; Expression *tree* with elementary functions

(struct var (name val) #:prefab)

;; Gradient operators/functions
(struct g+ (a b) #:prefab)
(struct gneg (a) #:prefab)
(define (g- a b) (g+ a (gneg b)))
(struct g* (a b) #:prefab)
(struct ginv (a) #:prefab)
(define (g/ a b) (g* a (ginv b)))
(struct g^ (a b) #:prefab)
(struct gsin (a) #:prefab)
(struct gln (a) #:prefab)
(define (glog base a) (g/ (gln a) (log base)))

(define-syntax define/var
  (syntax-rules ()
    [(define/var name val)
     (define name
       (var (quote name) val))]))

;;; Computational *Tree*

(define/cached (evaluate f)
  (match f
    [(var _name val) val]
    [(g+ a b) (+ (evaluate a) (evaluate b))]
    [(gneg a) (- (evaluate a))]
    [(g* a b) (* (evaluate a) (evaluate b))]
    [(ginv a) (/ 1 (evaluate a))]
    [(g^ a b) (expt (evaluate a) (evaluate b))]
    [(gsin a) (sin (evaluate a))]
    [(gln a) (log (evaluate a))]
    [n #:when (number? n) n]
    ))

(provide (struct-out var)
         (struct-out gsin)
         (struct-out g+)
         (struct-out gneg) g-
         (struct-out g*)
         (struct-out ginv) g/
         (struct-out g^)
         (struct-out gln) glog
         cached define/cached define/var evaluate)

