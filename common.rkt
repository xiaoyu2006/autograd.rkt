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


;;; Expression *Tree*
(struct var (name val) #:prefab)
(struct g+ (a b) #:prefab)
(struct g-neg (a) #:prefab)
(define (g- a b) (g+ a (g-neg b)))
(struct g* (a b) #:prefab)
(struct g-inv (a) #:prefab)
(define (g/ a b) (g* a (g-inv b)))
(struct g^ (a b) #:prefab)
(struct gsin (a) #:prefab)

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
    [(g-neg a) (- (evaluate a))]
    [(g* a b) (* (evaluate a) (evaluate b))]
    [(g-inv a) (/ 1 (evaluate a))]
    [(g^ a b) (expt (evaluate a) (evaluate b))]
    [(gsin a) (sin (evaluate a))]
    [n #:when (number? n) n]
    ))

(provide (struct-out var)
         (struct-out gsin)
         (struct-out g+)
         (struct-out g-neg) g-
         (struct-out g*)
         (struct-out g-inv) g/
         (struct-out g^)
         cached define/cached define/var evaluate)

