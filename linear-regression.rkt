#lang racket/base

(require racket/list)
(require "common.rkt")

;; f(x1, x2) = ka * x1 + kb * x2 + kc
(define (model x1 x2 ka kb kc)
  (g+ (g+ (g* ka x1) (g* kb x2)) kc))

;; Whole dataset as a batch, in the form of (((x1 x2) y) ...)
(define (loss dataset ka kb kc)
  (let ([n (length dataset)])
    (g/ (foldl (lambda (datum acc)
                (let* ([input (first datum)]
                       [x1 (first input)]
                       [x2 (second input)]
                       [y_actual (second datum)]
                       [y_pred (model x1 x2 ka kb kc)]
                       [squard-diff (g^ (g- y_actual y_pred) 2)])
                  (g+ squard-diff acc)))
              0
              dataset)
       n)))

(require "forward.rkt")

(define (gradient-descent! dataset ka kb kc learning-rate)
  (let* ([loss-symb (loss dataset ka kb kc)]
         [dka (evaluate (deriv loss-symb ka))]
         [dkb (evaluate (deriv loss-symb kb))]
         [dkc (evaluate (deriv loss-symb kc))])
    (displayln (format "dka: ~a, dkb: ~a, dkc: ~a" dka dkb dkc))
    (set-var-val! ka (- (var-val ka) (* learning-rate dka)))
    (set-var-val! kb (- (var-val kb) (* learning-rate dkb)))
    (set-var-val! kc (- (var-val kc) (* learning-rate dkc)))
    (evaluate loss-symb)))

(define (train! dataset ka kb kc learning-rate iterations)
  (for ([i (in-range iterations)])
    (define l (gradient-descent! dataset ka kb kc learning-rate))
    (displayln (format "Iteration ~a loss: ~a" i l)))
  (displayln (format "~nResult: ka = ~a, kb = ~a, kc = ~a" ka kb kc)))

;; Input data and train

(module datagen racket/base
  (require math/distributions)

  ;; Function to generate the linear regression dataset where f=(a*x1 + b*x2 + c) + noise
  ;; n: number of data points
  ;; noise-std: standard deviation of the Gaussian noise
  (define (generate-linear-regression-data n a b c noise-std)
    (let ([dist (normal-dist 0 noise-std)])
      (for/list ([_i n])
        (let* ([x1 (random -3 3)]
               [x2 (random -3 3)]
               [noise (sample dist)]
               [y (+ (+ (* a x1) (* b x2)) c noise)])
          (list (list x1 x2) y)))))

  (define num-points 200)
  (define a 3.0)
  (define b 8.0)
  (define c -4.0)
  (define noise-std 1)

  (define *dataset* (generate-linear-regression-data num-points a b c noise-std))
  (provide *dataset*))
(require 'datagen)

(define/var a (random))
(define/var b (random))
(define/var c (random))
(train! *dataset* a b c 0.005 300)
