#lang racket/base

(require "common.rkt")
(require racket/list)

;; MLP of 1 inputs, 1 hidden layers (3 neurons), 1 outputs, with LeakyReLU activation
(define (leaky-relu x)
  (g* (if (> (evaluate x) 0) 1 0.2) x))

(define (model x w1 w2 w3 b1 b2 b3 bw1 bw2 bw3 bb)
  (let* (
         ;; Hidden Layer
         [h1 (leaky-relu (g+ (g* w1 x) b1))]
         [h2 (leaky-relu (g+ (g* w2 x) b2))]
         [h3 (leaky-relu (g+ (g* w3 x) b3))]
         ;; Output Layer
         [y (g+ (g+ (g+ (g* bw1 h1) (g* bw2 h2)) (g* bw3 h3)) bb)])
    y))

(define (loss dataset params)
  (let ([n (length dataset)])
    (g/ (foldl (lambda (datum acc)  ; datum: ((x) y)
                 (let* ([input (first datum)]
                        [x (first input)]
                        [y_actual (second datum)]
                        [y_pred (apply model (cons x params))] ; Unpack params into model
                        [squared-diff (g^ (g- y_pred y_actual) 2)])
                   (g+ squared-diff acc)))
               0
               dataset)
        n)))

(require "forward.rkt")

(define (gradient-descent! dataset params learning-rate)
  (let* ([current-loss (loss dataset params)]
         [grads (map (lambda (param)
                       (evaluate (deriv current-loss param)))
                     params)]
         [updated-params
          (for/list ([param params]
                     [grad grads])
            (g+ param (g* (g* -1 learning-rate) grad)))])
    ;; Update parameter values
    (for ([p-upd updated-params]
          [p-ori params])
      (set-var-val! p-ori (evaluate p-upd)))
    ;; Return current loss for monitoring
    (evaluate current-loss)))

(define (train! dataset params learning-rate iterations)
  (for ([i (in-range iterations)])
    (define current-loss (gradient-descent! dataset params learning-rate))
    (displayln (format "Iteration ~a: Loss = ~a" (+ i 1) current-loss)))
  (for ([p params])
    (displayln (format "~a = ~a" (var-name p) (var-val p)))))

(module datagen racket/base
  (require math/distributions)

  ;; Function to generate cubic polynomial data: y = a*x^3 + b*x^2 + c*x + d + noise
  ;; n: number of data points
  ;; a, b, c, d: true parameters
  ;; noise-std: standard deviation of Gaussian noise
  (define (generate-cubic-data n a b c d noise-std)
    (define dist (normal-dist 0 noise-std))
    (for/list ([i n])
      (define x (random -3 3))
      (define noise (sample dist))
      (define y (+ (* a (expt x 3)) (* b (expt x 2)) (* c x) d noise))
      (list (list x) y)))

  ;; Parameters for dataset generation
  (define num-points 100)
  (define true-a 2.0)
  (define true-b -3.0)
  (define true-c 1.5)
  (define true-d 4.0)
  (define noise-std 1.0)

  ;; Generated dataset
  (define *dataset* (generate-cubic-data num-points true-a true-b true-c true-d noise-std))
  (provide *dataset*))

(require 'datagen)

;; INPUT -> HIDDEN weights and bias
(define/var w1 (random -1 1))
(define/var w2 (random -1 1))
(define/var w3 (random -1 1))
(define/var b1 (random -1 1))
(define/var b2 (random -1 1))
(define/var b3 (random -1 1))
;; HIDDEN -> OUTPUT weights and bias
(define/var bw1 (random -1 1))
(define/var bw2 (random -1 1))
(define/var bw3 (random -1 1))
(define/var bb (random -1 1))

(define params (list w1 w2 w3 b1 b2 b3 bw1 bw2 bw3 bb))
(train! *dataset* params 0.0005 500)
