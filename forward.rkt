#lang racket/base

(require racket/match)
(require racket/math)
(require "common.rkt")

;; Partial derivatives of $f with regard to $x, in expression again (in case of high order derivatives)
;; The computation graph is constructed implicitly in the cache
(define/cached (deriv f x)
  (match f
    ; dx/dx = 1 ; dy/dx = 0
    [(var name _val)
     (if (eq? name (var-name x)) 1 0)]
    ; dC/dx = 0
    [n #:when (number? n) 0]
    ; d(u+v)/dx = du/dx + dv/dx
    [(g+ u v)
     (g+ (deriv u x) (deriv v x))]
    ; d(uv)/dx = du/dx v + u dv/dx
    [(g* u v)
     (g+ (g* (deriv u x) v)
         (g* u (deriv v x)))]
    ; d(u^v)/dx = d(e^(v ln u))/dx = e^(v ln u) d(v ln u)/dx = u^v (v du/dx/u + ln u dv/dx)
    [(g^ u v)
     (g* (g^ u v)
         (g+ (g* v (g/ (deriv u x) u))
             (g* (gln u) (deriv v x))))]

    ; d(sin(u))/dx = cos(u) du/dx = sin(pi/2 - u) du/dx
    [(gsin u)
     (g* (gsin (g- (/ pi 2) u)) (deriv u x))]
    ; d(ln(u))/dx = 1/u du/dx
    [(gln u)
     (g* (ginv u) (deriv u x))]
    ; More special cases...
    [_else (error "Unsupported expression type in deriv" f)]
    ))

(provide deriv)

;;; Test cases

(module+ test
  (require rackunit))

(module+ test
  ;; Test Case 1: f = x + y
  ;; Expected d/dx f = 1
  (define/var x-test1 5)
  (define/var y-test1 3)
  (define f1 (g+ x-test1 y-test1))
  (check-= (evaluate (deriv f1 x-test1))
           1
           0.01)

  ;; Test Case 2: f = x * y
  ;; Expected d/dx f = y
  (define/var x-test2 2)
  (define/var y-test2 4)
  (define f2 (g* x-test2 y-test2))
  (check-= (evaluate (deriv f2 x-test2))
           4
           0.01)

  ;; Test Case 3: f = x^3
  ;; Expected d/dx f = 3x^2
  (define/var x-test3 3)
  (define f3 (g^ x-test3 3))
  (check-= (evaluate (deriv f3 x-test3))
           (* 3 (expt 3 2))
           0.01)

  ;; Test Case 4: f = sin(x)
  ;; Expected d/dx f = cos(x)
  (define/var x-test4 (/ pi 3))
  (define f4 (gsin x-test4))
  (check-= (evaluate (deriv f4 x-test4))
           (cos (/ pi 3))
           0.01)

  ;; Test Case 5: f = ln(x)
  ;; Expected d/dx f = 1/x
  (define/var x-test5 2)
  (define f5 (gln x-test5))
  (check-= (evaluate (deriv f5 x-test5))
           (/ 1 2)
           0.01)

  ;; Test Case 6: f = x / y
  ;; Expected d/dx f = 1/y
  (define/var x-test6 10)
  (define/var y-test6 2)
  (define f6 (g/ x-test6 y-test6))
  (check-= (evaluate (deriv f6 x-test6))
           (/ 1 2)
           0.01)

  ;; Test Case 7: f = sin(x + y)
  ;; Expected d/dx f = cos(x + y)
  (define/var x-test7 2)
  (define/var y-test7 3)
  (define f7 (gsin (g+ x-test7 y-test7)))
  (check-= (evaluate (deriv f7 x-test7))
           (cos (+ 2 3))
           0.01)

  ;; Test Case 8: f = x^2 * y + y^3
  ;; Expected d/dx f = 2x * y
  (define/var x-test8 4)
  (define/var y-test8 3)
  (define f8 (g+ (g* (g^ x-test8 2) y-test8)
                (g^ y-test8 3)))
  (check-= (evaluate (deriv f8 x-test8))
           (* 2 4 3)
           0.01)

  ;; Test Case 9: f = sin(x) * ln(y)
  ;; Expected d/dx f = cos(x) * ln(y)
  (define/var x-test9 3)
  (define/var y-test9 4)
  (define f9 (g* (gsin x-test9) (gln y-test9)))
  (check-= (evaluate (deriv f9 x-test9))
           (* (cos 3) (log 4))
           0.01)

  ;; Test Case 10: f = x^2 * y + y^2 * x + sin(x * y)
  ;; Expected d/dx f = 2x * y + y^2 + cos(x * y) * y
  (define/var x-test10 2)
  (define/var y-test10 3)
  (define f10 (g+
              (g+ (g* (g^ x-test10 2) y-test10)
                  (g* (g^ y-test10 2) x-test10))
              (gsin (g* x-test10 y-test10))))
  (check-= (evaluate (deriv f10 x-test10))
           (+ (* 2 2 3)
              (* (expt 3 2))
              (* (cos (* 2 3)) 3))
           0.01)

  ;; Test Case 11: f = 0
  ;; Expected d/dx f = 0
  (define/var x-test11 5)
  (define f11 0)
  (check-= (evaluate (deriv f11 x-test11))
           0
           0.01)

  ;; Test Case 12: f = c (constant)
  ;; Expected d/dx f = 0
  (define/var x-test12 5)
  (define/var c-test12 10)
  (define f12 c-test12)
  (check-= (evaluate (deriv f12 x-test12))
           0
           0.01)

  ;; Test Case 13: f = y + 3 (variable y, no x)
  ;; Expected d/dx f = 0
  (define/var x-test13 5)
  (define/var y-test13 5)
  (define f13 (g+ y-test13 3))
  (check-= (evaluate (deriv f13 x-test13))
           0
           0.01)

  ;; Credit: GPT generated
  )
