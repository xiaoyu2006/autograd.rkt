#lang racket/base

(require racket/match)
(require "common.rkt")
(require racket/math)

;; Partial derivatives of $f with regard to $x, in expression again (in case of high order derivatives)
(define/cached (deriv f x)
  (match f
    ; dx/dx = 1 ; dy/dx = 0
    [(var name _val)
     (if (eq? name x) 1 0)]
    ; dC/dx = 0
    [n #:when (number? n) 0]
    ; d(u+v)/dx = du/dx + dv/dx
    [(g+ u v)
     (g+ (deriv u x) (deriv v x))]
    ; d(uv)/dx = du/dx v + u dv/dx
    [(g* u v)
     (g+ (g* (deriv u x) v)
         (g* u (deriv v x)))]
    ; d(u^C)/dx = C u^(C-1) du/dx
    [(g^ u c)
     #:when (number? c)
     (g* c (g* (g^ u (- c 1)) (deriv u x)))]
    ; d(C^u)/dx = C^u ln(C) du/dx
    [(g^ c u)
     #:when (number? c)
     (g* (g^ c u) (g* (log c) (deriv u x)))]
    ; d(sin(u))/dx = cos(u) du/dx = sin(pi/2 - u) du/dx
    [(gsin u)
     (g* (gsin (g- (/ pi 2) u)) (deriv u x))]
    ; d(ln(u))/dx = 1/u du/dx
    [(gln u)
     (g* (ginv u) (deriv u x))]
    ; More special cases...
    ))

;;; Put together

(module+ test
  (require rackunit)
  (define X* 2)
  (define Y* 3)
  (define Z* 7)
  (define x (var 'x X*))
  (define y (var 'y Y*))
  (define z (var 'z Z*))
  ; f = sin(x+y) (xy + z^2)
  (define f (g* (gsin (g+ x y)) (g+ (g* x y) (g^ z 2))))
  (check-= (evaluate f)
           (* (sin (+ X* Y*)) (+ (* X* Y*) (expt Z* 2)))
           0.000001)
  (check-= (evaluate (deriv f 'x))
           (+ (* (cos (+ X* Y*)) (+ (* X* Y*) (expt Z* 2)))
              (* (sin (+ X* Y*)) Y*))
           0.000001)
)
