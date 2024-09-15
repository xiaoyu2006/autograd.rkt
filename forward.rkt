#lang racket/base

(require racket/match)
(require "common.rkt")

;; Partial derivatives of $f with regard to $x, in expression again
(define/cached (deriv f x)
  (match f
    ; dx/dx = 1 ; dy/dx = 0
    [(var name _val)
     (if (eq? name x) 1 0)]
    ; dC/dx = 0
    [n #:when (number? n) 0]
    ; d(u+v)/dx = du/dx + dv/dx
    [(list '+ u v)                                  
     (list '+ (deriv u x) (deriv v x))]
    ; d(-u)/dx = -du/dx
    [(list 'n u)
     (list '- (deriv u x))]
    ; d(uv)/dx = du/dx v + u dv/dx
    [(list '* u v)
     `(+ (* ,(deriv u x) ,v)
         (* ,u ,(deriv v x)))]
    ; d(u^-1)/dx = -u^-2 du/dx
    [(list 'i u)
     `(* -1 (/ 1 (expt ,u 2)) ,(deriv u x))]
    ; d(u^C)/dx = C u^(C-1) du/dx
    [(list 'e u n)
      #:when (number? n)
     `(* ,n (* (e ,u ,(- n 1)) ,(deriv u x)))]
    ; More special cases...
    [_catchall (error 'deriv "not implemented")]
    ))

;;; Put together

(define/var x 1.0)
(define/var y 2.0)

; f1 = x * ( x + y^2 )
(define sym-f1 `(* ,x (+ ,x (e ,y 2))))

(define sym-f1-dx (deriv sym-f1 'x))
(define sym-f1-dy (deriv sym-f1 'y))
(displayln (evaluate sym-f1))
(displayln (evaluate sym-f1-dx))
(displayln (evaluate sym-f1-dy))
