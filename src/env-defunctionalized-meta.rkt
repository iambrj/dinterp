#lang racket

; Does not use higher order functions for envs, i.e. envs are represented using
; records

(provide const?
         var?
         appl?
         opr
         opnd
         lambda?
         body
         fp
         cond?
         prem
         conc
         altr
         letrec?
         dvar
         dexp
         meta-eval
         init-env)

(struct simp (x v e))
(struct rec (letx old))

(define (get e x)
  (match e
    [(simp z v e) (if (eq? x z) v (get e x))]
    [(rec r old) (if (eq? x (dvar r)) (evlambda (dexp r) e) (get old x))]))

(define (ext x v e)
  (simp x v e))

(define (const? x)
  (or (number? x)
      (string? x)
      (boolean? x)))

(define var? symbol?)

(define (appl? expr)
  (match expr
    [`(,opr ,opnd) #t]
    [else #f]))

(define (opr expr)
  (match expr
    [`(,opr ,opnd) opr]
    [else #f]))

(define (opnd expr)
  (match expr
    [`(,opr ,opnd) opnd]
    [else #f]))

(define (lambda? expr)
  (match expr
    [`(lambda (,arg) ,body) #t]
    [else #f]))

(define (body expr)
  (match expr
    [`(lambda (,arg) ,body) body]
    [`(letrec ([,dvar ,dexp]) ,body) body]
    [else #f]))

(define (fp expr)
  (match expr
    [`(lambda (,arg) ,body) arg]
    [else #f]))

(define (evlambda l e)
  (lambda (a)
    (meta-eval (body l) (ext (fp l) a e))))

(define (cond? r)
  (match r
    [`(if ,p ,c ,a) #t]
    [else #f]))

(define (prem r)
  (match r
    [`(if ,p ,c ,a) p]
    [else #f]))

(define (conc r)
  (match r
    [`(if ,p ,c ,a) c]
    [else #f]))

(define (altr r)
  (match r
    [`(if ,p ,c ,a) a]
    [else #f]))

(define (letrec? r)
  (match r
    [`(letrec ([,dvar ,dexp]) ,body) #t]
    [else #f]))

(define (dvar r)
  (match r
    [`(letrec ([,dvar ,dexp]) ,body) dvar]
    [else #f]))

(define (dexp r)
  (match r
    [`(letrec ([,dvar ,dexp]) ,body) dexp]
    [else #f]))

(define init-env
  (foldr (lambda (b v e)
           (ext b v e))
         (lambda (x) (error "Missing binding : " x))
         '(+ * - / zero?)
         (list
           (lambda (u)
             (lambda (v)
               (+ u v)))
           (lambda (u)
             (lambda (v)
               (* u v)))
           (lambda (u)
             (lambda (v)
               (- u v)))
           (lambda (u)
             (lambda (v)
               (/ u v)))
           (lambda (u)
             (zero? u)))))

(define (meta-eval r e)
  (cond
    [(const? r) r]
    [(var? r) (get e r)]
    [(appl? r) ((meta-eval (opr r) e) (meta-eval (opnd r) e))]
    [(lambda? r) (evlambda r e)]
    [(cond? r) (if (meta-eval (prem r) e)
                 (meta-eval (conc r) e)
                 (meta-eval (altr r) e))]
    [(letrec? r) (meta-eval (body r) (rec r e))]
    [else (error "meta-eval : pattern not found")]))
