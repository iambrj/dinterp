#lang racket

(require "../faster-mk/mk.rkt"
         "../faster-mk/numbers.rkt")

(provide evalo
         apply-closro
         apply-primo
         const?o
         assoco
         zipo
         lookupo
         exto
         init-env
         letrec-bindso)

(define (zipo l1 l2 l)
  (conde
    [(== '() l1) (== '() l)]
    [(fresh (a1 a2 d1 d2 r)
       (== `(,a1 . ,d1) l1)
       (== `(,a2 . ,d2) l2)
       (== `((,a1 ,a2) . ,r) l)
       (zipo d1 d2 r))]))

(define (assoco x l p)
  (conde
    [(== '() l) (== #f p)]
    [(fresh (z v d)
       (== `((,z ,v) . ,d) l)
       (conde
         [(== z x) (== `(,z ,v) p)]
         [(=/= z x) (assoco x d p)]))]))

(define (membero x l)
  (fresh (a d)
    (== `(,a . ,d) l)
    (conde
      [(== x a)]
      [(=/= x a) (membero x d)])))

(define (imapo g l)
  (conde
    [(== '() l)]
    [(fresh (a d)
       (== `(,a . ,d) l)
       (g a)
       (imapo g d))]))

(define (mapo ge l v)
  (conde
    [(== '() l) (== '() v)]
    [(fresh (a d a-v d-v)
       (== `(,a . ,d) l)
       (== `(,a-v . ,d-v) v)
       (ge a a-v)
       (mapo ge d d-v))]))

(define (exto x* v* env env1)
  (fresh (l)
    (== `((simp ,l) . ,env) env1)
    (zipo x* v* l)))

(define (const?o x)
  (conde
    [(numbero x)]
    [(stringo x)]
    [(conde
       [(== #t x)]
       [(== #f x)])]))

; TODO : think about better name
(define (letrec-bindso binds)
  (conde
    [(== '() binds)]
    ; TODO : add duplicate check for both LHS vars and lambda args
    [(fresh (bind-var args body d)
       (== `((,bind-var (lambda ,args ,body)) . ,d) binds)
       (symbolo bind-var)
       (letrec-bindso d))]))

; TODO : see if performance gain by inlining assoco
; split assoco into two explicit cases for present and absent
(define (lookupo e x t)
  (conde
    [(fresh (v p xvl old)
       (== `((simp ,xvl) . ,old) e)
       (assoco x xvl p)
       (conde
         [(== `(,x ,v) p) (== v t)]
         [(== #f p) (lookupo old x t)]))]
    [(fresh (letx old binds body p lam)
       (== `((rec ,letx) . ,old) e)
       (== `(letrec ,binds ,body) letx)
       (assoco x binds p)
       (conde
         [(== `(,x ,lam) p)
          (== `(closr (,lam ,e)) t)]
         [(== #f p) (lookupo old x t)]))]))

(define (apply-closro c args val)
  (conde 
    ; NOTE : ordering biases search towards first clause
    [(fresh (env body ext-env params)
       (== `(closr ((lambda ,params ,body) ,env)) c)
       (exto params args env ext-env)
       (evalo body ext-env val))]
    [(fresh (op)
       (== `(prim ,op) c)
       (apply-primo op args val))]))

(define (not-in-envo x env)
  (conde
    [(== '() env)]
    [(fresh (xvl old)
       (== `((simp ,xvl) . ,old) env)
       ; TODO : call not membero instead, assoco more expensive
       (assoco x xvl #f)
       (not-in-envo x old))]
    [(fresh (d binds body)
       (== `((rec (letrec ,binds ,body)) . ,d) env)
       ; TODO : call not membero instead, assoco more expensive
       (assoco x binds #f)
       (not-in-envo x d))]))

; TODO : omit simp, only use tag for recursive
(define init-env '((simp
                     ((cons (prim cons))
                      (car (prim car))
                      (cdr (prim cdr))
                      (null? (prim null?))
                      (* (prim *))
                      (+ (prim +))
                      (- (prim -))
                      (add1 (prim add1))
                      (sub1 (prim sub1))
                      (zero? (prim zero?))))))

(define (apply-primo op args r)
  (conde
    [(== 'cons op) (fresh (x y) (== `(,x ,y) args) (== `(,x . ,y) r))]
    [(== 'car op) (fresh (d) (== `((,r . ,d)) args))]
    [(== 'cdr op) (fresh (a) (== `((,a . ,r)) args))]
    [(== 'null? op)
     (conde [(== '(()) args) (== #t r)] [(=/= '(()) args) (== #f r)])]
    [(== '+ op) (fresh (x y) (== `(,x ,y) args) (pluso x y r))]
    [(== '* op) (fresh (x y) (== `(,x ,y) args) (*o x y r))]
    [(== '- op) (fresh (x y) (== `(,x ,y) args) (pluso r y x))]
    [(== 'add1 op) (fresh (x) (== `(,x) args) (pluso x '(1) r))]
    [(== 'sub1 op) (fresh (x) (== `(,x) args) (pluso r '(1) x))]
    [(== 'zero? op)
     ; NOTE : Nice example of being unable to talk about failure if all we had
     ; was zeroo
     (fresh (x)
       (== `(,x) args)
       (conde
         [(== '() x) (== #t r)]
         [(=/= '() x) (== #f r)]))]))

(define (evalo expr env val)
  (conde
    [(const?o expr) (== expr val)]
    [(symbolo expr) (lookupo env expr val)]
    [(fresh (args body)
       (== `(lambda ,args ,body) expr)
       (== `(closr (,expr ,env)) val)
       (not-in-envo 'lambda env)
       ; TODO : add duplicate argument name check
       ; XXX : can cause infinite loops on synthesis!
       (imapo symbolo args))]
    [(== `(quote ,val) expr)
     (not-in-envo 'quote env)
     (absento 'closr val)
     (absento 'prim val)]
    [(fresh (prem conc altr prem-val)
       (== `(if ,prem ,conc ,altr) expr)
       (not-in-envo 'if env)
       (evalo prem env prem-val)
       (conde
         [(=/= #f prem-val) (evalo conc env val)]
         [(== #f prem-val) (evalo altr env val)]))]
    [(fresh (rator rands rator-val rand-vals)
       (== `(,rator . ,rands) expr)
       ; TODO : add closr check on rator-val to fail faster
       ; XXX : wouldn't that be expensive, since checking envs recursively is
       ; expensive?
       (evalo rator env rator-val)
       (mapo (lambda (x v)
               (evalo x env v))
             rands
             rand-vals)
       (apply-closro rator-val rand-vals val))]
    [(not-in-envo 'letrec env)
     (fresh (binds body rec-env)
       (== `(letrec ,binds ,body) expr)
       (== `((rec ,expr) . ,env) rec-env)
       (evalo body rec-env val))]
    [(fresh (e*)
       ; FIXME : should be primitive, not special form
       ; XXX : How to ensure the absento works if it were made a primitive?
       (== `(list . ,e*) expr)
       (absento 'closr e*)
       (absento 'prim e*)
       (not-in-envo 'list env)
       (mapo (lambda (x v)
                      (evalo x env v))
                    e*
                    val))]))
