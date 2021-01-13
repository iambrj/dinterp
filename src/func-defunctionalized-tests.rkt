#lang racket

(require rackunit)
(require "./func-defunctionalized-meta.rkt")

(check-true (const? 5))
(check-true (const? "foo"))
(check-true (const? #t))
(check-false (const? 'x))

(check-true (var? 'x))
(check-false (var? 5))

(check-true (appl? '((lambda (x) x) 5)))
(check-true (appl? '(f x)))
(check-false (appl? '(1)))

(check-equal? (opr '((lambda (x) x) 5)) '(lambda (x) x))
(check-equal? (opr '(5 5)) 5)

(check-equal? (opnd '((lambda (x) x) 5)) 5)
(check-equal? (opnd '(5 5)) 5)

(check-true (lambda? '(lambda (x) x)))
(check-false (lambda? 5))

(check-equal? (body '(lambda (x) x)) 'x)
(check-equal? (body '(lambda (x) 5)) 5)
(check-equal? (body '(lambda (x) (* x x))) '(* x x))

(check-equal? (fp '(lambda (x) x)) 'x)
(check-equal? (fp '(lambda (z) (lambda (y) y))) 'z)

(check-true (cond? '(if #t 3 4)))
(check-false (cond? '(if)))
(check-false (cond? '(if #t)))
(check-false (cond? '(if #t 3)))

(check-equal? (prem '(if #t 3 4)) #t)
(check-equal? (prem '(if (lambda (x) (x #t)) 3 4)) '(lambda (x) (x #t)))

(check-equal? (conc '(if #t 3 4)) 3)
(check-equal? (conc '(if (lambda (x) (x #t)) (lambda (y) (y y)) 5)) '(lambda (y) (y y)))

(check-equal? (altr '(if #t 3 4)) 4)
(check-equal? (altr '(if (lambda (x) (x #t)) 5 (lambda (y) (y y)))) '(lambda (y) (y y)))

(check-true (letrec? '(letrec ([x (lambda (z) (z z))]) (x (lambda (y) (y y))))))
(check-false (letrec? '(letrec 5))'(letrec ([x (lambda (z) (z z))]) (x (lambda (y) (y y)))))
(check-false (letrec? '(letrec ([x (lambda (z) (z z))]))))
(check-false (letrec? '(letrec (x (lambda (y) (y y))))))
(check-false (letrec? '(letrec [x (lambda (z) (z z))] (x (lambda (y) (y y))))))

(check-equal? (dvar '(letrec ([x (lambda (z) (z z))]) (x (lambda (y) (y y))))) 'x)
(check-equal? (dvar '(letrec ([id (lambda (z) z)]) (id 5))) 'id)

(check-equal? (dexp '(letrec ([x (lambda (z) (z z))]) (x (lambda (y) (y y))))) '(lambda (z) (z z)))
(check-equal? (dexp '(letrec ([id (lambda (z) z)]) (id 5))) '(lambda (z) z))

(check-equal? (body '(letrec ([x (lambda (z) (z z))]) (x (lambda (y) (y y))))) '(x (lambda (y) (y y))))
(check-equal? (body '(letrec ([id (lambda (z) z)]) (id 5))) '(id 5))

(check-equal? (meta-eval '((lambda (x) x) 5)
                         (lambda (x) (error "Missing binding : " x)))
              5)
(check-equal? (meta-eval '(letrec ([double (lambda (x)
                                                     (if (zero? x)
                                                       0
                                                       ((+ 2) (double ((- x) 1)))))])
                                    (double 3))
                         init-env)
                    6)
(check-equal? (meta-eval '(letrec ([factorial (lambda (x)
                                                     (if (zero? x)
                                                       1
                                                       ((* x) (factorial ((- x) 1)))))])
                                    (factorial 5))
                         init-env)
                    120)
