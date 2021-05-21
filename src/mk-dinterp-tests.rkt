#lang racket

(require rackunit
         "../faster-mk/mk.rkt"
         "./mk-dinterp.rkt"
         "../faster-mk/numbers.rkt")

(define (reco letx old e1)
  (== `((rec ,letx) . ,old) e1))

(define (simpo xvl e e1)
  (== `((simp ,xvl) . ,e) e1))

(check-equal? (run 1 (x) (assoco 'x '((x 5) (y 2)) x)) '((x 5)))
(check-equal? (run 1 (x) (assoco 'y '((x 5) (y 2)) x)) '((y 2)))
(check-equal? (run 1 (x) (assoco 'z '((x 5) (y 2)) x)) '(#f))
(check-equal? (run 1 (x) (assoco 'is-even? '([is-even? (lambda (n)
                                                        (if (zero? n)
                                                          #t
                                                          (is-odd? (- n 1))))]
                                            [is-odd? (lambda (n)
                                                       (if (zero? n)
                                                         #f
                                                         (is-even? (- n 1))))])
                                 x))
              '((is-even? (lambda (n) (if (zero? n) #t (is-odd? (- n 1)))))))
(check-equal? (run 1 (x) (assoco 'factorial '([is-even? (lambda (n)
                                                        (if (zero? n)
                                                          #t
                                                          (is-odd? (- n 1))))]
                                            [is-odd? (lambda (n)
                                                       (if (zero? n)
                                                         #f
                                                         (is-even? (- n 1))))])
                                 x))
              '(#f))
; lookupo tests
; simple env
(check-equal? (run 1 (x) (fresh (l e)
                           (zipo '(x y z) '(1 2 3) l)
                           (simpo l '() e)
                           (lookupo e 'z x)))
              '(3))
; simple inside simple
(check-equal? (run 1 (x) (fresh (l1 l2 e1 e2)
                           (zipo '(x y z) '(1 2 3) l1)
                           (zipo '(a b c) '(5 6 7) l2)
                           (simpo l1 '() e1)
                           (simpo l2 e1 e2)
                           (lookupo e2 'z x)))
              '(3))
(check-equal? (run 1 (x) (fresh (l1 l2 e1 e2)
                           (zipo '(x y z) '(1 2 3) l1)
                           (zipo '(a b c) '(5 6 7) l2)
                           (simpo l1 '() e1)
                           (simpo l2 e1 e2)
                           (lookupo e2 'b x)))
              '(6))
(check-equal? (run 1 (x) (fresh (l1 e1 e2)
                           (zipo '(x y z) '(1 2 3) l1)
                           (simpo l1 '() e1)
                           (exto '(a b c) '(5 6 7) e1 e2)
                           (lookupo e2 'b x)))
              '(6))
; searching for recursive binding in simple inside recursive env
(check-equal? (run 1 (x) (fresh (l1 e1 e2)
                           (zipo '(x y z) '(1 2 3) l1)
                           (simpo l1 '() e1)
                           (reco '(letrec ([factorial
                                             (lambda (x)
                                               (if (zero? x)
                                                 '(1)
                                                 (* x
                                                    (factorial (- x '(1))))))])
                                    (factorial '(1 0 1))) e1 e2)
                           (lookupo e2 'factorial x)))
              '((closr
                  ((lambda (x) (if (zero? x) '(1) (* x (factorial (- x '(1))))))
                   ((rec
                      (letrec ((factorial
                                 (lambda (x) (if (zero? x) '(1) (* x (factorial (- x '(1))))))))
                        (factorial '(1 0 1))))
                    (simp ((x 1) (y 2) (z 3))))))))
; searching for simple binding in simple inside recursive env
(check-equal? (run 1 (x) (fresh (l1 e1 e2)
                           (zipo '(x y z) '(1 2 3) l1)
                           (simpo l1 '() e1)
                           (reco '(letrec ([factorial
                                             (lambda (x)
                                               (if (zero? x)
                                                 '(1)
                                                 (* x
                                                    (factorial (- x '(1))))))])
                                    (factorial '(1 0 1))) e1 e2)
                           (lookupo e2 'y x)))
              '(2))
; searching for recursive binding in recursive inside simple env
(check-equal? (run 1 (x) (fresh (l1 e1 e2)
                           (reco '(letrec ([factorial
                                             (lambda (x)
                                               (if (zero? x)
                                                 '(1)
                                                 (* x
                                                    (factorial (- x '(1))))))])
                                    (factorial '(1 0 1))) '() e1)
                           (zipo '(x y z) '(1 2 3) l1)
                           (simpo l1 e1 e2)
                           (lookupo e2 'factorial x)))
              '((closr
                   ((lambda (x) (if (zero? x) '(1) (* x (factorial (- x '(1))))))
                    ((rec
                       (letrec ((factorial
                                  (lambda (x) (if (zero? x) '(1) (* x (factorial (- x '(1))))))))
                         (factorial '(1 0 1)))))))))
; searching for simple binding in recursive inside simple env
(check-equal? (run 1 (x) (fresh (l1 e1 e2)
                           (reco '(letrec ([factorial
                                             (lambda (x)
                                               (if (zero? x)
                                                 '(1)
                                                 (* x
                                                    (factorial (- x '(1))))))])
                                    (factorial '(1 0 1))) '() e1)
                           (zipo '(x y z) '(1 2 3) l1)
                           (simpo l1 e1 e2)
                           (lookupo e2 'z x)))
              '(3))
; searching for recursive binding in recursive inside recursive env
(check-equal? (run 1 (x) (fresh (l1 e1 e2)
                           (reco '(letrec ([factorial
                                             (lambda (x)
                                               (if (zero? x)
                                                 '(1)
                                                 (* x
                                                    (factorial (- x '(1))))))])
                                    (factorial '(1 0 1))) '() e1)
                           (reco '(letrec ([plus
                                             (lambda (x y)
                                               (if (zero? x)
                                                 y
                                                 (plus (- x '(1)) (+ y '(1)))))])
                                    (plus '(1 0 1) '(1 1 1))) e1 e2)
                           (lookupo e2 'plus x)))
              '((closr
                  ((lambda (x y) (if (zero? x) y (plus (- x '(1)) (+ y '(1)))))
                   ((rec
                      (letrec ((plus
                                 (lambda (x y) (if (zero? x) y (plus (- x '(1)) (+ y '(1)))))))
                        (plus '(1 0 1) '(1 1 1))))
                    (rec
                      (letrec ((factorial
                                 (lambda (x) (if (zero? x) '(1) (* x (factorial (- x '(1))))))))
                        (factorial '(1 0 1)))))))))
(check-equal? (run 1 (x) (letrec-bindso '())) '(_.0))
(check-equal? (run 1 (x) (letrec-bindso '([is-even? (lambda (n)
                                                      (if (zero? n)
                                                        #t
                                                        (is-odd? (- n 1))))]
                                          [is-odd? (lambda (n)
                                                     (if (zero? n)
                                                       #f
                                                       (is-even? (- n 1))))])))
              '(_.0))
; binding _should_ be a lambda expression
(check-equal? (run 1 (x) (letrec-bindso '([is-even? 5]
                                          [is-odd? (lambda (n)
                                                     (if (zero? n)
                                                       #f
                                                       (is-even? (- n 1))))])))
              '())
(check-equal? (run 1 (x) (letrec-bindso '([is-even? (lambda (n)
                                                      (if (zero? n)
                                                        #t
                                                        (is-odd? (- n 1))))])))
              '(_.0))
; evalo tests
(check-equal? (run 1 (x) (evalo '(letrec ([factorial
                                            (lambda (n)
                                              (if (zero? n)
                                                '(1)
                                                (* n (factorial (- n '(1))))))])
                                   (factorial '(1 0 1)))
                                init-env
                                x))
              '((0 0 0 1 1 1 1)))
(check-equal? (run 1 (x) (evalo '(letrec ([is-even? (lambda (n)
                                                      (if (zero? n)
                                                        #t
                                                        (is-odd? (- n '(1)))))]
                                          [is-odd? (lambda (n)
                                                     (if (zero? n)
                                                       #f
                                                       (is-even? (- n '(1)))))])
                                   (is-even? '(1 0 1)))
                                init-env
                                x))
              '(#f))
(check-equal? (run 1 (x) (evalo '(letrec ([is-even? (lambda (n)
                                                      (if (zero? n)
                                                        #t
                                                        (is-odd? (- n '(1)))))]
                                          [is-odd? (lambda (n)
                                                     (if (zero? n)
                                                       #f
                                                       (is-even? (- n '(1)))))])
                                   (is-even? '(0 1)))
                                init-env
                                x))
              '(#t))
(check-equal? (run 2 (x) (evalo `(letrec ([is-even? (lambda (n)
                                                      (if (zero? n)
                                                        #t
                                                        (is-odd? (- n '(1)))))]
                                          [is-odd? (lambda (n)
                                                     (if (zero? n)
                                                       #f
                                                       (is-even? (- n '(1)))))])
                                   (is-odd? ',x))
                                init-env
                                #t))
              '((1) (1 1)))
(check-equal? (run 1 (x) (evalo '(letrec ([my-append
                                            (lambda (l1 l2)
                                              (if (null? l1)
                                                l2
                                                (cons (car l1)
                                                      (my-append (cdr l1)
                                                                 l2))))])
                                   (my-append '(1 2 3) '(4 5 6))) init-env x))
              '((1 2 3 4 5 6)))
(check-equal? (run 1 (x) (evalo `(letrec ([my-append
                                            (lambda (l1 l2)
                                              (if (null? l1)
                                                ,x
                                                (cons (car l1)
                                                      (my-append (cdr l1)
                                                                 l2))))])
                                   (my-append '(1 2 3) '(4 5 6)))
                                init-env
                                '(1 2 3 4 5 6)))
              '(l2))
