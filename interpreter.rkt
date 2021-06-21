#lang racket

(struct closure (fun env) #:transparent)

(define (interpret current-prog [env '()])
  (match current-prog
    [(? number? num) (if (exact? num)
                         num
                         (error "inexact number:" num))]
    [(? symbol? var) (letrec ([find-var (lambda (var env)
                                          (cond
                                            [(empty? env) #f]
                                            [else (if (eqv? var (car (car env)))
                                                      (cdr (car env))
                                                      (find-var var (cdr env)))]))]
                              [val (find-var var env)])
                       (if (not val)
                           (error "undefined variable:" var)
                           val))]
    [`(lambda ,var ,prog) (closure current-prog env)]
    [`(call ,prog1 ,prog2) (let ([val1 (interpret prog1 env)])
                             (match val1
                               [(closure `(lambda ,var ,prog) saved-env)
                                (let ([val2 (interpret prog2 env)])
                                  (interpret prog (cons (cons 'self val1) (cons (cons var val2) saved-env))))]
                               [val (error "calling a non-closure value:" val)]))]
    [`(let ,var = ,prog1 in ,prog2) (let ([val (interpret prog1 env)])
                                      (interpret prog2 (cons (cons var val) env)))]
    [`(if ,prog1 then ,prog2 else ,prog3) (let ([val (interpret prog1 env)])
                                            (if (not (= val 0))
                                                (interpret prog2 env)
                                                (interpret prog3 env)))]
    [`(num? ,prog) (let ([val (interpret prog env)])
                     (if (exact? val) 1 0))]
    [`(closure? ,prog) (let ([val (interpret prog env)])
                         (if (closure? val) 1 0))]
    [`(,arith-op ,prog1 ,prog2) (let ([val1 (interpret prog1 env)]
                                      [val2 (interpret prog2 env)])
                                  (if (and (exact? val1) (exact? val2))
                                      (match arith-op
                                        ['+ (+ val1 val2)]
                                        ['- (- val1 val2)]
                                        ['* (* val1 val2)]
                                        ['/ (/ val1 val2)]
                                        ['== (if (= val1 val2) 1 0)]
                                        ['!= (if (not (= val1 val2)) 1 0)]
                                        ['< (if (< val1 val2) 1 0)]
                                        ['<= (if (<= val1 val2) 1 0)]
                                        ['> (if (> val1 val2) 1 0)]
                                        ['>= (if (>= val1 val2) 1 0)]
                                        [op (error "unknown arithmetic operator:" op)])
                                      (error "not all arithmetic operands are numbers:" val1 val2)))]))