#lang plai-typed
(define-syntax let/cc
  (syntax-rules ()
    [(let/cc k b)
     (call/cc (lambda (k) b))]))


(define-syntax (generator e)
   (syntax-case e ()
    [(generator (yield) (v) b)
      #'(let ([where-to-go (lambda (v) (error 'where-to-go "nothing"))])
           (letrec ([resumer (lambda (v)
                              (begin b
                                 (error 'generator "fell through")))]
                    [yield (lambda (v)
                             (let/cc gen-k
                               (begin
                                 (set! resumer gen-k)
                                 (where-to-go v))))])
          (lambda (v)
             (let/cc dyn-k
               (begin
                  (set! where-to-go dyn-k)
                  (resumer v))))))]))


(define g1 (generator (yield) (v)
               (letrec ([loop (lambda (n)
                                (begin
                                  (yield n)
                                  (loop (+ n 1))))])
                (loop v))))

(define g2 (generator (yield) (v)
               (letrec ([loop (lambda (n)
                                  (loop (+ (yield n) n)))])
                (loop v))))

(g1 12)
(g1 3)
(g1 42)
(g1 1729)

(display "\nNovos testes\n")

(g2 1000)
(g2 700)
(g2 20)
(g2 9)

