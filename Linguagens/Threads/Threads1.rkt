#lang plai-typed
(define-syntax let/cc
  (syntax-rules ()
    [(let/cc k b)
     (call/cc (lambda (k) b))]))


(define (scheduler-loop-0 threads)
  (cond
    [(empty? threads) 'done]
    [(cons? threads)
     (begin
       (let/cc after-thread ((first threads) after-thread))
       (scheduler-loop-0 (append (rest threads)
                                 (list (first threads)))))]))


(define-syntax thread-0
  (syntax-rules ()
    [(thread (yielder) b ...)
     (letrec ([thread-resumer (lambda (_)
                                (begin b ...))]
              [yielder (lambda () (error 'yielder "nothing here"))])
       (lambda (sched-k)
         (begin
           (set! yielder
                 (lambda ()
                   (let/cc thread-k
                     (begin
                       (set! thread-resumer thread-k)
                       (sched-k 'dummy)))))
           (thread-resumer 'tres))))]))


(define d display) 

(scheduler-loop-0
 (list
  (thread-0 (y) (d "t1-1 ") (y) (d "t1-2 ") (y) (d "t1-3 "))
  (thread-0 (y) (d "t2-1 ") (y) (d "t2-2 ") (y) (d "t2-3 "))
  (thread-0 (y) (d "t3-1 ") (y) (d "t3-2 ") (y) (d "t3-3 "))))
