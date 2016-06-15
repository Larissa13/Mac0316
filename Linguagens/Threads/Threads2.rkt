#lang plai-typed
(define-syntax let/cc
  (syntax-rules ()
    [(let/cc k b)
     (call/cc (lambda (k) b))]))

(define-type ThreadStatus 
  [Tsuspended]
  [Tdone])


(define-syntax thread-1
  (syntax-rules ()
    [(thread (yielder) b ...)
     (letrec ([thread-resumer (lambda (_)
                                (begin b ...
                                       (finisher)))]
              [finisher (lambda () (error 'finisher "nothing here"))]
              [yielder (lambda () (error 'yielder "nothing here"))])
       (lambda (sched-k)
         (begin
           (set! finisher
                 (lambda ()
                   (let/cc thread-k
                     (sched-k (Tdone)))))
           (set! yielder
                 (lambda ()
                   (let/cc thread-k
                     (begin
                       (set! thread-resumer thread-k)
                       (sched-k (Tsuspended))))))
           (thread-resumer 'tres))))]))

(define (scheduler-loop-1 threads)
  (cond
    [(empty? threads) 'done]
    [(cons? threads)
     (type-case ThreadStatus 
       (let/cc after-thread ((first threads) after-thread))
       [Tsuspended ()  (scheduler-loop-1 (append (rest threads)
                                                 (list (first threads))))]
       [Tdone () (scheduler-loop-1 (rest threads))])]))

(define d display) 

(scheduler-loop-1
 (list
  (thread-1 (y) (d "t1-1 ") (y) (d "t1-2 ") (y) (d "t1-3 "))
  (thread-1 (y) (d "t2-1 ") (y) (d "t2-2 ") (y) (d "t2-3 "))
  (thread-1 (y) (d "t3-1 ") (y) (d "t3-2 ") (y) (d "t3-3 "))))
