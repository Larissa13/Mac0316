#lang plai-typed

(define-syntax (cps e)
  (syntax-case e (generator with rec lam let/cc cnd seq set quote)
    
    ; generator
    [(_ (generator (yield) (v) b)) ; yielder, valor, corpo
     (and (identifier? #'v) (identifier? #'yield))
     #'(lambda (k)
         (k (letrec (
                     [where-to-go (lambda (v) (error 'where-to-go "nothing"))]
                     [resumer (lambda (v)
                                ((cps b) (lambda (v)
                                           (error 'generator "fell through"))))]
                     [yield (lambda (v gen-k)
                              (begin
                                (set! resumer gen-k)
                                (where-to-go v)))])
              (lambda (v dyn-k)
                (begin
                  (set! where-to-go dyn-k)
                  (resumer v)))
              )))]
    
    
    ; begin para 2 expressões
    [(_ (seq e1 e2))
     #'(lambda (k)
         ((cps e1) (lambda (_)
                     ((cps e2) k))))]    
    
    ; set é o mesmo que set!
    [(_ (set v e))
     #'(lambda (k)
         ((cps e) (lambda (ev)
                    (k (set! v ev)))))]
    
    ; with é como o ret
    [(_ (with (v e) b))
     #'(cps ((lam (v)  b) e))]
    
    ; rec é como letrec
    [(_ (rec (v f) b))
     #'(cps (with (v (lam (arg) (error 'dummy "nothing")))
                  (seq
                   (set v f)
                   b)))]
    ; quote
    [(_ 'e)
     #'(lambda (k) (k 'e))]
    
    
    ; lambda
    [(_ (lam (a1 a2) b))
     (and (identifier? #'a1) (identifier? #'a2))
     #'(lambda (k)
         (k (lambda (a1 a2  dyn-k)
              ((cps b) dyn-k))))]
    
    ; lambda
    [(_ (lam (a) b))
     (identifier? #'a)
     #'(lambda (k)
         (k (lambda (a dyn-k)
              ((cps b) dyn-k))))] 
    
    ; lambda
    [(_ (lam () b))
     #'(lambda (k)
         (k (lambda  (dyn-k)
              ((cps b) dyn-k))))] 
 
    ; let/cc
    [(_ (let/cc kont b))
     (identifier? #'kont)
     #'(lambda (k)
         (let ([kont (lambda (v dyn-k)
                       (k v))])
           ((cps b) k)))]
        
    ; cnd é o if
    [(_ (cnd tst thn els))
     #'(lambda (k)
         ((cps tst) (lambda (tstv)
                      (if tstv
                          ((cps thn) k)
                          ((cps els) k)))))]
    
    ; com dois
    [(_ (f a b))
     #'(lambda (k)
         ((cps a) (lambda (av)
                    ((cps b) (lambda (bv)
                               (k (f av bv )))))))]
    
    
    ; chamada com 1 argumento
    [(_ (f a))
     #'(lambda (k)
         ((cps f) (lambda (fv)
                    ((cps a) (lambda (av)
                               (fv av k))))))]
    ; símbolo
    [(_ atomic)
     #'(lambda (k)
         (k atomic))]
    

    ))

(define (run c) (c identity)) ; para poder testar
;(run (cps (display (+ (read-number "First") (read-number "Second")))))

(cps (generator (y) (a) (+ a 5)))
;

(run (cps (with (ns (generator (yield) (from)
                               (rec (f (lam (n) (seq
                                                 (yield n)
                                                 (f (+ n 1))))
                                       )
                                 (f from))))
                (seq (ns 0) (ns 42)))))

(test (run (cps (let/cc esc 3))) 3)
;(test (run (cps (let/cc esc (esc 3)))) 3)
;(test (run (cps (+ 1 (let/cc esc (esc 3))))) 4)
;(test (run (cps (let/cc esc (+ 2 (esc 3))))) 3)
;(test (run (cps (+ 1 (let/cc esc (+ 2 (esc 3)))))) 4)

