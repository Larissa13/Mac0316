#lang plai-typed

#|
 | Tipos estáticos
 |#

(define-type TyExprC
  [numC (n : number)]
  [idC  (s : symbol)]
  [plusC (l : TyExprC) (r : TyExprC)]
  [multC (l : TyExprC) (r : TyExprC)]
  [lamC (arg : symbol) (argT : Type) (retT : Type) (body : TyExprC)] 
  [appC (fun : TyExprC) (arg : TyExprC)]
  [ifC   (condição : TyExprC) (sim : TyExprC) (não : TyExprC)]

  )

(define-type Type
  [numT]
  [funT (arg : Type) (ret : Type)])


; Environment para tipos
(define-type Type-Bind
  [Tbind (name : symbol) (type : Type)])

; A lista de associações é o environment
(define-type-alias TyEnv (listof Type-Bind))
(define mt-tyenv empty)        ; ente pronunciar "mt" em inglês e compare com "empty"
(define extend-ty-env cons)     ; sorte, cons faz exatamente o que queremos para estender o env

(define (Tlookup [for : symbol] [tenv : TyEnv]) : Type
  (cond
    [(empty? tenv) (error 'Tlookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
    [else (cond
            [(symbol=? for (Tbind-name (first tenv)))   ; achou!
             (Tbind-type (first tenv))]
            [else (Tlookup for (rest tenv))])]))        ; vê no resto



(define (tc [expr : TyExprC] [tenv : TyEnv]) : Type 
  (type-case TyExprC expr
    [numC (n) (numT)]
    [idC (n) (Tlookup n tenv)]
    [plusC (l r) (let ([lt (tc l tenv)]
                       [rt (tc r tenv)])
                   (if (and (equal? lt (numT))
                            (equal? rt (numT)))
                       (numT)
                       (error 'tc "+ not both numbers")))]
    [multC (l r) (let ([lt (tc l tenv)]
                       [rt (tc r tenv)])
                   (if (and (equal? lt (numT))
                            (equal? rt (numT)))
                       (numT)
                       (error 'tc "* not both numbers")))]
    [appC (f a) (let ([ft (tc f tenv)]
                      [at (tc a tenv)])
                  (cond
                    [(not (funT? ft))
                     (error 'tc "not a function")]
                    [(not (equal? (funT-arg ft) at))
                     (error 'tc "app arg mismatch")]
                    [else (funT-ret ft)]))]
    [lamC (a argT retT b)
          (if (equal? (tc b (extend-ty-env (Tbind a argT) tenv)) retT)
              (funT argT retT)
              (error 'tc "lam type mismatch"))]
    [ifC (c s n) (let ([ct (tc c tenv)]
					   [st (tc s tenv)]
					   [nt (tc n tenv)])
				   (cond
					[(not (numT? ct))
					 (error 'tc "Condition must be numeric")]
					[(not (equal? st nt))
					 (error 'tc "Both branches must be of the same type")]
					[else st])
				   )]
    )
  )




; inclui funções
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)] 
  [lamS    (arg : symbol) (argT : Type) (retT : Type) (body : ExprS)] ; muda de acordo
  [appS    (fun : ExprS) (arg : ExprS)] 
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS	   (c : ExprS) (s : ExprS) (n : ExprS)]
  )


(define (desugar [as : ExprS]) : TyExprC  
  (type-case ExprS as
    [numS    (n) (numC n)]
    [idS     (s) (idC s)]
    [lamS     (a at rt b)  (lamC a at rt (desugar b))] ; idem
    [appS    (fun arg) (appC (desugar fun) (desugar arg))] 
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    ))

(tc (ifC (numC 0) (lamC 'a (numT) (numT) (numC 6)) (lamC 'a (numT) (numT) (plusC (numC 3) (numC 3)))) empty)
;(tc (ifC (lamC 'a (numT) (numT) (numC 6)) (numC 3) (numC 43)) empty);
;(tc (ifC (numC 0) (lamC 'a (numT) (numT) (numC 6)) (numC 3)) empty)
