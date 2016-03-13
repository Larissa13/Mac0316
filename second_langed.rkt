#lang plai-typed
(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons) ;cons é um par (car.cdr)

(define-type FunDefC
  [fdc (name : symbol) (arg : symbol) (body : ExprC)]
)

(define-type ExprC
  [numC  (n : number)]
  [idC (id : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC  (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC   (cond : ExprC) (sim : ExprC) (não : ExprC)]
  [ifgC (conda : ExprC) (condb : ExprC) (sim : ExprC) (não : ExprC)]
)

(define-type ExprS
  [numS  (n : number)]
  [idS (id : symbol)]
  [appS (fun : symbol) (arg : ExprS)]
  [plusS  (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [ifS   (cond : ExprS) (sim : ExprS) (não : ExprS)]
  [ifgS (conda : ExprS) (condb : ExprS) (sim : ExprS) (não : ExprS)]
  [iflS (conda : ExprS) (condb : ExprS) (sim : ExprS) (não : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
)

(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(ifg) (ifgS (parse (second sl)) (parse (third sl)) (parse (fourth sl)) (parse (list-ref sl 4)))]
         [(ifl) (ifgS (parse (second sl)) (parse (third sl)) (parse (fourth sl)) (parse (list-ref sl 4)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r ))]
    [idS (id) (idC id)]
    [appS (f a) (appC f (desugar a))]
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [ifgS (ca cb s n) (ifgC (desugar ca) (desugar cb) (desugar s) (desugar n))]
    [iflS (ca cb s n) (ifgC (desugar cb) (desugar ca) (desugar s) (desugar n))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "referência para função não definida")]
    [(cons? fds) (cond
                   [(equal? n (fdc-name (first fds))) (first fds)] ;achou a função
                   [else (get-fundef n (rest fds))])]))

( define ( lookup [ for : symbol ] [ env : Env ]) : number
   ( cond
      [( empty? env ) ( error 'lookup " name not found " ) ]
      [ else ( cond
                [( symbol=? for ( bind-name ( first env ) ) )
                 ( bind-val ( first env ) ) ]
                [ else ( lookup for ( rest env ) ) ]) ]) )

(define (interp [a : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (c) (lookup c env)]
    [ appC ( f a )
           ( local ([ define fd ( get-fundef f fds ) ])
              ( interp (fdc-body fd)
                       ( extend-env
                         ( bind ( fdc-arg fd ) ( interp a env fds ) )
                         env )
                       fds ) ) ]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [ifC (c s n) (if (zero? (interp c env fds)) (interp n env fds) (interp s env fds))]
    [ifgC (ca cb s n) (if (> (interp ca env fds) (interp cb env fds)) (interp s env fds) (interp n env fds))]
     )
 )

