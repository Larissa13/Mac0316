#lang plai-typed

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type Binding
      [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)        
(define extend-env cons)     

(define-type ExprC
  [numC  (n : number)]
  [idC (id : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [plusC  (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC   (cond : ExprC) (sim : ExprC) (não : ExprC)]
  [ifgC (conda : ExprC) (condb : ExprC) (sim : ExprC) (não : ExprC)]
)

(define-type ExprS
  [numS  (n : number)]
  [idS (id : symbol)]
  [appS (fun : ExprS) (arg : ExprS)]
  [lamS (arg : symbol) (body : ExprS)]
  [plusS  (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [ifS   (cond : ExprS) (sim : ExprS) (não : ExprS)]
  [ifgS (conda : ExprS) (condb : ExprS) (sim : ExprS) (não : ExprS)]
  [iflS (conda : ExprS) (condb : ExprS) (sim : ExprS) (não : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
)
;novos operadores aritméticos
(define (num+ [l : Value] [r : Value]) : Value
  (cond [(and (numV? l) (numV? r))
         (numV (+ (numV-n l) (numV-n r)))]
        [else (error 'num+ "Um dos argumentos não é número")]
        ))

(define (num* [l : Value] [r : Value]) : Value
  (cond [(and (numV? l) (numV? r))
         (numV (* (numV-n l) (numV-n r)))]
        [else (error 'num* "Um dos argumentos não é número")]
        ))

(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] 
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] 
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
    [appS (f a) (appC (desugar f) (desugar a))]
    [lamS (a b) (lamC a (desugar b))]
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [ifgS (ca cb s n) (ifgC (desugar ca) (desugar cb) (desugar s) (desugar n))]
    [iflS (ca cb s n) (ifgC (desugar cb) (desugar ca) (desugar s) (desugar n))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
))


( define ( lookup [ for : symbol ] [ env : Env ]) : Value
   ( cond
      [( empty? env ) ( error 'lookup (string-append (symbol->string for) " não foi encontrado") ) ]
      [ else ( cond
                [( symbol=? for ( bind-name ( first env ) ) )
                 ( bind-val ( first env ) ) ]
                [ else ( lookup for ( rest env ) ) ]) ]) )

(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (c) (lookup c env)]
    [lamC (a b) (closV a b env)]
    [appC (f a)
          (local ([define f-value (interp f env)]) 
            (interp (closV-body f-value)
                    (extend-env 
                        (bind (closV-arg f-value) (interp a env))
                        (closV-env f-value)
                    )))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [ifC (c s n) (if (zero? (numV-n(interp c env ))) (interp n env) (interp s env))]
    [ifgC (ca cb s n) (if (> (numV-n (interp ca env)) (numV-n (interp cb env))) (interp s env) (interp n env))]
     )
 )

(interp(desugar(parse '( call ( func f ( call f 32) ) ( func x (* x x ) ) )))mt-env)
(interp(desugar(parse '(call (func f (call (func x (call f 10))2)) (func x (+ x x))))) mt-env)
(interp(desugar(parse '(call (func f (+ (call f 3) (call f 5))) (func x (+ x x)))))  mt-env)
(interp(desugar(parse '(call (func f (call f 10)) (func x (+ x x)))))  mt-env)
(interp(desugar(parse '(call (func x (+ x x)) 10))) mt-env)
(interp(desugar(parse '(call (func f (func x (call f 10))) (+ x y)))) mt-env)