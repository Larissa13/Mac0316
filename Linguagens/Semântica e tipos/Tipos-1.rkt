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
    ))

;
;; tipo de retorno
;(define-type Value
;  [numV  (n : number)]
;  [closV (arg : symbol) (body : TyExprC) (env : Env)])
;
;; símbolos devem se associar ao número (ou a Value?)
;(define-type Binding
;  [bind (name : symbol) (val : Value)])
;
;; A lista de associações é o environment
;(define-type-alias Env (listof Binding))
;(define mt-env empty)        ; ente pronunciar "mt" em inglês e compare com "empty"
;(define extend-env cons)     ; sorte, cons faz exatamente o que queremos para estender o env
;
;; novos operadores
;(define (num+ [l : Value] [r : Value]) : Value
;  (cond
;    [(and (numV? l) (numV? r))
;     (numV (+ (numV-n l) (numV-n r)))]
;    [else
;     (error 'num+ "Um dos argumentos não é número")]))
;
;(define (num* [l : Value] [r : Value]) : Value
;  (cond
;    [(and (numV? l) (numV? r))
;     (numV (* (numV-n l) (numV-n r)))]
;    [else
;     (error 'num* "Um dos argumentos não é número")]))
;
;; trata agora lamC e appC
;(define (interp [a : TyExprC] [env : Env]) : Value
;  (type-case TyExprC a
;    [numC (n) (numV n)] 
;    [idC (n) (lookup n env)]
;    [lamC (a b) (closV a b env)] ; definição de função captura o environment
;    
;    [appC (f a)
;          (local ([define f-value (interp f env)]) ; f-value descreve melhor a ideia
;            (interp (closV-body f-value)
;                    (extend-env 
;                     (bind (closV-arg f-value) (interp a env))
;                     (closV-env f-value) ; não mais mt-env
;                     )))]
;    [plusC (l r) (num+ (interp l env) (interp r env))]
;    [multC (l r) (num* (interp l env) (interp r env))]
;    [ifC (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]
;    ))
;
;; lookup também muda o tipo de retorno
;(define (lookup [for : symbol] [env : Env]) : Value
;  (cond
;    [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
;    [else (cond
;            [(symbol=? for (bind-name (first env)))   ; achou!
;             (bind-val (first env))]
;            [else (lookup for (rest env))])]))        ; vê no resto

; o parser permite definir funções...
;(define (parse [s : s-expression]) : ExprS
;  (cond
;    [(s-exp-number? s) (numS (s-exp->number s))]
;    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
;    [(s-exp-list? s)
;     (let ([sl (s-exp->list s)])
;       (case (s-exp->symbol (first sl))
;         [(+) (plusS (parse (second sl)) (parse (third sl)))]
;         [(*) (multS (parse (second sl)) (parse (third sl)))]
;         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
;         [(~) (uminusS (parse (second sl)))]
;         [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
;         [(call) (appS (parse (second sl)) (parse (third sl)))]
;         [else (error 'parse "invalid input")]))]))

; Facilitador
;(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))

; Testes
;(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
;              mt-env)
;      (numV 15))
;(interpS '(+ 10 (call (func x (+ x x)) 16)))