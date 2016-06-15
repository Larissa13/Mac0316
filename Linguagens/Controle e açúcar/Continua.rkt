#lang plai-typed

#|
 | Funções não tem mais nome, serão chamadas de lamC (em homenagem ao λ)
 |#

(define-type ExprC
  [numC (n : number)]
  [idC  (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)] ; nomes não são mais necessários
  [appC (fun : ExprC) (arg : ExprC)]
  [ifC   (condição : ExprC) (sim : ExprC) (não : ExprC)]
  )

; inclui funções
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)] 
  [lamS    (arg : symbol) (body : ExprS)] ; muda de acordo
  [appS    (fun : ExprS) (arg : ExprS)] 
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (s : ExprS) (n : ExprS)]
  )


; agora é preciso tomar cuidado com as modificações
(define (desugar [as : ExprS]) : ExprC  
  (type-case ExprS as
    [numS    (n) (numC n)]
    [idS     (s) (idC s)]
    [lamS     (a b)  (lamC a (desugar b))] ; idem
    [appS    (fun arg) (appC (desugar fun) (desugar arg))] 
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    ))



; Value muda, pois closV tem um lambda como segundo argumento, a continuação
(define-type Value
  [numV  (n : number)]
  [closV (f : (Value (Value -> Value) -> Value))])

; símbolos devem se associar ao número (ou a Value?)
(define-type Binding
  [bind (name : symbol) (val : Value)])


; Trocamos por isso:
(define-type-alias Env (symbol -> Value)) ; o environment é uma função
(define (mt-env [name : symbol]) (error 'lookup "name not found")) ; função constante
(define (extend-env [b : Binding] [e : Env])
  (lambda ([name : symbol]) : Value
    (if (symbol=? name (bind-name b)) ; achou?
        (bind-val b)   ; sim, é o próprio
        (lookup name e) ; olha no anterior
        )))

(define (lookup [n : symbol] [e : Env]) : Value (e n))


(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "Um dos argumentos não é número")]))

;; interp tradicional
;(define (interp [expr : ExprC] [env : Env]) : Value
;  (type-case ExprC expr
;    [numC (n) (numV n)]
;    [idC  (n) (lookup n env)]
;    [appC (f a) (local ([define f-value (interp f env)]
;                        [define a-value (interp a env)])
;                  ((closV-f f-value) a-value))]
;    [plusC (l r) (num+ (interp l env) (interp r env))]
;    [multC (l r) (num* (interp l env) (interp r env))]
;    [ifC (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]
;    [lamC  (a b) (closV (lambda (arg-val)
;                         (interp b
;                                 (extend-env (bind a arg-val)
;                                             env))))]))


; interp com continuações
(define (interp/k [expr : ExprC] [env : Env] [k : (Value -> Value)]) : Value 
  (type-case ExprC expr
    [numC (n) (k (numV n))]
    [idC  (n) (k (lookup n env))]
    [appC (f a) (interp/k f env (lambda (fv)
                                  (interp/k a env 
                                            (lambda (av) ((closV-f fv) av k)))))]
    [plusC (l r) (interp/k l env (lambda (lv) (interp/k r env (lambda (rv) (k (num+ lv rv))))))]
    [multC (l r) (interp/k l env (lambda (lv) (interp/k r env (lambda (rv) (k (num* lv rv))))))]
    [ifC (c s n) (interp/k c env (lambda (cv) 
                                   (if (zero? (numV-n cv)) 
                                       (interp/k n env (lambda (nv)  (k nv)))
                                       (interp/k s env (lambda (sv)  (k sv))))))]
    [lamC  (a b) (k (closV (lambda (arg-val dyn-k)
                             (interp/k b
                                       (extend-env (bind a arg-val)
                                                   env) dyn-k))))]
    ))

; o parser permite definir funções...
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Disparador
(define (interp [expr : ExprC]) : Value
  (interp/k expr mt-env (lambda (id) id)))

; Facilitador
(define (interpS [s : s-expression]) (interp (desugar (parse s))))

; Testes
(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10))))  (numV 15))
(interpS '(+ 10 (call (func x (+ x x)) 16)))