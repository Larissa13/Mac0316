#lang plai-typed
(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)])

(define-type-alias Location number)
(define-type Binding
      [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)        
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  [v*s (v : Value) (s : Store)])

(define-type ExprC
  [numC  (n : number)]
  [idC (id : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)]
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
  [boxS (arg : ExprS)]
  [unboxS (arg : ExprS)]
  [setboxS (b : ExprS) (v : ExprS)]
  [seqS (b1 : ExprS) (b2 : ExprS)]
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
         [(-#) (boxS (parse (second sl)))]
         [(>#) (unboxS (parse (second sl)))]
         [(!#) (setboxS (parse (second sl)) (parse (third sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
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
    [boxS    (a)  (boxC   (desugar a))]
    [unboxS  (a)  (unboxC (desugar a))]
    [setboxS (b v)   (setboxC (desugar b) (desugar v))]
    [seqS    (b1 b2) (seqC (desugar b1) (desugar b2))]
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [ifgS (ca cb s n) (ifgC (desugar ca) (desugar cb) (desugar s) (desugar n))]
    [iflS (ca cb s n) (ifgC (desugar cb) (desugar ca) (desugar s) (desugar n))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
))


(define (lookup [for : symbol] [env : Env]) : Location
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto


; fetch é o lookup do store
(define (fetch [l : Location] [sto : Store]) : Value
       (cond
            [(empty? sto) (error 'fetch "posição não encontrada")]
            [else (cond
                  [(= l   (cell-location (first sto)))   ; achou!
                                 (cell-val (first sto))]
                  [else (fetch l (rest sto))])]))        ; vê no resto


;; retorna a próxima localização disponível
(define new-loc
   (let ( [ n (box 0)])
        (lambda () 
           (begin
              (set-box! n (+ 1 (unbox n)))
              (unbox n)))))

(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)] 
    [idC (n)  (v*s (fetch (lookup n env) sto) sto)]  ; busca em cascata, env e em seguida no sto
    [lamC (a b) (v*s (closV a b env) sto)]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1) ; resultado e store retornado por b1
                          (interp b2 env s-b1)])]
    ; aplicação de função
    [appC (f a)
      (type-case Result (interp f env sto) ; acha a função
         [v*s (v-f s-f)
              (type-case Result (interp a env s-f) ; argumento com sto modificado pela função
                 [v*s (v-a s-a)
                      (let ([onde (new-loc)]) ; aloca posição para o valor do argumento
                           (interp (closV-body v-f) ; corpo
                                   (extend-env (bind (closV-arg v-f) onde) ; com novo argumento
                                       (closV-env v-f))
                                   (override-store (cell onde v-a) s-a))) ; com novo valor
                  ])])]
    [plusC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]
    ; ifC já serializa
    [ifC (c s n) (if (zero? (numV-n (v*s-v (interp c env sto)))) (interp n env sto) (interp s env sto))]

    [ifgC (ca cb s n) (if (> (numV-n (v*s-v (interp ca env sto))) (numV-n (v*s-v (interp cb env sto))))
                          (interp s env sto) (interp n env sto))]
    
    ; cria uma caixa, precisa do valor e de um novo local
    [boxC (a) 
          (type-case Result (interp a env sto)
            [v*s (v-a s-a)
                 (let ([onde (new-loc)])
                   (v*s (boxV onde) 
                        (override-store (cell onde v-a) s-a)))])]
                          
    [unboxC (a) (type-case Result (interp a env sto)
                  [v*s (v-a s-a)
                       (v*s 
                        (fetch (boxV-l v-a) s-a) ; valor
                        s-a                      ; store
                        )])]

    [setboxC (b v) (type-case Result (interp b env sto)
                     [v*s (v-b s-b)
                          (type-case Result (interp v env s-b)
                            [v*s (v-v s-v)
                                 (v*s v-v
                                      (override-store 
                                       (cell (boxV-l v-b)
                                             v-v)
                                       s-v))])])]

    ))
   