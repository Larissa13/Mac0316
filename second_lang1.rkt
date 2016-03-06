#lang plai-typed
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

;argumento: valor isso: nome da 'variável' a ser substituída em: corpo da função          
(define (subst [valor : ExprC] [isso : symbol] [em : ExprC]) : ExprC
  (type-case ExprC em
    [numC (n) em] ; nada a substituir
    [idC (id) (cond
                [(symbol=? id isso) valor]
                [else em])]
    [appC  (f a) (appC f (subst valor isso a))]
    [plusC (l r) (plusC (subst valor isso l) (subst valor isso r))]
    [multC (l r) (multC (subst valor isso l) (subst valor isso r))]
    [ifC (c s n) (ifC (subst valor isso c) (subst valor isso s) (subst valor isso n))]
    [ifgC (ca cb s n) (ifgC (subst valor isso ca) (subst valor isso cb) (subst valor isso s) (subst valor isso n))]
    ))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "referência para função não definida")]
    [(cons? fds) (cond
                   [(equal? n (fdc-name (first fds))) (first fds)] ;achou a função
                   [else (get-fundef n (rest fds))])]))

(define (interp [a : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (c) (error 'interp "Não deveria enncontrar isto aqui")]
    [appC (f a )
          (local ([define fd (get-fundef f fds)])
            (interp (subst a (fdc-arg fd) (fdc-body fd)) fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [ifC (c s n) (if (zero? (interp c fds)) (interp n fds) (interp s fds))]
    [ifgC (ca cb s n) (if (> (interp ca fds) (interp cb fds)) (interp s fds) (interp n fds))]
     )
 )