#lang plai-typed
(define-type ArithC
  [numC  (n : number)]
  [plusC  (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [ifC   (cond : ArithC) (sim : ArithC) (não : ArithC)]
  [ifgC (conda : ArithC) (condb : ArithC) (sim : ArithC) (não : ArithC)]
)

(define-type ArithS
  [numS  (n : number)]
  [plusS  (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [ifS   (cond : ArithS) (sim : ArithS) (não : ArithS)]
  [ifgS (conda : ArithS) (condb : ArithS) (sim : ArithS) (não : ArithS)]
  [iflS (conda : ArithS) (condb : ArithS) (sim : ArithS) (não : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
)

(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     ( let ([ sl (s-exp->list s ) ])
      (case (s-exp->symbol (first sl))
        [(+) (plusS (parse (second sl)) (parse(third sl)))]
        [(*) (multS (parse (second sl)) (parse (third sl)))]
        [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
        [(ifg) (ifgS (parse (second sl)) (parse (third sl)) (parse (fourth sl)) (parse (list-ref sl 4)))]
        [(ifl) (ifgS (parse (second sl)) (parse (third sl)) (parse (fourth sl)) (parse (list-ref sl 4)))]
        [(-) (bminusS (parse (second sl)) (parse (third sl)))]
        [(~) ( uminusS ( parse ( second sl ) ) ) ]
        [else (error 'parse "invalid list input")]
    ))]
    [else (error 'parse "invalid input")]
    ))

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
  [numS (n) (numC n)]
  [plusS (l r) (plusC (desugar l) (desugar r))]
  [multS (l r) (multC (desugar l) (desugar r ))]
  [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
  [ifgS (ca cb s n) (ifgC (desugar ca) (desugar cb) (desugar s) (desugar n))]
  [iflS (ca cb s n) (ifgC (desugar cb) (desugar ca) (desugar s) (desugar n))]
  [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
  [uminusS (e) (multC (numC -1) (desugar e))]
  ))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [ifC (c s n) (if (zero? (interp c)) (interp n) (interp s))]
    [ifgC (ca cb s n) (if (> (interp ca) (interp cb)) (interp s) (interp n))]
     )
 )