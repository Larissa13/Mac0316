#lang plai-typed
(define-type ArithC
  [numC  (n : number)]
  [addC  (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [ifC   (cond : ArithC) (sim : ArithC) (não : ArithC)]
)

(define-type ArithS
  [numS  (n : number)]
  [addS  (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [ifS   (cond : ArithS) (sim : ArithS) (não : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
)

(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
    (let ([sl (s-exp->symbol s)])
      (case (s-exp->symbol (first sl))
        [(+) (addS (parse (second sl)) (parse(third sl)))]
        [(*) (multS (parse (second sl)) (parse (third sl)))]
        [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
        [(-) (bminusS (parse (second sl)) (parse (third sl)))]
        [(~) (uminusS (parse (second sl)))]
        [else (error 'parse "invalid list input")]
    ))]
    [else (error 'parse "invalid input")]
    ))

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
  [numS (n) (numC n)]
  [addS (l r) (addC (desugar l) (desugar r))]
  [multS (l r) (multC (desugar l) (desugar r ))]
  [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
  [bminusS (l r) (addC (desugar l) (multC (numC -1) (desugar r)))]
  [uminusS (e) (multC (numC -1) (desugar e))]
  ))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [addC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [ifC (c s n) (if (zero? (interp c)) (interp n) (interp s))]
     )
 )