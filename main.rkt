#lang racket/base

(require
 (for-syntax racket/base racket/syntax syntax/parse))

(provide -> ->> ->< ->>)

(define-syntax ->
  (syntax-parser
    [(_ x:expr)
     #'x]

    [(_ x:expr (f:expr xs:expr ...) more-forms:expr ...)
     #'(-> (f x xs ...) more-forms ...)]))

(define-syntax ->>
  (syntax-parser
    [(_ x:expr)
     #'x]

    [(_ x:expr (f:expr xs:expr ...) more-forms:expr ...)
     #'(->> (f xs ... x) more-forms ...)]))


(define-syntax -><
  (syntax-parser
    [(_ x:expr)
     #'x]

    [(_ e:expr (f:expr xs:expr ...) ...+)
     (with-syntax ([x (generate-temporary 'x)])
       #'(let ([x e])
           (values
            (f x xs ...) ...)))]))

(define-syntax ->><
  (syntax-parser
    [(_ x:expr)
     #'x]

    [(_ e:expr (f:expr xs:expr ...) ...+)
     (with-syntax ([x (generate-temporary 'x)])
       #'(let ([x e])
           (values
            (f xs ... x) ...)))]))


(define-syntax >-
  (syntax-parser
    #:datum-literals (->)
    
    [(_ x:id -> e:expr)
     #'e]
    
    [(_ x:id -> e:expr form:expr more-forms:expr ...)
     #'(> x -> (let ([x e]) form) more-forms ...)]))

(define-syntax >>-
  (syntax-parser
    #:datum-literals (->)
    [(_ x:id xs:id ... -> e:expr)
     #'e]

    [(_ x:id xs:id ... -> e:expr form:expr forms:expr ...)
     #'(>>- x xs ... -> (let-values ([(x xs ...) e]) form) forms ...)]))

(module+ test
  (require rackunit)
  (define (sqr x) (* x x))

  (check-equal? (-> 'x) 'x)
  (check-equal? (->> 'x) 'x)
  (check-equal? (->< 'x) 'x)
  (check-equal? (->>< 'x) 'x)

  (check-equal? (-> 1) 1)
  (check-equal? (-> 1 (+ 2)) 3)
  (check-equal? (-> 1 (+ 2) (- 4)) -1)
  (check-equal? (->> 1 (+ 2) (- 4)) 1)

  (let ([f (compose + (λ (x) (->< x (- 3) (+ 3))))])
    (check-equal? (f 3) 6)
    (check-equal? (f 4) 8))

  (let ([f (compose + (λ (x) (->>< x (- 3) (+ 3))))])
    (check-equal? (f 3) 6)
    (check-equal? (f 4) 6))

  (struct point (x y) #:transparent)
  (check-equal?
   (>>- c0 c1 -> (->< (point 3 4) (point-x) (point-y))
        (values (sqr c0) (sqr c1))
        (values (sqrt (+ c0 c1))))
   5))

