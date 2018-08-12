#lang racket

(provide

 ;; Pipe the expression as the function of each of the forms
 :-
 
 ;; Pipe the expression as the first argument of each of forms
 ;; (-> x (+ 3) (- 5))  is equivalent to (- (+ x 3) 5)
 :->

 ;; Pipe an expression as the last argument of each of the forms
 ;; (->> 2 (+ 3) (- 5)) is equivalent to (- 5 (+ 3 x))
 :->>



 ;; Pipe expressions through forms concurrently as the first argument
 ;; (=> (x y) (+ 3) (- 5)) is equivalent to (values (+ x 3) (- y 5))
 :=>

 ;; Pipe expressions through forms concurrently as the last argument
 ;; (=>> (x y) (+ 3) (- 5)) is equivalent to (values (+ 3 x) (- 5 y))
 :=>>

 ;; Fanout, piping a single value concurently as the first argument
 ;; (-<=> x (+ 3) (- 5)) is equivalent to (values (+ x 3) (- x 5))
 :-<=>

 ;; Fanout, piping a single value concurently as the last argument
 ;; (-<=>> x (+ 3) (- 5)) is equivalent to (values (+ 3 x) (- 5 x))
 :-<=>>

 ;; Unsplit, pipe the values into the form, proceding the existing arguments
 ;; (=>-> (x y) (- 5)) is equivalent to (- x y 5)
 :=>->

 ;; Unsplit, pipe the values into the form, proceding the existing arguments
 ;; (=>->> (x y) (- 5)) is equivalent to (- 5 x y)
 :=>->>)

(require racket/syntax)

(define-syntax :-
  (syntax-rules ()
    ((_ x) x)
    ((_ f (xs ...) forms ...)
     (:- (f xs ...) forms ...))))

(define-syntax :->
  (syntax-rules ()
    ((_ x) x)
    ((_ x (f xs ...) forms ...)
     (:-> (f x xs ...) forms ...))))

(define-syntax :->>
  (syntax-rules ()
    ((_ x) x)
    ((_ x (f xs ...) forms ...)
     (:->> (f xs ... x) forms ...))))

(define-syntax :=>
  (syntax-rules ()
    ((_ (xs ...) (f ys ...) ...)
     (values (f xs ys ...) ...))))

(define-syntax :=>>
  (syntax-rules ()
    ((_ (xs ...) (f ys ...) ...)
     (values (f ys ... xs) ...))))

(define-syntax (:-<=> stx)
  (syntax-case stx ()
    ((_ x (f ys ...) ...)
     (let ((var (gensym)))
       #`(let ((#,var x))
	   (values (f #,var ys ...) ...))))))

(define-syntax (:-<=>> stx)
  (syntax-case stx ()
    ((_ x (f ys ...) ...)
     (let ((var (gensym)))
       #`(let ((#,var x))
	   (values (f ys ... #,var) ...))))))

(define-syntax (:=>-> stx)
  (syntax-case stx (values :=> :=>> :-<=> :-<=>>)
    ((_ (values xs ...) (f ys ...))
     #'(f xs ... ys ...))

    ((_ (:=> (xs ...) (f ys ...) ...) (g zs ...))
     #'(g (f xs ys ...) ... zs ...))

    ((_ (:=>> (xs ...) (f ys ...) ...) (g zs ...))
     #'(g (f ys ... xs) ... zs ...))

    ((_ (:-<=> x (f ys ...) ...) (g zs ...))
     (let ((var (gensym)))
       #`(let ((#,var x))
	   (g (f #,var ys ...) ... zs ...))))

    ((_ (:-<=>> x (f ys ...) ...) (g zs ...))
     (let ((var (gensym)))
       #`(let ((#,var x))
	   (g (f ys ... #,var) ... zs ...))))))


(define-syntax (:=>->> stx)
  (syntax-case stx (values :=> :=>> :-<=> :-<=>>)
    ((_ (values xs ...) (f ys ...))
     #'(f xs ... ys ...))

    ((_ (:=> (xs ...) (f ys ...) ...) (g zs ...))
     #'(g zs ... (f xs ys ...) ... ))

    ((_ (:=>> (xs ...) (f ys ...) ...) (g zs ...))
     #'(g zs ... (f ys ... xs) ...))

    ((_ (:-<=> x (f ys ...) ...) (g zs ...))
     (let ((var (gensym)))
       #`(let ((#,var x))
	   (g zs ... (f #,var ys ...) ...))))

    ((_ (:-<=>> x (f ys ...) ...) (g zs ...))
     (let ((var (gensym)))
       #`(let ((#,var x))
	   (g zs ... (f ys ... #,var) ...))))))

(module+ test
  (require rackunit)

  (check-equal? (:-> 1)  1)
  (check-equal? (:->> 1) 1)

  (check-equal? (:-> 1 (- 10)) -9)
  (check-equal? (:->> 1 (- 10)) 9)

  (check-equal?  (:-> 10 (- 1) (- 2)) 7)
  (check-equal?  (:->> 10 (- 1) (- 2)) 11)

  (check-equal? (call-with-values (lambda () (:=> (1 2) (+ 3) (+ 4))) list)
		'(4 6))
  (check-equal? (call-with-values (lambda () (:=>> (1 2) (- 3) (- 4))) list)
		'(2 2))

  (check-equal? (:=>-> (:=> (1 2) (+ 3) (+ 4)) (- 10)) -12)
  (check-equal? (:=>->> (:=> (1 2) (+ 3) (+ 4)) (- 10)) 0))


