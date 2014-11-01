;;; the ordinary function composition
(define (compose f g)
  (lambda (x) (f (g x))))

;;; and the muliple arguments approach
;;; (better than the one with eval which I showed)
(define (composem f g)
  (lambda x (f (apply g x))))

;;; using lets for helper procedures
(define (is-prime? p)
  (letrec
    ((helper
      (lambda (n)
        (if (= n p) #t
            (and (not (zero? (modulo p n)))
                 (helper (+ n 1)))))))
    (and (> p 1)
         (helper (ceiling (sqrt p))))))

;;; the odd-even example with letrec
(define (my-even? n)
  (letrec
    ((my-even?
      (lambda (x)
        (if (zero? x) #t
            (my-odd? (- x 1)))))
     (my-odd?
      (lambda (x)
        (if (zero? x) #f
            (my-even? (- x 1))))))
    (my-even? n)))

;;; a procedure that transforms a list of numbers
;;; to a list of squares
(define (squares lst)
  (if (null? lst) (list)
      (cons (* (car lst) (car lst)) (squares (cdr lst)))))

;;; a procedure that returns the even numbers in a range
(define (evens to)
  (mult-by-two (range 0 (quotient to 2))))

(define (mult-by-two lst)
  (if (null? lst) (list)
      (cons (* (car lst) 2) (mult-by-two (cdr lst)))))
  
;;; implementation of map
(define (my-map proc lst)
  (if (null? lst) (list)
      (cons (proc (car lst)) (my-map proc (cdr lst)))))

;;; redo the previous two functions using map
(define (squares2 lst)
  (map (lambda (x) (* x x)) lst))

(define (evens to)
  (map (lambda (x) (* x 2)) (range 0 (quotient to 2))))
