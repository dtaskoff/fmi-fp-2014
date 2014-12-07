;;; is a triangle pythagorean?
(define (is-pythagorean? a b c)
  (..))

;;; find the area of a triangle
(define (area a b c)
  (..))

;;; find the area of a triangle,
;;; but this time if it's pythagorean
;;; calculate it by multiplying the legs
(define (area a b c)
  (..))

;;; find the nth number in the fibonacci's sequence
(define (fib n)
  (define (fib-iter i a b)
    (cond
      ((= i n) a)
      ((fib-iter (+ i 1) b (+ a b)))))
  (fib-iter 0 0 1))

;;; calculate euler's number with 
;;; accurracy the nth term
(define (e n)
  (define (e-iter i term result)
    (cond
      ((= i n) result)
      ((e-iter (+ i 1) (/ term i) (+ result term)))))
  (e-iter 1 1 0))

;;; calculate e^x with accuracy n
(define (ex x n)
  (define (ex-iter i term result)
    (cond
      ((= i n) result)
      ((ex-iter (+ i 1) (* x (/ term i)) (+ result term)))))
(ex-iter 1 1 0))

;;; is a number prime?
(define (is-prime? n)
  (..))
