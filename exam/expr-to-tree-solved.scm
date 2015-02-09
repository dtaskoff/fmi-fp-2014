;;; Define a procedure with the following signature:
;;; (define (expr->tree expr) ..)

;;; which takes a logic expression and constructs
;;; a deduction tree from it.
;;;
;;; A logic expression is:
;;;   * a variable (an alphabetic symbol such as 'x', 'y'..)
;;;   * a binary predicate taking expressions as it's arguments
;;; And a predicate is also an alphabetic symbol
;;;
;;; Examples of expressions:
;;; x    p(x,y)    p(q(x,y),r(y,x))
;;;
;;; Info: There won't be any spaces in the expressions,
;;; so p(x, y) is not a valid expression in this case.
;;;
;;; Example tests:
;;; (expr->tree "x")           -> #\x
;;; (expr->tree "p(x,y)")      -> (list #\p (list #\x) (list #\y))
;;; (expr->tree "p(x,q(y,z))") -> (list #\p (list #\x)
;;;                                     (list #\q (list #\y) (list #\z))

;;; and yes, you can use the r5rs specification if you want:
;;; http://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-2.html
(define (expr->tree expr)
  (let ((l (string-length expr)))
    (letrec ((helper
              (lambda (curr i)
                (if (>= i l)
                    (list (list) i)
                    (let ((c (string-ref expr i)))
                      (cond ((char=? c #\()
                             (let* ((lres (helper c (+ i 1)))
                                    (ltree (car lres))
                                    (lj (cadr lres))
                                    (rres (helper c lj))
                                    (rtree (car rres))
                                    (rj (cadr rres)))
                               (list (list curr ltree rtree) rj)))
                            ((char=? c #\))
                             (list (list curr) (+ i 2)))
                            ((char=? c #\,)
                             (list (list curr) (+ i 1)))
                            (else (helper c (+ i 1)))))))))
      (if (= l 1)
          (string-ref expr 0)
          (car (helper #\nul 0))))))

;;; some procedures for testing
(define test-count 0)

(define (test input case)
  (begin
    (set! test-count (+ test-count 1))
    (string-append "Test #" (number->string test-count)
                   (if (equal? input case)
                       " passed!"
                       " failed!"))))

;;; p           -> p
(test (expr->tree "p") #\p)
;;; p(x,y)      -> (p (x) (y))
(test (expr->tree "p(x,y)")
      (list #\p (list #\x) (list #\y)))
;;; p(x,q(y,z)) -> (p (x (q (y z))))
(test (expr->tree "p(x,q(y,z))")
      (list #\p (list #\x) (list #\q (list #\y) (list #\z))))
;;; p(q(x,y),z) -> (p ((q (x y)) z))
(test (expr->tree "p(q(x,y),z)")
      (list #\p (list #\q (list #\x) (list #\y)) (list #\z)))
;;; p(q(x,y),r(x,y)) -> (p (q (x) (y)) (r (x) (y)))
(test (expr->tree "p(q(x,y),r(x,y))")
      (list #\p (list #\q (list #\x) (list #\y))
            (list #\r (list #\x) (list #\y))))
;;; p(q(r(x,y),z),q(x,z))
(test (expr->tree "p(q(r(x,y),z),q(x,z))")
      (list #\p (list #\q (list #\r (list #\x) (list #\y)) (list #\z))
            (list #\q (list #\x) (list #\z))))
;;; p(q(r(x,y),z),q(x,r(y,z))
(test (expr->tree "p(q(r(x,y),z),q(x,r(y,z)))")
      (list #\p (list #\q (list #\r (list #\x) (list #\y)) (list #\z))
            (list #\q (list #\x) (list #\r (list #\y) (list #\z)))))
;;; p(q(r(x,y),r(y,x),q(r(x,y),r(y,x))
(test (expr->tree "p(q(r(x,y),r(y,x),q(r(x,y),r(y,x))")
      (list #\p (list #\q (list #\r (list #\x) (list #\y))
                      (list #\r (list #\y) (list #\x)))
            (list #\q (list #\r (list #\x) (list #\y))
                      (list #\r (list #\y) (list #\x)))))
;;; p(q(r(s(x,y),y),y),y)
(test (expr->tree  "p(q(r(s(x,y),y),y),y)")
      (list #\p (list #\q (list #\r (list #\s (list #\x) (list #\y))
                                (list #\y))
                      (list #\y))
            (list #\y)))
;;; p(x,q(x,r(x,s(x,y))))
(test (expr->tree  "p(x,q(x,r(x,s(x,y))))")
      (list #\p (list #\x)
            (list #\q (list #\x)
                  (list #\r (list #\x)
                        (list #\s (list #\x) (list #\y))))))
