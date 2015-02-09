;;; Define a procedure with the following signature:
(define (expr->tree expr) ..)

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
(expr->tree "x")           -> #\x
(expr->tree "p(x,y)")      -> (list #\p (list #\x) (list #\y))
(expr->tree "p(x,q(y,z))") -> (list #\p (list #\x)
                                    (list #\q (list #\y) (list #\z))

;;; and yes, you can use the r5rs specification if you want:
;;; http://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-2.html
