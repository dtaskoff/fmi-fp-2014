;;; write a procedure which takes a mark and a board (3x3)
;;; and tells us if the player using this mark has won

;;; in order to be equally easy to write this in Scheme as it's in Haskell,
;;; I've provided some standard procedures' implementations

(define (id x) x)

(define (reduce proc start lst)
  (letrec ((helper
            (lambda (acc lst)
              (if (null? lst) acc
                  (helper (proc acc (car lst)) (cdr lst))))))
    (helper start lst)))

(define (all pred lst)
  (reduce (lambda (acc x)
    (and (pred x) acc)) #t lst))

(define (any pred lst)
  (reduce (lambda (acc x)
    (or (pred x) acc)) #f lst))

(define (transpose lst)
  (if (null? (car lst)) (list)
      (cons (map car lst)
            (transpose (map cdr lst)))))

;;; It's your turn now!
(define (has-won mark board) 
    ..)

;;; Examples:
;;; (has-won 1 (list (list 0 0 0)
;;;                  (list 0 0 0)
;;;                  (list 0 0 0))) -> #f
;;; (has-won 1 (list (list 1 0 0)
;;;                  (list 0 1 0)
;;;                  (list 0 0 1))) -> #t
