(define (flatten lst)
  (cond ((not (list? lst)) (list lst))
        ((null? lst) lst)
        ((append (flatten (car lst))
                (flatten (cdr lst))))))


(define (is-prime? p)
  (letrec ((ip-helper?
             (lambda (n)
                (if (< n 2)
                    #t
                    (and (not (zero? (modulo p n)))
                         (ip-helper? (- n 1)))))))
        (and (> p 1)
             (ip-helper? (floor (sqrt p))))))

(define (next-prime n)
    (let ((next (+ n 1)))
        (if (is-prime? next)
            next
            (next-prime next))))

(define (pf-helper n factor p res)
  (cond
    ((is-prime? n)
     (if (= n factor)
         (cons (cons (n (+ p 1))) res)
         (cons (cons n 1) (cons (cons factor p) res))))
    ((= n factor) (cons (cons n (+ p 1)) res))
    ((zero? (modulo n factor))
     (pf-helper (quotient n factor) factor (+ p 1) res))
    ((zero? p)
     (pf-helper n (next-prime factor) 0 res))
    (else (pf-helper n (next-prime factor) 0
                (cons (cons factor p) res)))))

(define (prime-factorization n)
  (if (< n 2) (list)
      (reverse (pf-helper n 2 0 (list)))))
