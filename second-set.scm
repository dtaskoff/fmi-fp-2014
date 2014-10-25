;;; comments with some explanations
;;; are coming soon :)

(define (range from to)
  (if (= from to)
      (list to)
      (cons from
            (range (+ from 1) to))))

(define (elem x lst)
  (if (null? lst)
      #f
      (or (equal? x (car lst))
          (elem x (cdr lst)))))

(define (elem2 x lst)
  (not (null? (member x lst))))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      (list)
      (cons
        (cons (car lst1) (car lst2))
        (zip (cdr lst1) (cdr lst2)))))

(define (chars str)
  (cond ((string=? str "") (list))
        ((char=? (string-ref str 0) #\ )
         (chars (substring str 1)))
        (else
          (cons (string-ref str 0)
                (chars (substring str 1))))))

(define (op o)
  (case o
    (#\+ +)
    (#\- -)
    (#\* *)
    (#\/ /)))

(define (op? o)
  (case o 
    ((#\+ #\- #\* #\/) #t)
    (else #f)))

(define (char->number n)
  (- (char->integer n) 48))

(define (rpn-calculator expr)
  (define (helper-calc stack rest)
    (cond
      ((null? rest) (car stack))
      ((op? (car rest))
       (helper-calc
        (cons
          ((op (car rest)) (cadr stack) (car stack))
          (cddr stack))
        (cdr rest)))
      (else (helper-calc (cons (char->number (car rest))
                               stack)
                         (cdr rest)))))
  (helper-calc (list) (chars expr)))
