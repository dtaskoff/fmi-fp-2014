;;; a procedure returning a bank account (a closure)
;;; (it's supposed that you already did that during lectures, but..)
(define (account s)
  (define sum s)
  (define (withdraw n)
    (if (> n sum)
        "you don't have enough money remaining!"
        (begin (set! sum (- sum n)) sum)))
  (define (deposit n)
    (if (< n 0)
        "you are trying to deposit a negative sum!"
        (begin (set! sum (+ sum n)) sum)))
  (lambda (action)
    (cond
      ((equal? action 'balance) sum)
      ((equal? action 'withdraw) withdraw)
      ((equal? action 'deposit) deposit)
      (else "incorrect action!"))))

;;; from here on starts the implementation of our
;;; binary search tree

(define nil '())

(define (empty? tree)
  (null? tree))

;;; the getters.
;;; we assume that tree is not the empty tree!
(define (root tree)
  (car tree))
(define (left tree)
  (cadr tree))
(define (right tree)
  (caddr tree))

;;; procedure constructing a tree from a root and two subtrees
(define (cons-tree root l r)
  (list root l r))

;;; procedure that adds a node to our tree.
;;; it does nothing if the element already exists in it
(define (add elem tree)
  (if (empty? tree)
      (cons-tree elem nil nil)
      (let ((x (root tree))
            (l (left tree))
            (r (right tree)))
        (cond
          ((< elem x) (cons-tree x (add elem l) r))
          ((> elem x) (cons-tree x l (add elem r)))
          (else tree)))))

;;; constructing a tree from a list
(define (make-tree lst)
  (if (null? lst)
      nil
      (add (car lst)
           (make-tree (cdr lst)))))

;;; ookay, now go play with these trees!

;;; is a given tree binary search tree?
(define (bst? tree)
  (or
   (empty? tree)
   (and
    (list? tree)
    (list? (left tree))
    (list? (right tree))
    (null? (cdddr tree))
    (bst? (left tree))
    (bst? (right tree))
    (or (empty? (left tree))
        (< (root (left tree))
           (root tree)))
    (or (empty? (right tree))
        (> (root (right tree))
           (root tree))))))

;;; okay, that was ugly.. -.-

;;; constructing a balanced tree from a node and
;;; a desired number of nodes
(define (baltree node n)
  (let ((n1 (floor (/ n 2)))
        (n2 (floor (/ (- n 1) 2))))
    (if (zero? n)
        nil
        (cons-tree node
                   (baltree node n1)
                   (baltree node n2)))))

;;; now, onto associative lists!
;;; we will represent them as binary trees
;;; whose nodes are keys and values, so most of the code will
;;; be the same

;;; adding a node to the associative list asl
(define (asc-list-add k v asl)
  (if (empty? asl)
      (cons-tree (cons k v) nil nil)
      (let ((x (root asl))
            (xk (car (root asl)))
            (l (left asl))
            (r (right asl)))
        (cond
          ((< k xk) (cons-tree x (asc-list-add k v l) r))
          ((> k xk) (cons-tree x l (asc-list-add k v r)))
          (else asl)))))

;;; constructing an associative list from an ordinary list
(define (make-asc lst)
  (if (null? lst)
      nil
      (asc-list-add (caar lst) (cdar lst)
                    (make-asc (cdr lst)))))

;;; getting the value at some key
;;; (false if such key is not in our list)
;;; (now we're O(lg(n)) ^^)
(define (find-at k asl)
  (if (empty? asl)
      #f
      (let ((x (root asl))
            (xk (car (root asl)))
            (l (left asl))
            (r (right asl)))
        (cond
          ((= k xk) (cdr x))
          ((< k xk) (find-at k l))
          ((> k xk) (find-at k r))))))

;;; setting the value at some key (the dirty way)
;;; we get the value back if such key is not present in the list
;;; (O(lg(n)) again)
(define (set-at! k v asl)
  (if (empty? asl)
      v
      (let ((x (root asl))
            (xk (car (root asl)))
            (l (left asl))
            (r (right asl)))
        (cond
          ((= k xk) (set-cdr! x v))
          ((< k xk) (set-at! k v l))
          ((> k xk) (set-at! k v r))))))

;;; the same procedure as above, except that we're pure
;;; this time
(define (set-at k v asl)
  (if (empty? asl)
      v
      (let ((x (root asl))
            (xk (car (root asl)))
            (l (left asl))
            (r (right asl)))
        (cond
          ((= k xk) (cons-tree (cons k v) l r))
          ((< k xk) (cons-tree x (set-at k v l) r))
          ((> k xk) (cons-tree x l (set-at k v r)))))))