Exercise 1:
------------
'''
(define (flatten lst)
..)'''
 
Examples:
'''
> (flatten '())
> ()
'''
'''
> (flatten '(1 2 3))
> (1 2 3)
'''
'''
> (flatten '('(1 2) '('(1 2) '()) '('('(1)))))
> (1 2 1 2 1)
'''
 
Exercise 2:
------------------
'''
(define (prime-factorization n)
..)
'''
 
Examples:
'''
> (prime-factorization 2)
> ((2 . 1))
'''
'''
> (prime-factorization 15)
> ((3 . 1) (5 . 1))
'''
'''
> (prime-factorization 2305672)
> ((2 . 3) (288209 . 1))
'''
