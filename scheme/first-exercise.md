first week
==============

data types:
-------------
* booleans - #t, #f
* numbers - 1, 1.0, 1e0
* characters - #\c

expressions:
-------------
* defining variables  
`(define x 5)`
* defining procedures  
`(define (id x) x)`
* the applicative order of evaluation in r5rs  
```
(define (f) (f))  
(define (my-if test if-case else-case)
  (if test if-case else-case)) 
(my-if #t 0 (f))
```

conditionals:
-------------
* if
* cond

recursion:
-------------
* tail recursion and tail context
* why tail recursion is better? (example with fibonacci's sequence)

some things about pairs and lists
-------------
* cons, car, cdr
* represantation of lists in r5rs

additional texts:
-------------
* [SICP Chapter one](http://mitpress.mit.edu/sicp/full-text/book/book.html)
