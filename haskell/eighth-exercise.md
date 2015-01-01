eighth week
==============

* some more things with higher order functions on lists
* the `($)` operator
* function composition and the `(.)` operator

folds (foldr):
----------------------------------
* prooves by induction
* universal property
`h = fold f v` iff

`h [] = v` and `h (x:xs) = f x (h xs)`

* fusion property
`h . fold g w = fold f v` iff

`h w = v` and `h (g x y) = f x (h y)`
