sixth week
==============

finally, we introduced haskell!

pure functional programming

how will we run our haskell code?
---------------------
* download haskell platform https://www.haskell.org/platform/
* start ghci

useful options in ghci:
----------------------
* :l -> load a haskell file
* :r -> reload
* :t -> check the type of a function
* :q -> quit

data types in haskell:
----------------------
* Int, Integer
* Float, Double
* Char, String
* Bool

lists in haskell:
----------------------
* [a]
* head, tail

a little about pattern matching:
----------------------
```
f (x:xs) = ...
f _      = ...
```
```
f []     = ...
f (x:xs) = ...
```

additional material:
----------------------
http://learnyouahaskell.com/starting-out
