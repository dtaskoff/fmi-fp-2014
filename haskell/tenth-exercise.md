tenth week
==============

* like our last one, this week we took a double exercise

we talked a little about functors again:
----------------------------------
* why indeed we will want to use them?
* why sometimes we need a little more than functors..

applicatives:
--------------------------------
* what is an applicative?
* one level above functors
* `pure`, `<$>`(`fmap`) and `<*>` (also `<*` and `*>`)
* implementations of Shell (Maybe), ordinary lists and ziplists
```
instance Applicative Shell where

    pure        = A

    A f <*> A x = A $ f x

    _   <*> _   = Empty

instance Applicative [] where

    pure x    = [x]

    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative ZipList

    pure x    = cycle x

    fs <*> xs = zipWith ($) fs xs
    
```


alternatives:
----------------------------------
* helping us to short circuit a chain of applicatives
* `<|>` and `empty` (I think I forgot that)


and finally:
-----------------------------------
* the sentence parser
* we only looked at simple sentences containing of

words, spaces and some ending characters (`.`, `?`, `!`)
* check out our implementation for it!
