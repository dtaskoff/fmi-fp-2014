ninth week
==============

* this week we took a double exercise, that's why 
the material we covered is a little bit more

the `main` function (finally):
----------------------------------
* the entry point of a haskell program
* what is a `do` block
* separating pure and impure code

interaction with the user (reeally basic IO):
----------------------------------
* `getLine`
* `print`
* `putStr` and `putStrLn`
* `getContents`
* `readFile` and `writeFile`
* `getArgs` (from `System.Environment`)
* we wrote a basic script imitating the unix' wc command

(algebraic) datatypes:
----------------------------------
* the keywords `type`, `data` and `newtype`
* the record syntax
* type parameters and the `Maybe` datatype
* recursive data structures

typeclasses:
--------------------------------
* we have already used them: `Num`, `Ord`, `Eq` and so on..
* making instances of typeclasses (`deriving`)

monoids:
--------------------------------
* what is a monoid?
* `mempty` and `mappend` (and `mconcat`)
* monoid laws:
    mempty is the unit of the monoid:
        `mappend x mempty = x`
        `mappend mempty x = x`
    associativity holds for the monoid:
        `mappend x (mappend y z) = mappend (mappend x y) z`
* the `Sum` and `Product` instances
* why would we used them?

functors:
----------------------------------
* what is a functor?
* `fmap`
* functor laws:
    `fmap id      = id`
    `fmap (f . g) = fmap f . fmap g`
