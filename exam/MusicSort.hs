{- You have a database (the file music.db) containing information about some albums
by some artists.

Write a haskell program, which receives command-line arguments
and does the following:
    * if the first argument is 'artist', then the second will
be a name of an artist and you should display all the albums composed by that artist.
    * the same goes if the argument is 'album'
    * if it is 'sort', then the second argument can be
'by-artist', 'by-album' and 'by-year', and you should display all the albums sorted by the given criteria (in increasing order).

An album information should be printed in the same fashion as written in the database, i.e.:
Of Mice & Men - Restoring Force [2014]
and each one should be displayed on a separate line.

Examples:
> runhaskell MusicSort.hs artist "Of Mice & Men"
Of Mice & Men - Restoring Force [2014]
Of Mice & Men - The Flood [2011]

> runhaskell MusicSort.hs sort by-year
Of Mice & Men - The Flood [2011]
Of Mice & Men - Restoring Force [2014]
-}


-- you can use sortBy for the sorting,
-- check with :t (or Hoogle) what it accepts
-- and with :i you can gather some information about
-- the Ordering type
import Data.List (sortBy, intercalate)
import System.Environment (getArgs)


-- This here is a huge hint for the implementation.
-- (At some time you will need that instance. Of course,
-- it was possible to write your own, but
-- we didn't go through that on the exercises)
instance Read Album where
    readsPrec i r =
        [(Album (init artist) (init name) (read year)
          , rest) |
            let (artist, h:s:res1) = break (== '-') r,
            let (name,   o:res2)   = break (== '[') res1,
            let (year,   c:rest)   = break (== ']') res2]

-- and remember, :t (or :type) and Hoogle (https://www.haskell.org/hoogle/)
-- are your best friends!
