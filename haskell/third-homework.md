Exercise 1:
------------
```
Write a function which takes a list and 
returns a list of lists, consisting of:
the first list contains all elements
the second one contains all elements in steps of one
the third one contains all elements in steps of two
etc...
```
 
Examples:
```
> skips []
[]
```
```
> skips [1..3]
[[1,2,3],[2],[3]]
```
```
> skips [1..6]
[[1,2,3,4,5,6],[2,4,6],[3,6],[4],[5],[6]]
```
 
Exercise 2:
------------------
```
Write a function which takes a list and removes
all adjacent duplicates.
You must implement it using `fold`!
```
 
Examples:
```
> nodups [1,1]
[1]
```
```
> nodups [1, 1, 2, 2, 3, 3]
[1,2,3]
```
```
> take 4 $ nodups $ cycle [1, 2]
[1,2,1,2]
```
