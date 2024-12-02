A collection of snippets that stood out when solving the daily puzzles.

### sublists (day 2)

Given an input list `xs`, generate all the lists obtained by dropping *one* element from `xs`:

```hs
subs :: [a] -> [[a]]
subs []     = []
subs (x:xs) = xs : map (x :) (subs xs)

-- >>> subs [1, 2, 3]
-- [[2,3],[1,3],[1,2]]
```
