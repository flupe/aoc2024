A collection of snippets that stood out when solving the daily puzzles.

### Sublists (day 2)

Given an input list `xs`, generate all the lists obtained by dropping *one* element from `xs`:

```hs
subs :: [a] -> [[a]]
subs []     = []
subs (x:xs) = xs : map (x :) (subs xs)

-- >>> subs [1, 2, 3]
-- [[2,3],[1,3],[1,2]]
```

### strict updates on `STRef` (day 4)

On day 4 I tried using a `STRef` for my counters, and avoid building lists as much as possible.
I don't have a good grasp over laziness but it would appear this style of writing is not worth the hassle.
Though it should be noted that it's crucial to use the strict `modifySTRef'` to avoid a memory leak.

```hs
increment :: STRef s Int -> ST s ()
increment = flip modifySTRef' (+ 1)
```
