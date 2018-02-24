
data Cell = OpenCell [Int]
          | FilledCell Int
          | FailedCell

instance Show Cell where
    show (OpenCell xs) = show xs
    show (FilledCell x) = show x
    show FailedCell = "**"

cp s ls
    | s == 0 = []
    | otherwise = ls : (cp (s-1) ls)
emptyG s = cp s (cp s (OpenCell [1..s]))


inI v (a,b) = and [a <= v, v < b]

{-
[[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
,[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
,[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
,[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]]

[[1 2 3 4],
 [3 4 1 2],
 [2 3 4 1],
 [4 1 2 3]]
 -}

d = 3

modifyElem (x:xs) i func
    | i == 0 = (func x) : xs
    | otherwise = x : (modifyElem xs (i-1) func)

modifyElems [] (a,b) func = []
modifyElems (x:xs) (a,b) func
    | inI 0 (a,b) = (func x) : modifyElems xs (a-1, b-1) func
    | b <= 0      = x:xs
    | otherwise   = x : modifyElems xs (a-1, b-1) func

reduceC v (OpenCell c) = OpenCell (filter (/= v) c)
reduceC v (FilledCell c)
    | v == c = FailedCell
    | otherwise = (FilledCell c)
reduceR v r j = modifyElem r j (reduceC v)
reduceRs v r (a,b) = modifyElems r (a,b) (reduceC v)
reduceG v g i j = modifyElem g i (\x -> reduceR v x j)


--setValue v [] x y = []
setValue v (l:ls) x y = func (l:ls) 0
    where func [] _ = []
          func (l:ls) yi
            | yi == y = (red l x) : (func ls (yi+1))
            | inI yi yI = (reduceRs v l xI) : (func ls (yi+1))
            | otherwise = (reduceR v l x) : (func ls (yi+1))
          set (OpenCell c)
            | elem v c = (FilledCell v)
            | otherwise = FailedCell
          red [] i = []
          red (a:as) i
            | i == 0 = (set a) : red as (i-1)
            | otherwise = (reduceC v a) : red as (i-1)
          yI = ((div y d)*d, (div y d)*d + d)
          xI = ((div x d)*d, (div x d)*d + d)


-- input <- readFile "s1"
-- putStrLn input
main = do
    let g = emptyG 9

    putStrLn (show (setValue 2 g 4 4))
