import Debug.Trace

{- Generic implementation of a divide and conquer algorithm, being:
    - a the domain of the problem input
    - b the domain of the problem output

    And the parameters being:
    - (a -> Bool) (trivial): To determine if given a problem input, the solution is trivial (.e. base case)
    - (a -> (a, a)) (divide): Divide a problem input into two "simpler" subproblems.
    - (a -> (a, a) -> (b, b) -> b) (conquer): Given a problem, its two subproblems and the solutions for the
        two subproblems, returns the solution to the "bigger" problem.
-}
divideAndConquer :: (a -> Bool) -> (a -> b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
divideAndConquer trivial direct divide conquer problem
    | trivial problem   = direct problem
    | otherwise         = conquer problem (subprob1, subprob2) (subsol1, subsol2)
                            where
                                (subprob1, subprob2) = divide problem
                                subsol1 = divideAndConquer trivial direct divide conquer subprob1
                                subsol2 = divideAndConquer trivial direct divide conquer subprob2


-- Merge sort implementation
trivial :: [a] -> Bool
trivial x = length x <= 1

direct :: [a] -> [a]
direct = id

-- Auxiliary function to get the index that points to the middle of a list (floor rounding)
middle :: [a] -> Int
middle x = div (length x) 2

divide :: [a] -> ([a], [a])
divide array = (take (middle array) array, drop (middle array) array)


-- Implementation using tracing to debug at each step (needs Debug.Trace)
conquerDebug :: (Ord a, Show a) => [a] -> ([a], [a]) -> ([a], [a]) -> [a]
conquerDebug _ _ ([], []) = []
conquerDebug problem subprobs ((x:xs), []) = traceShow ("Case 1", x, xs) $ traceShowId (x : (conquerDebug problem subprobs (xs, [])))
conquerDebug problem subprobs ([], (y:ys)) = traceShow ("Case 2", y, ys) $ traceShowId (y : (conquerDebug problem subprobs ([], ys)))
conquerDebug problem subprobs ((x:xs), (y:ys)) = traceShow ("Case 3", next, x, xs, y, ys) $ traceShowId (next : nextCall)
    where
        next = if x <= y then x else y
        nextCall
            | x <= y    = conquerDebug problem subprobs (xs, y:ys)
            | otherwise = conquerDebug problem subprobs (x:xs, ys)

mergeSortDebug :: (Ord a, Show a) => [a] -> [a]
mergeSortDebug = divideAndConquer trivial direct divide conquerDebug

-- Regular implementation
conquer :: Ord a => [a] -> ([a], [a]) -> ([a], [a]) -> [a]
conquer _ _ ([], []) = []
conquer problem subprobs ((x:xs), []) = x : (conquer problem subprobs (xs, []))
conquer problem subprobs ([], (y:ys)) = y : (conquer problem subprobs ([], ys))
conquer problem subprobs ((x:xs), (y:ys))
    | x <= y        = x : (conquer problem subprobs (xs, y:ys))
    | otherwise     = y : (conquer problem subprobs (x:xs, ys))

mergeSort :: Ord a => [a] -> [a]
mergeSort = divideAndConquer trivial direct divide conquer
