-- Haskell - Expressions lògiques
-- https://jutge.org/problems/P41359_ca
-- P41359_ca:std::RunHaskell
-- Created on 10/30/2025, 4:38:39 PM by Daniel Gallardo Peña

data LogicExpression 
    = Or  LogicExpression LogicExpression
    | And LogicExpression LogicExpression
    | Not LogicExpression
    | Var String
    | Val Bool
    deriving (Read)

instance Show LogicExpression where
    show (Or left right)    = "(" ++ show left ++ " or " ++ show right ++ ")"
    show (And left right)   = "(" ++ show left ++ " and " ++ show right ++ ")"
    show (Not expr)         = "(not " ++ show expr ++ ")"
    show (Var var)          = var
    show (Val val)          = show (if val then 1 else 0)

pushNegations :: LogicExpression -> LogicExpression
-- Eliminate double negation first
pushNegations (Not (Not expr)) = pushNegations expr

-- De Morgan's laws
pushNegations (Not (Or left right)) = And (pushNegations (Not left)) (pushNegations (Not right))
pushNegations (Not (And left right)) = Or (pushNegations (Not left)) (pushNegations (Not right))

-- Simplify negated values immediately
pushNegations (Not (Val val)) = Val (not val)

-- Recursively process Or/And
pushNegations (Or left right) = Or (pushNegations left) (pushNegations right)
pushNegations (And left right) = And (pushNegations left) (pushNegations right)

-- Base cases: Var, Not (Var ...), Val
pushNegations expr = expr

bits :: [[[Int]]]
bits = iterate (\bs -> (:) <$> [0,1] <*> bs) [[]] 

main :: IO ()
main = do
    contents <- getContents
    let parse expr = (read expr) :: LogicExpression 
    mapM_ (putStrLn . show . parse) (lines contents)