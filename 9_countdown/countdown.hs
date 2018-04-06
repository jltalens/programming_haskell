module Main where

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak expr = "(" ++ show expr ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys]

solution :: Expr -> [Int] -> Int -> Bool
solution expr ns n = elem (values expr) (choices ns) && eval expr == [n]

e :: Expr
e = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))


split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls, rs) | (ls, rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [ex | (ls, rs) <- split ns,
             l <- exprs ls,
             r <- exprs rs,
             ex <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [ex | ns' <- choices ns, ex <- exprs ns', eval ex == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
              lx <- results ls,
              ry <- results rs,
              res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [expr | ns' <- choices ns, (expr,m) <- results ns', m == n]

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys) = if x == y then ys else y : remove x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = elem x ys && isChoice xs (remove x ys)


main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)
