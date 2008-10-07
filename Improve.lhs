The flag -fallow-undecidable-instances is only required for the Show instances at the very end of this file, meant to make experimenting with the other functions easier.

> {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

> module Improve where 

Some imports, with appropriate hiding clauses for names we reuse.

> import Prelude hiding (abs, getChar, putChar, fail)
> import qualified Prelude
> import Control.Monad hiding (fail)


Section 2.

> data Tree a = Leaf a | Node (Tree a) (Tree a)  deriving Show

> subst :: Tree a -> (a -> Tree b) -> Tree b
> subst (Leaf a)     k = k a
> subst (Node t1 t2) k = Node (subst t1 k) (subst t2 k)

> instance Monad Tree where
>   return = Leaf
>   (>>=)  = subst

> fullTree :: Int -> Tree Int
> fullTree 1     = Leaf 1
> fullTree (n+1) = 
>   do
>    i <- fullTree n
>    Node (Leaf (n-i)) (Leaf (i+1))

> zigzag :: Tree Int -> Int
> zigzag = zig
>   where
>    zig (Leaf n)     = n
>    zig (Node t1 t2) = zag t1
>    zag (Leaf n)     = n
>    zag (Node t1 t2) = zig t2

> newtype CTree a = CTree (forall b. (a -> Tree b) -> Tree b)

> rep_Tree ::  Tree a -> CTree a
> rep_Tree t = CTree (subst t)
> 
> abs_Tree :: CTree a -> Tree a
> abs_Tree (CTree p) = p Leaf

> instance Monad CTree where
>   return a      = CTree (\h -> h a)
>   CTree p >>= k = CTree (\h -> p (\a -> case k a of CTree q -> q h))

> class Monad m => TreeLike m where
>   node :: m a -> m a -> m a

> instance TreeLike Tree where
>   node = Node

> instance TreeLike CTree where
>   node (CTree p1) (CTree p2) = CTree (\h -> Node (p1 h) (p2 h))

> leaf :: TreeLike m => a -> m a
> leaf = return

> fullTree' :: TreeLike m => Int -> m Int
> fullTree' 1     = leaf 1
> fullTree' (n+1) = 
>   do 
>    i <- fullTree' n
>    node (leaf (n-i)) (leaf (i+1))

> improve_Tree :: (forall m. TreeLike m => m a) -> Tree a
> improve_Tree m = abs_Tree m

> fullTree'' :: Int -> (Int -> Tree b) -> Tree b
> fullTree'' 1 h = h 1
> fullTree'' (n+1) h = fullTree'' n (\i -> Node (h (n-i)) (h (i+1)))


Section 3.

> data Free f a = Return a | Wrap (f (Free f a))
> 
> instance Functor f => Monad (Free f) where
>   return         = Return
>   Return a >>= k = k a
>   Wrap t   >>= k = Wrap (fmap (>>= k) t)

> newtype C m a = C (forall b. (a -> m b) -> m b)
> 
> rep :: Monad m => m a -> C m a
> rep m = C (m >>=)
> 
> abs :: Monad m => C m a -> m a
> abs (C p) = p return
> 
> instance Monad (C m) where
>   return a  = C (\h -> h a)
>   C p >>= k = C (\h -> p (\a -> case k a of C q -> q h))

> class (Functor f, Monad m) => FreeLike f m where
>   wrap :: f (m a) -> m a

> instance Functor f => FreeLike f (Free f) where
>   wrap = Wrap

> instance FreeLike f m => FreeLike f (C m) where
>   wrap t = C (\h -> wrap (fmap (\(C p) -> p h) t))

> improve :: Functor f => (forall m. FreeLike f m => m a) -> Free f a
> improve m = abs m

> data F b = N b b  deriving Show
> 
> instance Functor F where
>   fmap h (N x y) = N (h x) (h y)


Section 4.

> data F_IO b = GetChar (Char -> b) | PutChar Char b  deriving Show
> 
> instance Functor F_IO where
>   fmap h (GetChar f)   = GetChar (h . f)
>   fmap h (PutChar c x) = PutChar c (h x)

> getChar :: FreeLike F_IO m => m Char
> getChar = wrap (GetChar return)
> 
> putChar :: FreeLike F_IO m => Char -> m ()
> putChar c = wrap (PutChar c (return ()))

> revEcho :: FreeLike F_IO m => m ()
> revEcho = 
>   do 
>    c <- getChar
>    when (c /= ' ') $
>     do
>      revEcho
>      putChar c

> data Output a = Read (Output a) | Print Char (Output a) | Finish a  deriving Show
> 
> data Stream a = Cons {hd :: a, tl :: Stream a}  deriving Show
> 
> run :: Free F_IO a -> Stream Char -> Output a
> run (Return a)           cs = Finish a
> run (Wrap (GetChar f))   cs = Read (run (f (hd cs)) (tl cs))
> run (Wrap (PutChar c p)) cs = Print c (run p cs)


Section 5.1.

> exec :: Free F_IO a -> IO a
> exec (Return a)           = return a
> exec (Wrap (GetChar f))   = Prelude.getChar >>= (exec . f)
> exec (Wrap (PutChar c p)) = Prelude.putChar c >> exec p


Section 5.2.

> data Expr = Add Expr Expr | Div Expr Expr | Lit Int  deriving Show
> 
> eval (Add e1 e2) =
>   do
>    x <- eval e1
>    y <- eval e2
>    return (x+y)
> eval (Div e1 e2) =
>   do
>    y <- eval e2
>    if y == 0 then fail "division by zero" else
>     do
>      x <- eval e1
>      return (div x y)
> eval (Lit i) = return i

> data F_Exc b = Fail String  deriving Show
> 
> instance Functor F_Exc where
>   fmap h (Fail s) = Fail s
> 
> fail s = wrap (Fail s)

> deep n = foldl Add (Div (Lit 1) (Lit 0)) (map Lit [2..n])


Auxiliary Show instances.

> instance (Functor f, Show a, Show (f String)) => Show (Free f a) where
>   show (Return a) = "Return " ++ show a
>   show (Wrap t)   = "Wrap (" ++ trans (show (fmap show t)) ++ ")"
>     where
>      trans []     = []
>      trans (c:cs)
>       | c == '"'  = '(':trans' cs
>       | otherwise = c:trans cs
>      trans' []     = []
>      trans' (c:cs)
>       | c == '"'  = ')':trans cs
>       | otherwise = c:trans' cs
> 
> instance Show b => Show (Char -> b) where
>   show f = '{':drop 2 (concat ["; " ++ show c ++ " -> " ++ show (f c) | c <- " abc"]) ++ "}"
