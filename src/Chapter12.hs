module Chapter12 where
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

-- (-> a) :: func from a to ...

--instance Functor ((->) a) where
    -- fmap :: (b -> c) -> (a->b ) -> (a-> c)
--    fmap f g = f . g  

-- instance Applicative ((->) a) where
    -- pure :: b -> (a->b)
    -- pure = const
    --(<*>) :: (a -> (b->c)) -> (a -> b) -> (a ->c)
    -- g <*> f = \x -> g x (f x)    

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z . fmap g $ xs
    
instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure = Z . repeat
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z (zipWith ($) gs xs)

-- pure id <*> x = x :: f (a -> a) <*> f a = f a
-- pure (g x) = pure g <*> pure x :: f ((a -> b) -> a)  = f (a->b) <*> f a
-- x <*> pure y = pure (\g -> g y) <*> x :: f (a -> b) <*> f a = f ((a -> b) -> (a -> b) -> a ) <*> f a
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z = f (b ->c) <*> (f (a -> b) -> f a) = f ((b -> c) -> (a -> b) -> a -> c) <*> f(b -> c) <*> f (a -> b) <*> f a

--instance Monad ((->) a) where
--    -- (a -> b) >>= (b -> (a ->c)) -> (a -> c)
--   f >>= g = \x -> g (f x) x

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    fmap f (Var a) = Var (f a)
    fmap _ (Val x) = Val x
    fmap f (Add a b) = Add (fmap f a) (fmap f b)

instance Applicative Expr where
    pure = Var
    (Var f) <*> a = fmap f a
    (Add f g) <*> a = Add (f <*> a) (g <*> a)
    (Val x) <*> _ = Val x

-- example :: Expr Double with func (*2) --> (Val (*2)) <*> a 
-- Add (*2) (*2) <*> a

instance Monad Expr where
    --(>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Var a) >>= f = f a
    (Val x) >>= _ = Val x
    (Add l r) >>= f = Add (l >>= f) (r>>= f)

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) = st

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do
        x <- st
        return (g x) -- aka st >>= \x -> return g x

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))
    
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do
        f <- stf
        x <- stx
        return (f x) -- aka stf >>= \f -> stx >> \x -> return (f x)

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s ->let (x,s') = app st s in app (f x) s')
