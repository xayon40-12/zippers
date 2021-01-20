module Tree where

data Tree a = E | T (Tree a) a (Tree a) deriving (Eq,Show)
data DTree a = LTree a (Tree a) | RTree a (Tree a) deriving (Eq,Show)

type ZTree a = (Tree a,[DTree a])

goLeft :: ZTree a -> Maybe (ZTree a)
goLeft (E,_) = Nothing
goLeft (T l x r,ds) = Just (l,RTree x r:ds)

goRight :: ZTree a -> Maybe (ZTree a)
goRight (E,_) = Nothing
goRight (T l x r,ds) = Just (r,LTree x l:ds)

goUp :: ZTree a -> Maybe (ZTree a)
goUp (_,[]) = Nothing
goUp (r,LTree x l:ds) = Just (T l x r,ds)
goUp (l,RTree x r:ds) = Just (T l x r,ds)

attach :: Tree a -> ZTree a -> ZTree a
attach t (_,ds) = (t,ds)
mattach t = Just . attach t

add :: a -> ZTree a -> ZTree a
add x (_,ds) = (T E x E,ds)
madd x = Just . add x

goTop :: ZTree a -> ZTree a
goTop zt = maybe zt goTop (goUp zt)
mgoTop = Just . goTop
