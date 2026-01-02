module SkewHeap (SkewHeap,empty,insert,peek,pop) where

data SkewHeap k v = SH { left :: !(SkewHeap k v), key :: !k, val :: v,
                         right :: SkewHeap k v } | Nil

keyval :: SkewHeap k v -> (k,v)
keyval h = (key h,val h)

empty :: SkewHeap k v
empty = Nil

merge :: Ord k => SkewHeap k v -> SkewHeap k v -> SkewHeap k v
merge p Nil = p
merge Nil q = q
merge p q
  | key p > key q = merge q p
  | otherwise = SH (merge (right p) q) (key p) (val p) (left p)

insert :: Ord k => (k,v) -> SkewHeap k v -> SkewHeap k v
insert (k,v) h = merge h $ SH Nil k v Nil

peek :: Ord k => SkewHeap k v -> Maybe (k,v)
peek Nil = Nothing
peek h = Just $ keyval h

pop :: Ord k => SkewHeap k v -> Maybe ((k,v),SkewHeap k v)
pop Nil = Nothing
pop h = Just $ (keyval h,merge (left h) (right h))
