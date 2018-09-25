import Control.Comonad

data Memory a = Memory [a] a [a] deriving Show

instance Functor Memory where
  fmap f (Memory l p r) = Memory (fmap f l) (f p) (fmap f r)

instance Comonad Memory where
  extract (Memory _ p _) = p
  duplicate m = Memory (tail $ iterate decrement m) m (tail $ iterate increment m)
  -- extend f == fmap f . duplicate

store :: Memory a -> a -> Memory a
store (Memory l _ r) p = Memory l p r


--Aliases
write :: Memory a -> a -> Memory a
write = store

load :: Memory a -> a
load = extract

increment :: Memory a -> Memory a
increment (Memory ls p (r:rs)) = Memory (p:ls) r rs
increment (Memory ls p []) = (Memory ls p [])

decrement :: Memory a -> Memory a
decrement (Memory (l:ls) dp rs) = Memory ls l (dp:rs)
decrement (Memory [] dp rs) = (Memory [] dp rs)

memEnd :: Memory a -> Bool
memEnd (Memory _ _ []) = True
memEnd _ = False


