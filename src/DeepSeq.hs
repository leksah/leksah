module DeepSeq where

class DeepSeq a where
    deepSeq :: a -> b -> b

infixr 0 `deepSeq`, $!!

($!!) :: (DeepSeq a) => (a -> b) -> a -> b
f $!! x = x `deepSeq` f x

instance  DeepSeq ()  where  deepSeq = seq

instance  DeepSeq Bool  where  deepSeq = seq
instance  DeepSeq Char  where  deepSeq = seq

instance  (DeepSeq a) => DeepSeq (Maybe a)  where
    deepSeq Nothing  y = y
    deepSeq (Just x) y = deepSeq x y

instance  (DeepSeq a, DeepSeq b) => DeepSeq (Either a b)  where
    deepSeq (Left  a) y = deepSeq a y
    deepSeq (Right b) y = deepSeq b y

instance  DeepSeq Ordering  where  deepSeq = seq

instance  DeepSeq Int       where  deepSeq = seq
instance  DeepSeq Integer   where  deepSeq = seq
instance  DeepSeq Float     where  deepSeq = seq
instance  DeepSeq Double    where  deepSeq = seq

instance  DeepSeq (a -> b)  where  deepSeq = seq

instance  DeepSeq (IO a)  where  deepSeq = seq

instance  (DeepSeq a) => DeepSeq [a]  where
    deepSeq []     y = y
    deepSeq (x:xs) y = deepSeq x $ deepSeq xs y

instance  (DeepSeq a,DeepSeq b) => DeepSeq (a,b)  where
    deepSeq (a,b)            = deepSeq a $ deepSeq b

instance  (DeepSeq a,DeepSeq b,DeepSeq c) => DeepSeq (a,b,c)  where
    deepSeq (a,b,c)          = deepSeq a $ deepSeq b $ deepSeq c

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d) => DeepSeq (a,b,c,d)  where
    deepSeq (a,b,c,d)        = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e) => DeepSeq (a,b,c,d,e)  where
    deepSeq (a,b,c,d,e)      = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f) => DeepSeq (a,b,c,d,e,f)  where
    deepSeq (a,b,c,d,e,f)    = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e $ deepSeq f

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f,DeepSeq g) => DeepSeq (a,b,c,d,e,f,g)  where
    deepSeq (a,b,c,d,e,f,g)  = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e $ deepSeq f $ deepSeq g





