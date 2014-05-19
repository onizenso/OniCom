module Sec where

-- Sec
newtype Sec s a = MkSec a

instance Monad (Sec s) where
    return x = sec x

sec :: a -> Sec s a
sec x = MkSec x

open :: sec s a -> s -> a
open (MkSec a) s = s 'seq' a

up :: Less s s' => Sec s a -> Sec s' a
up sec_s(MkSec a) = less s s' 'seq' sec_s'
                    where (sec_s')  = MkSec a
                            s       = unSecType sec_s
                            s'      = unSecType sec_s'

-- For type-checking, not exported
unSecType :: Sec s a -> s
unSecType _ = undefined

-- only use with trusted code
reveal :: Sec s a -> a
reveal (MkSec a) = a
