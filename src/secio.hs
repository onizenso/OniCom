module SecIO where

import Lattice
import Sec
    -- SecIO

newtype SecIO s a = MkSecIO (IO (Sec s a))

instance Monad (SecIO s) where
    return x = MkSecIO (return (return x))
    
    MkSecIO m >>= k =
        MkSecIO (do sa <- m
                    let MkSecIO m’ = k (reveal sa)
                    m’)
    
    -- SecIO functions
    value :: Sec s a -> SecIO s a
    value sa = MkSecIO (return sa)

    run :: SecIO s a -> IO (Sec s a)
    run (MkSecIO m) = m
    
    plug :: Less sl sh => SecIO sh a -> SecIO sl (Sec sh a)
    plug ss_sh@(MkSecIO m)
        = less sl sh ‘seq‘ ss_sl
            where
                (ss_sl) = MkSecIO (do sha <- m
                                    return (sec sha))
    
    sl = unSecIOType ss_sl
    sh = unSecIOType ss_sh
    
    -- For type-checking purposes (not exported).
    unSecIOType :: SecIO s a -> s
    unSecIOType _ = undefined
    
    -- File IO
    data File s = MkFile FilePath

    readSecIO :: File s’ -> SecIO s (Sec s’ String)
    readSecIO (MkFile file) =
        MkSecIO ((sec . sec) ‘fmap‘ readFile file)

    writeSecIO :: File s’ -> String -> SecIO s ()
    writeSecIO (MkFile file) s =
        MkSecIO (sec ‘fmap‘ writeFile file s)
