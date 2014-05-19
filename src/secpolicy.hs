module Policies ( declassification ) where
import SecLibTypes ; import Declassification
import SpwdData

    declassification 
        = ntimes 3 (hatch (\(spwd,c) -> cypher spwd == c))
            :: IO (Hatch H L (Spwd, String) Bool)

    module Main ( main ) where
    import Policies
    import Login

    main = do match <- declassification
    login match

    module Login ( login ) where
    import SecLibTypes ; import SecLib
    import SpwdData ; import Spwd
    import Maybe

    check :: (?match :: Hatch H L (Spwd, Cypher) Bool)
            => Sec H Spwd -> String -> Int -> String
                -> IO ()
    
    check spwd pwd n u =
        do acc <- ?match ((\s -> (s, pwd)) ‘fmap‘ spwd)
            if (public (fromJust acc))
                then putStrLn "Launching shell..."
                else do putStrLn "Invalid login!"
                    auth (n-1) u spwd

    auth 0 _ spwd = return ()
    auth n u spwd = do putStr "Password:"
        pwd <- getLine
        check spwd pwd n u
        
    login match
        = do let ?match = match
            putStrLn "Welcome!"
            putStr "login:"
            u <- getLine
            src <- getSpwdName u
            case src of
                Nothing -> putStrLn "Invalid user!"
                Just spwd -> auth 3 u spwd
