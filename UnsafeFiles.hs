import System.IO

example = do
    h <- openFile "foo" ReadMode
    c <- hGetChar h
    hClose h
    c' <- hGetChar h
    return [c, c']