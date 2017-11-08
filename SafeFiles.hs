{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SafeFiles where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
import ParameterisedMonad

-- Import the
import qualified Prelude as P
import qualified System.IO as UIO

import GHC.TypeLits


newtype SafeFiles pre post a = SafeFiles { unSafeFiles :: IO a }
data (n :: Nat) :* (opens :: [Nat])

instance PMonad SafeFiles where
   return = SafeFiles . P.return
   (SafeFiles m) >>= k = SafeFiles (m P.>>= (unSafeFiles . k))


newtype SafeHandle (n :: Nat) = SafeHandle { unsafeHandle :: UIO.Handle }

-- openFile :: FilePath -> IOMode -> IO Handle
openFile :: UIO.FilePath -> UIO.IOMode -> SafeFiles (n :* opens) ((n + 1) :* (n ': opens)) (SafeHandle n)
openFile f mode = SafeFiles $ fmap SafeHandle (UIO.openFile f mode)

-- hClose :: Handle -> IO ()
hClose :: Member m opens => SafeHandle m -> SafeFiles (n :* opens) (n :* Delete m opens) ()
hClose (SafeHandle h) = SafeFiles (UIO.hClose h)

type family Delete (n :: Nat) (ns :: [Nat]) where
            Delete n '[] = '[]
            Delete n (n ': ns) = ns
            Delete n (m ': ns) = m ': Delete n ns

class Member (x :: Nat) (xs :: [Nat]) where
instance {-# OVERLAPS #-} Member x (x ': xs) where
instance Member x xs => Member x (y ': xs)


-- hGetChar :: Handle -> IO Char
hGetChar :: Member m opens => SafeHandle m -> SafeFiles (n :* opens) (n :* opens) Char
hGetChar (SafeHandle h) = SafeFiles (UIO.hGetChar h)

-- hPutChar :: Handle -> Char -> IO ()
hPutChar :: Member m opens => SafeHandle m -> Char -> SafeFiles (n :* opens) (n :* opens) ()
hPutChar (SafeHandle h) c = SafeFiles (UIO.hPutChar h c)

hIsEOF :: SafeHandle m -> SafeFiles (n :* opens) (n :* opens) Bool
hIsEOF (SafeHandle h) = SafeFiles (UIO.hIsEOF h)

-- Don't need these any more
-- hIsOpen :: Handle -> IO Bool
-- hIsClosed :: Handle -> IO Bool

runSafeFiles :: SafeFiles (0 :* '[]) (n :* '[]) () -> IO ()
runSafeFiles = unSafeFiles


example :: IO ()
example = runSafeFiles $ do
  h  <- openFile "foo" UIO.ReadWriteMode
  h' <- openFile "bar" UIO.ReadWriteMode
  x  <- hGetChar h
  hPutChar h' x
  hClose h
  hClose h'

example2 :: IO ()
example2 = runSafeFiles $ do
  h1  <- openFile "foo" UIO.ReadWriteMode
  h2 <- openFile "bar" UIO.ReadWriteMode
  loopy h1 h2
 where
  loopy h1 h2 = do
   isEmpty <- hIsEOF h1
   if isEmpty
     then do
       hClose h1
       hClose h2
     else do
       x <- hGetChar h1
       hPutChar h2 x
       loopy h1 h2
