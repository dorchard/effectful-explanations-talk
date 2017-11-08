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
import qualified System.IO as IO

import GHC.TypeLits

{-

-- openFile :: FilePath -> IOMode -> IO Handle
-- hGetChar :: Handle -> IO Char
-- hPutChar :: Handle -> Char -> IO ()
-- hClose :: Handle -> IO ()

-- hIsOpen :: Handle -> IO Bool
-- hIsClosed :: Handle -> IO Bool

-}

-- Wrap the IO monad
newtype SafeFiles pre post a = SafeFiles { unSafeFiles :: IO a }

instance PMonad SafeFiles where
   -- return :: a -> SafeFiles p p a
   return = SafeFiles . P.return
   -- (>>=) :: SafeFiles p q a -> (a -> SafeFiles q r b) -> SafeFiles p r b
   (SafeFiles m) >>= k = SafeFiles (m P.>>= (unSafeFiles . k))

-- Protocol states are a pair of a type-level nat and list of naturals
data St (n :: Nat) (opens :: [Nat])

-- Safe handlers are indexed by a (unique) number
newtype SafeHandle (n :: Nat) =
    SafeHandle { unsafeHandle :: IO.Handle }

-- openFile :: FilePath -> IOMode -> IO Handle
openFile ::
    IO.FilePath
 -> IO.IOMode
 -> SafeFiles (St n opens) (St (n + 1) (n ': opens)) (SafeHandle n)
openFile f mode = SafeFiles $ fmap SafeHandle (IO.openFile f mode)

-- hClose :: Handle -> IO ()
hClose :: Member m opens =>
     SafeHandle m
  -> SafeFiles (St n opens) (St n (Delete m opens)) ()
hClose (SafeHandle h) = SafeFiles (IO.hClose h)

-- Delete a handler name from a list
type family Delete (n :: Nat) (ns :: [Nat]) where
            Delete n '[] = '[]
            Delete n (n ': ns) = ns
            Delete n (m ': ns) = m ': Delete n ns

-- Membership predicate
class Member (x :: Nat) (xs :: [Nat]) where
instance {-# OVERLAPS #-} Member x (x ': xs) where
instance Member x xs => Member x (y ': xs)

-- hGetChar :: Handle -> IO Char
hGetChar :: Member m opens =>
     SafeHandle m
  -> SafeFiles (St n opens) (St n opens) Char
hGetChar (SafeHandle h) = SafeFiles (IO.hGetChar h)

-- hPutChar :: Handle -> Char -> IO ()
hPutChar :: Member m opens =>
     SafeHandle m
  -> Char -> SafeFiles (St n opens) (St n opens) ()
hPutChar (SafeHandle h) c = SafeFiles (IO.hPutChar h c)

hIsEOF :: SafeHandle m -> SafeFiles (St n opens) (St n opens) Bool
hIsEOF (SafeHandle h) = SafeFiles (IO.hIsEOF h)

-- Only allow running where every file is closed
runSafeFiles :: SafeFiles (St 0 '[]) (St n '[]) () -> IO ()
runSafeFiles = unSafeFiles

example :: IO ()
example = runSafeFiles $ do
  h  <- openFile "foo" IO.ReadWriteMode
  h' <- openFile "bar" IO.ReadWriteMode
  x  <- hGetChar h
  hPutChar h' x
  hClose h
  hClose h'

example2 :: IO ()
example2 = runSafeFiles $ do
  h1  <- openFile "foo" IO.ReadWriteMode
  h2 <- openFile "bar" IO.ReadWriteMode
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
