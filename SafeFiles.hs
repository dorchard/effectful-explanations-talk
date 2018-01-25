
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

-- Import qualified versions of standard code we want to wrap
import qualified Prelude as P
import qualified System.IO as IO

import GHC.TypeLits -- gives us type-level natural numbers

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



-- Just use the IO monad underneath...
instance PMonad SafeFiles where
   -- return :: a -> SafeFiles p p a
   return = SafeFiles . P.return
   -- (>>=) :: SafeFiles p q a -> (a -> SafeFiles q r b) -> SafeFiles p r b
   (SafeFiles m) >>= k = SafeFiles (m P.>>= (unSafeFiles . k))


------------------------------------------------------------------



-- Safe handlers are indexed by a (unique) number
newtype SafeHandle (n :: Nat) =
       SafeHandle { unsafeHandle :: IO.Handle }



-- Protocol states are a pair of a
--   * a type-level nat representing the next fresh handle
--   * list of open handles
data St (n :: Nat) (opens :: [Nat])



-- openFile :: FilePath -> IOMode -> IO Handle
-- Opens a file, returns a handler with a fresh name
openFile ::
    IO.FilePath
 -> IO.IOMode
 -> SafeFiles (St h opens) (St (h + 1) (h ': opens)) (SafeHandle h)
openFile f mode = SafeFiles $ fmap SafeHandle (IO.openFile f mode)


-- hGetChar :: Handle -> IO Char
hGetChar :: Member h opens =>
     SafeHandle h
  -> SafeFiles (St n opens) (St n opens) Char

hGetChar = SafeFiles . IO.hGetChar . unsafeHandle


-- Membership predicate
class Member (x :: Nat) (xs :: [Nat]) where
instance {-# OVERLAPS #-}     Member x (x ': xs) where
instance {-# OVERLAPPABLE #-} Member x xs => Member x (y ': xs)


-- hPutChar :: Handle -> Char -> IO ()
hPutChar :: Member h opens =>
     SafeHandle h
  -> Char -> SafeFiles (St n opens) (St n opens) ()

hPutChar (SafeHandle h) = SafeFiles . IO.hPutChar h


-- hClose :: Handle -> IO ()
hClose :: Member h opens =>
     SafeHandle h
  -> SafeFiles (St n opens) (St n (Delete h opens)) ()
hClose = SafeFiles . IO.hClose . unsafeHandle

-- Delete a handler name from a list
type family Delete (n :: Nat) (ns :: [Nat]) where
            Delete n '[] = '[]
            Delete n (n ': ns) = ns
            Delete n (m ': ns) = m ': Delete n ns

-- hIsEOF :: Handler -> IO Bool
hIsEOF :: Member h opens =>
  SafeHandle h -> SafeFiles (St n opens) (St n opens) Bool
hIsEOF (SafeHandle h) = SafeFiles (IO.hIsEOF h)

-- Only allow running when every file is closed at the end
runSafeFiles :: SafeFiles (St 0 '[]) (St n '[]) a -> IO a
runSafeFiles = unSafeFiles

example = runSafeFiles $ do
  h  <- openFile "foo" IO.ReadWriteMode
  h' <- openFile "bar" IO.ReadWriteMode
  x <- hGetChar h
  hPutChar h' x
  hClose h'
  hClose h
  return ()

example2 :: IO ()
example2 = runSafeFiles $ do
  h1  <- openFile "foo" IO.ReadWriteMode
  h2 <- openFile "bar" IO.ReadWriteMode
  loopy h1 h2

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
