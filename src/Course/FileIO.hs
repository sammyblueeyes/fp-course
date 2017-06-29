{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
-- getArgs    :: IO (List Chars)
    do 
        l <- getArgs
        case l of
            Nil -> putStrLn "Error"
            (h :. _) -> run h


type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run f =
-- readFile   :: Chars -> IO Chars
-- getFile    :: FilePath -> IO (FilePath, Chars)
-- printFiles :: List (FilePath, Chars) -> IO ()
--    do
--        l  <- (readFile f)
--        fs <-(getFiles (lines l))
--        printFiles fs
        (readFile f) >>= (\l -> 
        getFiles (lines l) >>= 
        printFiles)
        

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles l =
-- getFile :: FilePath -> IO (FilePath, Chars)
--    do 
--        x <- sequence ((<$>) (getFile) l)
--        pure (x)
--    (sequence ((<$>) (getFile) l)) >>= \x -> pure x    
--    (sequence ((<$>) (getFile) l)) >>= pure
    (sequence ((<$>) (getFile) l))
    
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile f = 
--    do 
--        x <- readFile f
--        pure (f, x)
    readFile f >>= \x -> pure(f, x)

getFile2 ::
  FilePath
  -> IO (FilePath, Chars)
getFile2 = lift2 (<$>) (,) readFile


printFiles ::
  List (FilePath, Chars)
  -> IO ()
--printFiles l = void ( sequence ((<$>) (\(f, c) -> printFile f c) l) )
--printFiles  = traverse_ (\(f, c) -> printFile f c)  
printFiles  = traverse_ (uncurry printFile)  

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile f c  = 
--    do
--        putStrLn f
--        putStrLn c
--    (void . sequence . (<$>) putStrLn) (("====" ++ f) :. c :. Nil)
    traverse_ putStrLn (("====" ++ f) :. c :. Nil)

traverse_ :: Applicative f => (a1 -> f a2) -> List a1 -> f ()
traverse_  f = void . sequence . (<$>) f

