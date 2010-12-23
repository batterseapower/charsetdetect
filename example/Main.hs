import System.Environment
import System.Exit
import System.IO

import Codec.Text.Detect

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
    args <- getArgs
    bs <- case args of
      [fp] -> L.readFile fp
      []   -> L.getContents
    
    case detectEncodingName bs of
      Nothing       -> hPutStrLn stderr "Could not detect encoding" >> exitWith (ExitFailure 1)
      Just encoding -> putStrLn encoding
