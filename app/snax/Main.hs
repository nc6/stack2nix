{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show
{-# LANGUAGE TypeOperators      #-}

import Distribution.Nixpkgs.Haskell.Snax.Lib

import Options
import Options.Generic (unwrapRecord)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(..), hPutStrLn, hPrint)

main :: IO ()
main = do
  x <- unwrapRecord "Snax"
  run x

run :: Options -> IO ()
run (Init{..}) = do
  createDirectoryIfMissing True libPath
  writeRepo (allCabalHashes _allCabalHashes)
  writeRepo (ltsHaskell _ltsHaskell)
  withFile (libPath </> "fetch-repo.nix") WriteMode $ \h -> do
    hPutStrLn h ("# Generated by snax")
    hPrint h fetchRepo

  withFile (libPath </> "default.nix") WriteMode $ \h -> do
    hPutStrLn h ("# Generated by snax")
    hPrint h libNix
