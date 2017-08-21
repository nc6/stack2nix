{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE StrictData         #-}

module Options where

import Options.Generic

type Options = Options' Unwrapped

data Options' w
  = Init
    { _doCheckPackages :: w ::: Bool
      <?> "Enable tests for project packages."
    , _doCheckStackage :: w ::: Bool
      <?> "Enable tests for stackage packages."
    , _doHaddockPackages :: w ::: Bool
      <?> "Enable haddock for project packages."
    , _nixpkgsRepository :: w ::: Maybe FilePath
      <?> "Path to custom nixpkgs."
    , _stackYaml :: w ::: Maybe FilePath
      <?> "Path to a specific stack.yaml."
    , _allCabalHashes :: w ::: String
      <?> "Revision of all-cabal-hashes to use."
    , _ltsHaskell :: w ::: String
      <?> "Revision of lts-haskell to use."
    } deriving Generic

instance ParseRecord (Options' Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (Options' Unwrapped)
