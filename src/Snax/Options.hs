{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE StrictData         #-}

module Snax.Options where

import Control.Lens
import Control.Monad (join)

import qualified Data.Text as T
import Options.Generic

import Stack.Types (StackYaml(..))
import System.FilePath (splitFileName)

type Options = Options' Options.Generic.Unwrapped

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
    , _bootstrap :: w ::: Maybe FilePath
      <?> "Bootstrap stackage2nix using an existing .nix file."
    }
  | ExtractResolver
    { _stackYaml :: w ::: Maybe FilePath
      <?> "Path to a specific stack.yaml."
    }
  | GenerateStackage
    { _resolver :: w ::: T.Text
      <?> "Stack resolver to generate derivations for."
    , _doCheckStackage :: w ::: Bool
      <?> "Enable tests for stackage packages."
    , _doHaddockStackage :: w ::: Bool
      <?> "Enable haddock for stackage packages."
    , _nixpkgsRepository :: w ::: Maybe FilePath
      <?> "Path to custom nixpkgs."
    , _allCabalHashesPath :: w ::: FilePath
      <?> "Path to all-cabal-hashes repo."
    , _ltsHaskellPath :: w ::: FilePath
      <?> "Path to lts-haskell repo."
    }
  | GeneratePackages
    { _stackYaml :: w ::: Maybe FilePath
      <?> "Path to a specific stack.yaml."
    }
  deriving Generic

makeLenses ''Options'

instance ParseRecord (Options' Options.Generic.Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

stackYamlParsed :: Getter Options StackYaml
stackYamlParsed = pre stackYaml
                . to join
                . non "./stack.yaml" . to mkStackYaml

mkStackYaml :: FilePath -> StackYaml
mkStackYaml p = case splitFileName p of
  (dir, "")   -> StackYaml dir "stack.yaml"
  (dir, ".")  -> StackYaml dir "stack.yaml"
  (_  , "..") -> StackYaml p "stack.yaml"
  (dir, file) -> StackYaml dir file

deriving instance Show (Options' Options.Generic.Unwrapped)
