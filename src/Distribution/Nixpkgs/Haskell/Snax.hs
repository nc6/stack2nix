{-# LANGUAGE DeriveGeneric #-}

-- | Snax infrastructure.
module Distribution.Nixpkgs.Haskell.Snax where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Distribution.Nixpkgs.Haskell.Packages.PrettyPrinting as PP

import GHC.Generics (Generic)

import Language.Nix.PrettyPrinting as PP

import System.FilePath ((</>), (<.>))

libPath :: FilePath
libPath = ".snax"

-- | Repository configuration
data Repo = Repo
  { owner :: String
  , repo :: String
  , rev :: String
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Repo

instance Aeson.ToJSON Repo

-- | .stax/lib/fetch-repo.nix
fetchRepo :: Doc
fetchRepo =
  vcat
    [ funargs ["repoFile"]
    , "let"
    , nest 2 $ attr "repo" "builtins.fromJSON (builtins.readFile repoFile)"
    , "in builtins.fetchTarball {"
    , nest 2 $
      attr
        "url"
        "https://github.com/${repo.owner}/${repo.repo}/tarball/${repo.rev}"
    , "}"
    ]

writeRepo :: Repo -> IO ()
writeRepo r = LBS.writeFile (libPath </> repo r <.> ".json") (Aeson.encode r)

allCabalHashes :: String -> Repo
allCabalHashes revision = Repo {
    owner = "commercialhaskell"
  , repo = "all-cabal-hashes"
  , rev = revision
  }

ltsHaskell :: String -> Repo
ltsHaskell revision = Repo {
    owner = "fpco"
  , repo = "lts-haskell"
  , rev = revision
  }

-- | .stax/lib/default.nix
libNix :: Doc
libNix =
  vcat
    [ hsep [funarg "self", funarg "super"]
    , ""
    , "with super;"
    , "{"
    , nest 2 $
      vcat
        [ "all-cabal-hashes = callPackage \"./fetch-repo.nix\" ./all-cabal-hashes.json;"
        , "lts-haskell = callPackage \"./fetch-repo.nix\" ./lts-haskell.json;"
        , "stack-resolver = runCommand \"extract-stack-resolver\" {} ''"
        , nest 2 $ vcat
          [ "snax extract-resolver > $out" ]
        , "'';"
        , "stackage-packages = runCommand \"generate-stackages\" { stack-resolver } ''"
        , nest 2 $ vcat
          [ "snax extract-resolver > $out" ]
        , "'';"
        ]
    , "}"
    ]
