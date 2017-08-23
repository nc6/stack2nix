{-# LANGUAGE DeriveGeneric #-}

-- | Snax infrastructure.
module Distribution.Nixpkgs.Haskell.Snax where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.String (fromString)

import Distribution.Nixpkgs.Haskell.Packages.PrettyPrinting as PP

import GHC.Generics (Generic)

import qualified Language.Nix.FilePath as Nix
import Language.Nix.PrettyPrinting as PP

import System.FilePath ((</>), (<.>))

libPath :: FilePath
libPath = ".snax"

systemNixpkgs :: Doc
systemNixpkgs = "<nixpkgs>"

-- | Repository configuration
data Repo = Repo
  { owner :: String
  , repo :: String
  , rev :: String
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Repo

instance Aeson.ToJSON Repo

-- | .snax/fetch-repo.nix
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
        "\"https://github.com/${repo.owner}/${repo.repo}/tarball/${repo.rev}\""
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

-- | .snax/default.nix
libNix :: Maybe FilePath -- ^ Stack yaml
       -> Maybe FilePath -- ^ Bootstrap configuration
       -> Doc
libNix mstackYaml mbootstrap =
  vcat
    [ funargsCurried["self", "super"]
    , "with super;"
    , "let"
    , nest 2 $ vcat $
      (maybe [] (\bs -> [attr "haskellPackages" $ "callPackage " <> fromString bs <> " {}"]) mbootstrap) ++
      [ attr "stack-yaml" $ maybe "../stack.yaml" fromString mstackYaml
      , "all-cabal-hashes = callPackage ./fetch-repo.nix {repoFile = ./all-cabal-hashes.json;};"
      , "lts-haskell = callPackage ./fetch-repo.nix {repoFile = ./lts-haskell.json;};"
      , "stack-resolver = runCommand \"extract-stack-resolver\" { nativeBuildInputs = [ haskellPackages.stackage2nix ];} ''"
      , nest 2 $ vcat
        [ "${haskellPackages.stackage2nix}/bin/snax extract-resolver --stack-yaml ${stack-yaml} > $out" ]
      , "'';"
      , "stackage-packages = runCommand \"generate-stackages\" {nativeBuildInputs = [ haskellPackages.stackage2nix ];} ''"
      , nest 2 $ vcat
        [ "mkdir -p $out"
        , "cd $out"
        , "${haskellPackages.stackage2nix}/bin/snax generate-stackage \\"
        , "  --resolver $(cat ${stack-resolver}) \\"
        , "  --lts-haskell-path ${lts-haskell} \\"
        , "  --all-cabal-hashes-path ${all-cabal-hashes}"
        ]
      , "'';"
      ]
    , "in"
    , nest 2 $ "{ inherit stackage-packages; }"
    ]

-- | ./default.nix
defaultNix :: String -> Doc
defaultNix nixpkgs' = let
    nixpkgs = if fromString nixpkgs' == systemNixpkgs
      then systemNixpkgs
      else (disp . (fromString :: FilePath -> Nix.FilePath)) nixpkgs'
  in vcat
    [ funargs ["nixpkgs ? " <> nixpkgs]
    , ""
    , "import nixpkgs {"
    , nest 2 $ vcat
      [ attr "config" $ "{ allowUnfree = true; }"
      , "overlays = ["
      , nest 2 $ "(import " <> "./" <> fromString libPath <> ")"
      , "];"
      ]
    , "}"
    ]
