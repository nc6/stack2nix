module Distribution.Nixpkgs.Haskell.Stack where

import Control.Lens
import Data.Text as T
import Distribution.Compiler as Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.FromCabal as FromCabal
import Distribution.Nixpkgs.Haskell.PackageSourceSpec as PackageSourceSpec
import Distribution.PackageDescription as PackageDescription
import Distribution.System as System
import Network.URI as URI
import Stack.Config


newtype HackageDb = HackageDb { fromHackageDB :: T.Text }
  deriving (Eq, Ord, Show)

makePrisms ''HackageDb

unHackageDb :: HackageDb -> String
unHackageDb = T.unpack . fromHackageDB

data StackPackagesConfig = StackPackagesConfig
  { _spcHaskellResolver   :: !HaskellResolver
  , _spcNixpkgsResolver   :: !NixpkgsResolver
  , _spcTargetPlatform    :: !Platform
  , _spcTargetCompiler    :: !CompilerInfo
  , _spcFlagAssignment    :: !FlagAssignment
  , _spcDoCheckPackages   :: !Bool
  , _spcDoHaddockPackages :: !Bool
  , _spcDoCheckStackage   :: !Bool
  , _spcDoHaddockStackage :: !Bool
  }

makeLenses ''StackPackagesConfig

getStackPackageFromDb
  :: Maybe HackageDb
  -> StackPackage
  -> IO Package
getStackPackageFromDb optHackageDb stackPackage =
  PackageSourceSpec.getPackage
    (unHackageDb <$> optHackageDb)
    (stackLocationToSource $ stackPackage ^. spLocation)

stackLocationToSource :: PackageLocation -> Source
stackLocationToSource = \case
  HackagePackage p -> Source
    { sourceUrl      = "cabal://" ++ T.unpack p
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = mempty }
  StackFilePath p  -> Source
    { sourceUrl      = p
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = mempty }
  StackUri uri     -> Source
    { sourceUrl      = URI.uriToString id uri mempty
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = mempty }
  StackRepo r      -> Source
    { sourceUrl      = T.unpack $ r ^. rUri
    , sourceRevision = T.unpack $ r ^. rCommit
    , sourceHash     = UnknownHash
    , sourceCabalDir = mempty }

packageDerivation
  :: StackPackagesConfig
  -> Maybe HackageDb
  -> StackPackage
  -> IO Derivation
packageDerivation conf optHackageDb stackPackage = do
  pkg <- getStackPackageFromDb optHackageDb stackPackage
  let drv = genericPackageDerivation conf pkg & src .~ pkgSource pkg
  return $ if stackPackage ^. spExtraDep
    then drv
      & doCheck &&~ conf ^. spcDoCheckStackage
      & runHaddock &&~ conf ^. spcDoHaddockStackage
    else drv
      & doCheck &&~ conf ^. spcDoCheckPackages
      & runHaddock &&~ conf ^. spcDoHaddockPackages

genericPackageDerivation
  :: StackPackagesConfig
  -> Package
  -> Derivation
genericPackageDerivation conf pkg =
  FromCabal.fromGenericPackageDescription
    (conf ^. spcHaskellResolver)
    (conf ^. spcNixpkgsResolver)
    (conf ^. spcTargetPlatform)
    (conf ^. spcTargetCompiler)
    (conf ^. spcFlagAssignment)
    []
    (pkgCabal pkg)
