{-# LANGUAGE DeriveGeneric #-}
import           Test.Cabal.Prelude

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Aeson
import           GHC.Generics

main = cabalTest $ do
    r <- cabal' "new-show-build-info" ["lib:Complex", "-v0"]
    let buildInfoEither = eitherDecodeStrict (T.encodeUtf8 . T.pack $ resultOutput r) :: Either String [BuildInfo]
    case buildInfoEither of
      Left err -> fail $ "Could not parse build-info command" ++ err
      Right buildInfos -> do
        assertEqual "Build Infos, exactly one" 1  (length buildInfos)
        let [buildInfo] = buildInfos
        assertEqual "Cabal Version" "3.0.0.0" (cabalVersion buildInfo)
        assertEqual "Compiler flavour" "ghc" (flavour $ compiler buildInfo)
        assertBool "Compiler id" (and $ zipWith (==) "ghc" (compilerId $ compiler buildInfo))
        assertBool "Compiler path non-empty" (not . null . path $ compiler buildInfo)
        assertEqual "Components, exactly one" 1 (length $ components buildInfo)
        let [component] = components buildInfo
        assertEqual "Component type" "lib" (componentType component)
        assertEqual "Component name" "lib" (componentName component)
        assertEqual "Component unit-id" "Complex-0.1.0.0-inplace" (componentUnitId component)
        assertBool "Component compiler args are non-empty" (not . null $ componentCompilerArgs component)
        assertBool "Component ghc-options contains all specified in .cabal"
                  (all
                    (`elem` componentCompilerArgs component)
                    [ "-threaded"
                    , "-rtsopts"
                    , "-with-rtsopts=-N"
                    , "-Wall"
                    ]
                  )
        assertBool "Component ghc-options does not contain -Wredundant-constraints"
                  (all
                    (`notElem` componentCompilerArgs component)
                    [ "-Wredundant-constraints"
                    ]
                  )
        assertEqual "Component modules" ["Lib", "Paths_complex"] (componentModules component)
        assertEqual "Component source files" [] (componentSrcFiles component)
        assertEqual "Component source directories" ["src"] (componentSrcDirs component)
    return ()

data BuildInfo = BuildInfo
    { cabalVersion :: String
    , compiler :: CompilerInfo
    , components :: [ComponentInfo]
    } deriving (Generic, Show)

data CompilerInfo = CompilerInfo
    { flavour :: String
    , compilerId :: String
    , path :: String
    } deriving (Generic, Show)

data ComponentInfo = ComponentInfo
    { componentType :: String
    , componentName :: String
    , componentUnitId :: String
    , componentCompilerArgs :: [String]
    , componentModules :: [String]
    , componentSrcFiles :: [String]
    , componentSrcDirs :: [String]
    } deriving (Generic, Show)

instance ToJSON BuildInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON BuildInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '-' }


instance ToJSON CompilerInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CompilerInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON ComponentInfo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ComponentInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 10 . camelTo2 '-' }