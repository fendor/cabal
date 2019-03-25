-- |
-- This module defines a simple JSON-based format for exporting basic
-- information about a Cabal package and the compiler configuration Cabal
-- would use to build it. This can be produced with the @cabal show-build-info@
-- command.
--
-- This format is intended for consumption by external tooling and should
-- therefore be rather stable. Moreover, this allows tooling users to avoid
-- linking against Cabal. This is an important advantage as direct API usage
-- tends to be rather fragile in the presence of user-initiated upgrades of
-- Cabal.
--
-- Below is an example of the output this module produces,
--
-- @
-- { "cabal_version": "1.23.0.0",
--   "compiler": {
--     "flavor": "GHC",
--     "compiler_id": "ghc-7.10.2",
--     "path": "/usr/bin/ghc",
--   },
--   "components": [
--     { "type": "library",
--       "name": "CLibName",
--       "compiler_args":
--         ["-O", "-XHaskell98", "-Wall",
--          "-package-id", "parallel-3.2.0.6-b79c38c5c25fff77f3ea7271851879eb"]
--       "modules": ["Project.ModA", "Project.ModB", "Paths_project"],
--       "source_files": [],
--       "source_dirs": ["src"]
--     }
--   ]
-- }
-- @
--
-- The @cabal_version@ property provides the version of the Cabal library
-- which generated the output. The @compiler@ property gives some basic
-- information about the compiler Cabal would use to compile the package.
--
-- The @components@ property gives a list of the Cabal 'Component's defined by
-- the package. Each has,
--
-- * @type@: the type of the component (one of @library@, @executable@,
--   @test-suite@, or @benchmark@)
-- * @name@: a string serving to uniquely identify the component within the
--   package.
-- * @compiler_args@: the command-line arguments Cabal would pass to the
--   compiler to compile the component
-- * @modules@: the modules belonging to the component
-- * @source_dirs@: a list of directories where the modules might be found
-- * @source_files@: any other Haskell sources needed by the component
--
-- Note: At the moment this is only supported when using the GHC compiler.
--

module Distribution.Simple.ShowBuildInfo (mkBuildInfo) where

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.Program.GHC as GHC

import Distribution.PackageDescription
import Distribution.Compiler
import Distribution.Verbosity
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Simple.Utils.Json
import Distribution.Types.TargetInfo
import Distribution.Text
import Distribution.Pretty

-- | Construct a JSON document describing the build information for a package
mkBuildInfo :: PackageDescription  -- ^ Mostly information from the .cabal file
            -> LocalBuildInfo      -- ^ Configuration information
            -> BuildFlags          -- ^ Flags that the user passed to build
            -> [TargetInfo]
            -> Json
mkBuildInfo pkg_descr lbi _flags targetsToBuild = info
  where
    componentsToBuild = map (\target -> (componentLocalName $ targetCLBI target,targetCLBI target)) targetsToBuild
    (.=) :: String -> Json -> (String, Json)
    k .= v = (k, v)

    info = JsonObject
      [ "cabal_version" .= JsonString (display cabalVersion)
      , "compiler" .= mkCompilerInfo
      , "components" .= JsonArray (map mkComponentInfo componentsToBuild)
      ]

    mkCompilerInfo = JsonObject
      [ "flavour" .= JsonString (prettyShow $ compilerFlavor $ compiler lbi)
      , "compiler_id" .= JsonString (showCompilerId $ compiler lbi)
      , "path" .= path
      ]
      where
        path = maybe JsonNull (JsonString . programPath)
               $ lookupProgram ghcProgram (withPrograms lbi)

    mkComponentInfo (name, clbi) = JsonObject
      [ "type" .= JsonString compType
      , "name" .= JsonString (prettyShow name)
      , "compiler_args" .= JsonArray (map JsonString $ getCompilerArgs bi lbi clbi)
      , "modules" .= JsonArray (map (JsonString . display) modules)
      , "source_files" .= JsonArray (map JsonString source_files)
      , "source_dirs" .= JsonArray (map JsonString $ hsSourceDirs bi)
      ]
      where
        bi = componentBuildInfo comp
        Just comp = lookupComponent pkg_descr name
        compType = case comp of
          CLib _     -> "library"
          CExe _     -> "executable"
          CTest _    -> "test-suite"
          CBench _   -> "benchmark"
          CFLib _    -> "foreign-library"
        modules = case comp of
          CLib lib -> explicitLibModules lib
          CExe exe -> exeModules exe
          _        -> []
        source_files = case comp of
          CLib _   -> []
          CExe exe -> [modulePath exe]
          _        -> []

-- | Get the command-line arguments that would be passed
-- to the compiler to build the given component.
getCompilerArgs :: BuildInfo
                -> LocalBuildInfo
                -> ComponentLocalBuildInfo
                -> [String]
getCompilerArgs bi lbi clbi =
  case compilerFlavor $ compiler lbi of
      GHC   -> ghc
      GHCJS -> ghc
      c     -> error $ "ShowBuildInfo.getCompilerArgs: Don't know how to get "++
                       "build arguments for compiler "++show c
  where
    -- This is absolutely awful
    ghc = GHC.renderGhcOptions (compiler lbi) (hostPlatform lbi) baseOpts
      where
        baseOpts = GHC.componentGhcOptions normal lbi bi clbi (buildDir lbi)
