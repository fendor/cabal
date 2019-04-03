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

module Distribution.Client.ShowBuildInfo (showBuildInfoAction, showBuildInfo, showBuildInfoCommand) where

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.Program.GHC as GHC

import Distribution.PackageDescription
import Distribution.Compiler
import Distribution.Verbosity
import Distribution.Simple (getBuildConfig)
import Distribution.Simple.BuildTarget
import Distribution.Simple.Compiler
import Distribution.Simple.Command
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup 
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils (cabalVersion, wrapText)
import Distribution.Compat.Graph (nodeKey)
import Distribution.Client.Utils.Json as Json
import Distribution.Types.TargetInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Text
import Distribution.Pretty

showBuildInfoAction :: UserHooks -> BuildFlags -> Args -> IO ()
showBuildInfoAction hooks flags args = do
  distPref <- findDistPrefOrDefault (buildDistPref flags)
  let verbosity = fromFlag $ buildVerbosity flags
      flags' = flags { buildDistPref = toFlag distPref }

  lbi <- getBuildConfig hooks verbosity distPref
  progs <- reconfigurePrograms verbosity
             (buildProgramPaths flags')
             (buildProgramArgs flags')
             (withPrograms lbi)

  pbi <- preBuild hooks args flags'
  let lbi' = lbi { withPrograms = progs }
      pkg_descr0 = localPkgDescr lbi'
      pkg_descr = updatePackageDescription pbi pkg_descr0
  -- TODO: Somehow don't ignore build hook?
  showBuildInfo pkg_descr lbi' flags

  postBuild hooks args flags' pkg_descr lbi'

showBuildInfo :: PackageDescription  -- ^ Mostly information from the .cabal file
  -> LocalBuildInfo      -- ^ Configuration information
  -> BuildFlags          -- ^ Flags that the user passed to build
  -> IO ()
showBuildInfo pkg_descr lbi flags = do
  let verbosity = fromFlag (buildVerbosity flags)
  targets <- readTargetInfos verbosity pkg_descr lbi (buildArgs flags)
  let targetsToBuild = neededTargetsInBuildOrder' pkg_descr lbi (map nodeKey targets)
      doc = mkBuildInfo pkg_descr lbi flags targetsToBuild
  putStrLn $ Json.encodeToString doc

-- | Construct a JSON document describing the build information for a package
mkBuildInfo :: PackageDescription  -- ^ Mostly information from the .cabal file
            -> LocalBuildInfo      -- ^ Configuration information
            -> BuildFlags          -- ^ Flags that the user passed to build
            -> [TargetInfo]
            -> Value
mkBuildInfo pkg_descr lbi _flags targetsToBuild = info
  where
    componentsToBuild = map (\target -> (componentLocalName $ targetCLBI target,targetCLBI target)) targetsToBuild

    info = Json.Object
      [ "cabal_version" .= Json.String (display cabalVersion)
      , "compiler" .= mkCompilerInfo
      , "components" .= Json.Array (map mkComponentInfo componentsToBuild)
      ]

    mkCompilerInfo = Json.Object
      [ "flavour" .= Json.String (prettyShow $ compilerFlavor $ compiler lbi)
      , "compiler_id" .= Json.String (showCompilerId $ compiler lbi)
      , "path" .= path
      ]
      where
        path = maybe Json.Null (Json.String . programPath)
               $ lookupProgram ghcProgram (withPrograms lbi)

    mkComponentInfo (name, clbi) = Json.Object
      [ "type" .= Json.String compType
      , "name" .= Json.String (prettyShow name)
      , "compiler_args" .= Json.Array (map Json.String $ getCompilerArgs bi lbi clbi)
      , "modules" .= Json.Array (map (Json.String . display) modules)
      , "source_files" .= Json.Array (map Json.String source_files)
      , "source_dirs" .= Json.Array (map Json.String $ hsSourceDirs bi)
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

  

-- ------------------------------------------------------------
-- * show-build-info command flags
-- ------------------------------------------------------------

showBuildInfoCommand :: ProgramDb -> CommandUI BuildFlags
showBuildInfoCommand progDb = CommandUI
  { commandName         = "show-build-info"
  , commandSynopsis     = "Emit details about how a package would be built."
  , commandDescription  = Just $ \_ -> wrapText $
         "Components encompass executables, tests, and benchmarks.\n"
      ++ "\n"
      ++ "Affected by configuration options, see `configure`.\n"
  , commandNotes        = Just $ \pname ->
       "Examples:\n"
        ++ "  " ++ pname ++ " show-build-info      "
        ++ "    All the components in the package\n"
        ++ "  " ++ pname ++ " show-build-info foo       "
        ++ "    A component (i.e. lib, exe, test suite)\n\n"
        ++ programFlagsDescription progDb
--TODO: re-enable once we have support for module/file targets
--        ++ "  " ++ pname ++ " show-build-info Foo.Bar   "
--        ++ "    A module\n"
--        ++ "  " ++ pname ++ " show-build-info Foo/Bar.hs"
--        ++ "    A file\n\n"
--        ++ "If a target is ambiguous it can be qualified with the component "
--        ++ "name, e.g.\n"
--        ++ "  " ++ pname ++ " show-build-info foo:Foo.Bar\n"
--        ++ "  " ++ pname ++ " show-build-info testsuite1:Foo/Bar.hs\n"
  , commandUsage        = usageAlternatives "show-build-info" $
      [ "[FLAGS]"
      , "COMPONENTS [FLAGS]"
      ]
  , commandDefaultFlags = defaultBuildFlags
  , commandOptions      = \showOrParseArgs ->
      [ optionVerbosity
        buildVerbosity (\v flags -> flags { buildVerbosity = v })

      , optionDistPref
        buildDistPref (\d flags -> flags { buildDistPref = d }) showOrParseArgs
      ]
      ++ buildOptions progDb showOrParseArgs
  }
