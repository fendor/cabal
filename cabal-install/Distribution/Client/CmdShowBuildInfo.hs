-- | cabal-install CLI command: build
--
module Distribution.Client.CmdShowBuildInfo (
    -- * The @build@ CLI and action
    showBuildInfoCommand,
    showBuildInfoAction
  ) where

import Distribution.Client.ProjectOrchestration

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Utils
         ( wrapText)

import qualified Data.Map as Map
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Client.SetupWrapper
import Distribution.Simple.Program ( defaultProgramDb )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.ProjectPlanning (setupHsBuildFlags, setupHsScriptOptions)
import Distribution.Client.DistDirLayout (distBuildDirectory)
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.PackageId (pkgName)
import Distribution.Client.Types ( PackageLocation(..), GenericReadyPackage(..) )
import Distribution.Client.JobControl (newLock, Lock)

showBuildInfoCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
showBuildInfoCommand = Client.installCommand {
  commandName         = "new-show-build-info",
  commandSynopsis     = "Show project build information",
  commandUsage        = usageAlternatives "new-show-build-info" [ "[TARGETS] [FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Build one or more targets from within the project. The available "
     ++ "targets are the packages in the project as well as individual "
     ++ "components within those packages, including libraries, executables, "
     ++ "test-suites or benchmarks. Targets can be specified by name or "
     ++ "location. If no target is specified then the default is to build "
     ++ "the package in the current directory.\n\n"

     ++ "Dependencies are built or rebuilt as necessary. Additional "
     ++ "configuration flags can be specified on the command line and these "
     ++ "extend the project configuration from the 'cabal.project', "
     ++ "'cabal.project.local' and other files.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-build\n"
     ++ "    Build the package in the current directory or all packages in the project\n"
     ++ "  " ++ pname ++ " new-build pkgname\n"
     ++ "    Build the package named pkgname in the project\n"
     ++ "  " ++ pname ++ " new-build ./pkgfoo\n"
     ++ "    Build the package in the ./pkgfoo directory\n"
     ++ "  " ++ pname ++ " new-build cname\n"
     ++ "    Build the component named cname module Distribution.Client.InstallPlanin the project\n"
     ++ "  " ++ pname ++ " new-build cname --module Distribution.Client.InstallPlanenable-profiling\n"
     ++ "    Build the component in profilingmodule Distribution.Client.InstallPlan mode (including dependencies as needed)\n\n"

     ++ cmdCommonHelpTextNewBuildBeta
   }


-- | The @build@ command does a lot. It brings the install plan up to date,
-- selects that part of the plan needed by the given or implicit targets and
-- then executes the plan.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
showBuildInfoAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
            -> [String] -> GlobalFlags -> IO ()
showBuildInfoAction (configFlags, configExFlags, installFlags, haddockFlags)
            targetStrings globalFlags = do

  baseCtx <- establishProjectBaseContext verbosity cliConfig

  buildCtx <-
    runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan ->
          return (elaboratedPlan, Map.empty)

  let baseCtx' = baseCtx {
                    buildSettings = (buildSettings baseCtx) {
                      buildSettingDryRun = True
                    }
                  }

  scriptLock <- newLock
  mapM_ (showInfo verbosity baseCtx' buildCtx scriptLock (configured buildCtx)) targetStrings
  
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags
    configured ctx = [p | InstallPlan.Configured p <- InstallPlan.toList (elaboratedPlanOriginal ctx)]


showInfo :: Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> [ElaboratedConfiguredPackage] -> String -> IO ()
showInfo verbosity baseCtx buildCtx lock pkgs targetName = setupWrapper verbosity scriptOptions (Just $ elabPkgDescription pkg) (Cabal.showBuildInfoCommand defaultProgramDb) (const flags)  []
  where pkg = head $ filter isTarget pkgs
        isTarget = (mkPackageName targetName ==) . pkgName . elabPkgSourceId
        shared = elaboratedShared buildCtx
        buildDir = distBuildDirectory (distDirLayout baseCtx) (elabDistDirParams shared pkg)
        flags = setupHsBuildFlags pkg shared verbosity buildDir
        srcDir = case (elabPkgSourceLocation pkg) of
          LocalUnpackedPackage fp -> fp
          _ -> ""
        scriptOptions = setupHsScriptOptions (ReadyPackage pkg) shared srcDir buildDir False lock