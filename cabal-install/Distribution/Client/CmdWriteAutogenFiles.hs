-- | cabal-install CLI command: build
--
module Distribution.Client.CmdWriteAutogenFiles (
    -- * The @build@ CLI and action
    writeAutogenFilesCommand,
    writeAutogenFilesAction
  ) where

import Distribution.Client.ProjectOrchestration

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags, WriteAutogenFilesFlags(..) )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault, Flag(..) )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Verbosity
         ( Verbosity, silent )
import Distribution.Simple.Utils
         ( wrapText)

import qualified Data.Map as Map
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Client.SetupWrapper
import Distribution.Simple.Program ( defaultProgramDb )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.ProjectPlanning (setupHsScriptOptions)
import Distribution.Client.DistDirLayout (distBuildDirectory, distDirectory)
import Distribution.Client.Types ( PackageLocation(..), GenericReadyPackage(..) )
import Distribution.Client.JobControl (newLock, Lock)

writeAutogenFilesCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
writeAutogenFilesCommand = Client.installCommand {
  commandName         = "new-write-autogen-files",
  commandSynopsis     = "",
  commandUsage        = usageAlternatives "new-write-autogen-files" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $ 
        "Generate and write out the Paths_<pkg>.hs and cabal_macros.h files\n"
     ++ "for all components in the project",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-write-autogen-files\n"
     ++ "    Write for all packages in the project\n"
  }

writeAutogenFilesAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags) 
                            -> [String] -> GlobalFlags -> IO ()
writeAutogenFilesAction (configFlags, configExFlags, installFlags, haddockFlags) _ globalFlags = do
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
  writeAutogenFiles verbosity baseCtx' buildCtx scriptLock (configured buildCtx)
  
  where
    -- Default to silent verbosity otherwise it will pollute our json output
    verbosity = fromFlagOrDefault silent (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags configExFlags
                  installFlags haddockFlags
    configured ctx = [p | InstallPlan.Configured p <- InstallPlan.toList (elaboratedPlanOriginal ctx)]


writeAutogenFiles :: Verbosity -> ProjectBaseContext -> ProjectBuildContext -> Lock -> [ElaboratedConfiguredPackage] -> IO ()
writeAutogenFiles verbosity baseCtx buildCtx lock pkgs = mapM_ runWrapper pkgs
  where runWrapper pkg = do
          let shared = elaboratedShared buildCtx
              buildDir = distBuildDirectory (distDirLayout baseCtx) (elabDistDirParams shared pkg)
              srcDir = case (elabPkgSourceLocation pkg) of
                LocalUnpackedPackage fp -> fp
                _ -> ""
              scriptOptions = setupHsScriptOptions (ReadyPackage pkg) shared srcDir buildDir False lock
          setupWrapper 
            verbosity 
            scriptOptions 
            (Just $ elabPkgDescription pkg) 
            (Cabal.writeAutogenFilesCommand defaultProgramDb) 
            (const $ WriteAutogenFilesFlags (Flag buildDir) (Flag verbosity))
            []