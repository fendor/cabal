{-# LANGUAGE NondecreasingIndentation #-}

import Test.Cabal.Workdir
import Test.Cabal.Script
import Test.Cabal.Server
import Test.Cabal.Monad
import Test.Cabal.TestCode

import Distribution.Verbosity
import Distribution.Simple.Utils     (getDirectoryContentsRecursiveOsPath, toUTF8LBS)
import Distribution.Simple.Program
import Distribution.Utils.Path       (getSymbolicPath)

import Options.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import GHC.Conc (numCapabilities)
import Data.List
import Data.Proxy (Proxy(Proxy))
import qualified Data.Sequence as Seq (fromList)
import Text.Printf
import qualified Test.Tasty.Options as Tasty
  ( OptionSet
  , OptionDescription (Option)
  , lookupOption
  )
import qualified Test.Tasty.Runners as Tasty
  ( optionParser
  , TestPattern
  , testPatternMatches
  )
import qualified System.Clock as Clock
import System.IO
import System.OsPath
-- import System.FilePath
import System.Exit
import System.Process (readProcessWithExitCode, showCommandForUser)
-- import System.Directory
import System.Directory.Internal ( os )
import Distribution.Pretty
import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import System.Directory.OsPath
import qualified System.File.OsPath as FileIO
import qualified System.OsString as OsString
import qualified Options.Applicative as OA
import qualified System.Process.Environment.OsString
import Debug.Trace

{- Note [Testsuite package environments]

There are three different package environments which are used when running the
testsuite.

1. Environment used to compile `cabal-tests` executable
2. Environment used to run test scripts "setup.test.hs"
3. Environment made available to tests themselves via `./Setup configure`

These are all distinct from each other and should be specified separately.

Where are these environments specified:

1. The build-depends on `cabal-tests` executable in `cabal-testsuite.cabal`
2. The build-depends of `test-runtime-deps` executable in `cabal-testsuite.cabal`
   These dependencies are injected in a special module (`Test.Cabal.ScriptEnv0`) which
   then is consulted in `Test.Cabal.Monad` in order to pass the right environment.
   This is the mechanism by which the `./Setup` tests have access to the in-tree
   `Cabal`, `Cabal-syntax` and `Cabal-hooks` libraries.
3. No specification, only the `GlobalPackageDb` is available (see
   `testPackageDBStack`) unless the test itself augments the environment with
   `withPackageDb`.

At the moment, `cabal-install` tests always use the bootstrap cabal, which is a
bit confusing but `cabal-install` is not flexible enough to be given additional
package databases (yet).

-}

-- | Record for arguments that can be passed to @cabal-tests@ executable.
data MainArgs = MainArgs {
        mainArgThreads :: Int,
        mainArgTestPaths :: [OsString],
        mainArgHideSuccesses :: Bool,
        mainArgVerbose :: Bool,
        mainArgQuiet   :: Bool,
        mainArgDistDir :: Maybe OsPath,
        mainArgCabalSpec :: Maybe CabalLibSpec,
        mainCommonArgs :: CommonArgs,
        mainTastyArgs :: Tasty.OptionSet
    }

data CabalLibSpec = BootCabalLib | InTreeCabalLib OsPath OsPath | SpecificCabalLib String OsPath

cabalLibSpecParser :: Parser CabalLibSpec
cabalLibSpecParser = bootParser <|> intreeParser <|> specificParser
  where
    bootParser = flag' BootCabalLib (long "boot-cabal-lib")
    intreeParser = InTreeCabalLib <$> option osPath (long "intree-cabal-lib" <> metavar "ROOT")
                                  <*> option osPath ( help "Test TMP" <> long "test-tmp" )
    specificParser = SpecificCabalLib <$> strOption (long "specific-cabal-lib" <> metavar "VERSION")
                                      <*> option osPath ( help "Test TMP" <> long "test-tmp" )


osPath :: ReadM OsPath
osPath = OA.str >>= \fp -> case encodeUtf fp of
  Right txt -> pure txt
  Left ex -> fail $ "failed: " ++ displayException ex

-- | optparse-applicative parser for 'MainArgs'
mainArgParser :: Parser MainArgs
mainArgParser = MainArgs
    <$> option auto
        ( help "Number of threads to run"
       <> short 'j'
       <> showDefault
       <> value numCapabilities
       <> metavar "INT")
    <*> many (argument osPath (metavar "FILE"))
    <*> switch
        ( long "hide-successes"
       <> help "Do not print test cases as they are being run"
        )
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Be verbose"
        )
    <*> switch
        ( long "quiet"
       <> short 'q'
       <> help "Only output stderr on failure"
        )
    <*> optional (option osPath
        ( help "Dist directory we were built with"
       <> long "builddir"
       <> metavar "DIR"))
    <*> optional cabalLibSpecParser
    <*> commonArgParser
    <*> tastyArgParser

tastyArgParser :: Parser Tasty.OptionSet
tastyArgParser =
  let (warnings, parser) =
        Tasty.optionParser
          [ Tasty.Option (Proxy @Tasty.TestPattern)
          ]
   in if null warnings
         then parser
         else error $ unlines ("Failed to create parser for Tasty CLI options:" : warnings)

-- Unpack and build a specific released version of Cabal and Cabal-syntax libraries
buildCabalLibsProject :: ByteString -> Verbosity -> Maybe String -> OsPath -> IO [OsPath]
buildCabalLibsProject projString verb mbGhc dir = do
  let prog_db = userSpecifyPaths [("ghc", path) | Just path <- [mbGhc] ]  defaultProgramDb
  (cabal, _) <- requireProgram verb (simpleProgram "cabal") prog_db
  (ghc, _) <- requireProgram verb ghcProgram prog_db

  dirFp <- decodeUtf dir
  let storeRoot = dir </> os "store"
  storeRootFp <- decodeUtf storeRoot
  projectFile <- decodeUtf $ dir </> os "cabal.project-test"
  let pv = fromMaybe (error "no ghc version") (programVersion ghc)
  let final_package_db = dir </> os "dist-newstyle" </> os "packagedb" </> os ("ghc-" ++ prettyShow pv)
  createDirectoryIfMissing True dir
  FileIO.writeFile (dir </> os "cabal.project-test") projString

  runProgramInvocation verb
    ((programInvocation cabal
      ["--store-dir", storeRootFp
      , "--project-file", projectFile
      , "build"
      , "-w", programPath ghc
      , "Cabal", "Cabal-syntax", "Cabal-hooks", "hooks-exe"
      ] ) { progInvokeCwd = Just dirFp })

  -- Determine the path to the packagedb in the store for this ghc version
  storesByGhc <- listDirectory storeRoot
  case filter (os (prettyShow pv) `OsString.isInfixOf`) storesByGhc of
    [] -> return [final_package_db]
    storeForGhc:_ -> do
      let storePackageDB = storeRoot </> storeForGhc </> os "package.db"
      return [storePackageDB, final_package_db]



buildCabalLibsSpecific :: String -> Verbosity -> Maybe FilePath -> OsPath -> IO [OsPath]
buildCabalLibsSpecific ver verb mbGhc builddir_rel = do
  let prog_db = userSpecifyPaths [("ghc", path) | Just path <- [mbGhc] ]  defaultProgramDb
  (cabal, _) <- requireProgram verb (simpleProgram "cabal") prog_db
  dir <- canonicalizePath (builddir_rel </> os "specific" </> os ver)
  dirFp <- decodeUtf dir
  cgot <- doesDirectoryExist (dir </> os ("Cabal-" ++ ver))
  unless cgot $
    runProgramInvocation verb ((programInvocation cabal ["get", "Cabal-" ++ ver]) { progInvokeCwd = Just dirFp })
  csgot <- doesDirectoryExist (dir </> os ("Cabal-syntax-" ++ ver))
  unless csgot $
    runProgramInvocation verb ((programInvocation cabal ["get", "Cabal-syntax-" ++ ver]) { progInvokeCwd = Just dirFp })
  chgot <- doesDirectoryExist (dir </> os ("Cabal-hooks-" ++ ver))
  unless chgot $
    runProgramInvocation verb ((programInvocation cabal ["get", "Cabal-hooks-" ++ ver]) { progInvokeCwd = Just dirFp })
  buildCabalLibsProject (toUTF8LBS $ "packages: Cabal-" ++ ver ++ " Cabal-syntax-" ++ ver ++ " Cabal-hooks-" ++ ver) verb mbGhc dir


buildCabalLibsIntree :: OsPath -> Verbosity -> Maybe FilePath -> OsPath -> IO [OsPath]
buildCabalLibsIntree root verb mbGhc builddir_rel = do
  dir <- canonicalizePath (builddir_rel </> os "intree")
  let libs = map os [ "Cabal", "Cabal-syntax", "Cabal-hooks", "hooks-exe" ]
  pkgs <- traverse ( decodeUtf . (root </>) ) libs
  buildCabalLibsProject (toUTF8LBS $ "packages: " ++ unwords pkgs ) verb mbGhc dir

myExecParser :: ParserInfo b -> IO b
myExecParser pinfo = do
  oss <- System.Process.Environment.OsString.getArgs
  traceShowM oss
  ps <- traverse decodeUtf oss
  traceShowM ps
  OA.handleParseResult $ OA.execParserPure OA.defaultPrefs pinfo ps

main :: IO ()
main = do
    -- By default, stderr is not buffered.  This isn't really necessary
    -- for us, and it causes problems on Windows, see:
    -- https://github.com/appveyor/ci/issues/1364
    hSetBuffering stderr LineBuffering

    -- Parse arguments.  N.B. 'helper' adds the option `--help`.
    args <- myExecParser $ info (mainArgParser <**> helper) mempty
    let verbosity =
          mkVerbosity defaultVerbosityHandles $
            if mainArgVerbose args then verbose else normal
        testPattern = Tasty.lookupOption @Tasty.TestPattern (mainTastyArgs args)

    pkg_dbs <-
      -- Not path to cabal-install so we're not going to run cabal-install tests so we
      -- can skip setting up a Cabal library to use with cabal-install.
      case argCabalInstallPath (mainCommonArgs args) of
        Nothing -> do
          when (isJust $ mainArgCabalSpec args)
               (putStrLn "Ignoring Cabal library specification as cabal-install tests are not running")
          return []
        -- Path to cabal-install is passed, so need to install the requested relevant version of Cabal
        -- library.
        Just {} ->
          case mainArgCabalSpec args of
            Nothing -> do
              putStrLn "No Cabal library specified, using boot Cabal library with cabal-install tests"
              return []
            Just BootCabalLib -> return []
            Just (InTreeCabalLib root build_dir) ->
              buildCabalLibsIntree root verbosity (argGhcPath (mainCommonArgs args)) build_dir
            Just (SpecificCabalLib ver build_dir) ->
              buildCabalLibsSpecific ver verbosity (argGhcPath (mainCommonArgs args)) build_dir

    -- To run our test scripts, we need to be able to run Haskell code
    -- linked against the Cabal library under test.  The most efficient
    -- way to get this information is by querying the *host* build
    -- system about the information.
    --
    -- Fortunately, because we are using a Custom setup, our Setup
    -- script is bootstrapped against the Cabal library we're testing
    -- against, so can use our dependency on Cabal to read out the build
    -- info *for this package*.
    --
    -- NB: Currently assumes that per-component build is NOT turned on
    -- for Custom.
    dist_dir <- case mainArgDistDir args of
                  Just dist_dir -> return dist_dir
                  Nothing -> encodeUtf . getSymbolicPath =<< guessDistDir
    dist_dir_fp <- decodeUtf dist_dir
    when (verbosityLevel verbosity >= Verbose) $ do
        hPutStrLn stderr $ "Using dist dir: " ++ dist_dir_fp
    -- Get ready to go!
    senv <- mkScriptEnv verbosity

    let runTest :: (Maybe cwd -> [unusedEnv] -> FilePath -> [FilePath] -> IO result)
                -> OsPath
                -> IO result
        runTest runner path = do
          path_fp <- decodeUtf path
          pkg_dbs_fp <- traverse decodeUtf pkg_dbs
          runner Nothing [] path_fp $
            ["--builddir", dist_dir_fp, path_fp] ++ ["--extra-package-db=" ++ pkg_db | pkg_db <- pkg_dbs_fp] ++ renderCommonArgs (mainCommonArgs args)

    case mainArgTestPaths args of
        [path] -> do
            -- Simple runner
            (real_path, real_args) <- runTest (runnerCommand senv) path
            case decodeUtf path of
              Nothing -> do
                hPutStrLn stderr "# SKIPPED: Unicode in paths not supported"
              Just {} -> do
                hPutStrLn stderr $ showCommandForUser real_path real_args
                -- If the test was reported flaky, the `runghc` call will exit
                -- with exit code 1, and report `TestCodeFlaky` on the stderr output
                --
                -- This seems to be the only way to catch this case.
                --
                -- Sadly it means that stdout and stderr are not interleaved
                -- directly anymore.
                (e, out, err) <- readProcessWithExitCode real_path real_args ""
                putStrLn "# STDOUT:"
                putStrLn out
                putStrLn "# STDERR:"
                putStrLn err
                if "TestCodeFlaky" `isInfixOf` err
                  then pure ()
                  else throwIO e
                hPutStrLn stderr "OK"
        user_paths -> do
            -- Read out tests from filesystem
            hPutStrLn stderr $ "threads: " ++ show (mainArgThreads args)

            test_scripts <- if null user_paths
                                then findTests
                                else return user_paths

            (single_tests, multi_tests) <- partitionTests testPattern test_scripts
            let all_tests = multi_tests ++ single_tests
                margin = maximum (map OsString.length all_tests) + 2
            hPutStrLn stderr $ "tests to run: " ++ show (length all_tests)

            -- TODO: Get parallelization out of multitests by querying
            -- them for their modes and then making a separate worker
            -- for each.  But for now, just run them earlier to avoid
            -- them straggling at the end
            work_queue <- newMVar all_tests
            unexpected_fails_var  <- newMVar []
            unexpected_passes_var <- newMVar []
            skipped_var <- newMVar []
            flaky_pass_var <- newMVar []
            flaky_fail_var <- newMVar []

            chan <- newChan
            let logAll msg = writeChan chan (ServerLogMsg AllServers msg)
                logEnd = writeChan chan ServerLogEnd
            -- NB: don't use withAsync as we do NOT want to cancel this
            -- on an exception
            async_logger <- async (withFile "cabal-tests.log" WriteMode $ outputThread verbosity chan)

            -- Make sure we pump out all the logs before quitting
            (\m -> finally m (logEnd >> wait async_logger)) $ do

            -- NB: Need to use withAsync so that if the main thread dies
            -- (due to ctrl-c) we tear down all of the worker threads.
            let go server = do
                    let split [] = return ([], Nothing)
                        split (y:ys) = return (ys, Just y)
                        logMeta msg = writeChan chan
                                    $ ServerLogMsg
                                        (ServerMeta (serverProcessId server))
                                        msg
                    mb_work <- modifyMVar work_queue split
                    case mb_work of
                        Nothing -> return ()
                        Just path -> do
                            path_fp <- decodeUtf path
                            when (verbosityLevel verbosity >= Verbose) $
                                logMeta $ "Running " ++ path_fp
                            start <- getTime
                            r <- runTest (runOnServer server) path
                            end <- getTime
                            let time = end - start
                                code = serverResultTestCode r

                            unless (mainArgHideSuccesses args && code == TestCodeOk) $ do
                                logMeta $
                                    path_fp ++ replicate (margin - length path_fp) ' ' ++ displayTestCode code ++
                                    if time >= 0.01
                                        then printf " (%.2fs)" time
                                        else ""

                            when (code == TestCodeFail) $ do
                                let description
                                      | mainArgQuiet args = serverResultStderr r
                                      | otherwise =
                                       "$ " ++ serverResultCommand r ++ "\n" ++
                                       "stdout:\n" ++ serverResultStdout r ++ "\n" ++
                                       "stderr:\n" ++ serverResultStderr r ++ "\n"
                                logMeta $
                                          description
                                       ++ "*** unexpected failure for " ++ path_fp ++ "\n\n"
                                modifyMVar_ unexpected_fails_var $ \paths ->
                                    return (path:paths)

                            when (isJust $ isTestCodeUnexpectedSuccess code) $
                                modifyMVar_ unexpected_passes_var $ \paths ->
                                    return (path:paths)

                            when (isTestCodeSkip code) $
                                modifyMVar_ skipped_var $ \paths ->
                                    return (path:paths)

                            case isTestCodeFlaky code of
                              NotFlaky  -> pure ()
                              Flaky b _ ->
                                modifyMVar_ (if b then flaky_pass_var else flaky_fail_var) $ \paths ->
                                    return (path:paths)

                            go server

            -- Start as many threads as requested by -j to spawn
            -- GHCi servers and start running tests off of the
            -- run queue.
            replicateConcurrently_ (mainArgThreads args) (withNewServer chan senv go)

            unexpected_fails  <- traverse decodeUtf =<< takeMVar unexpected_fails_var
            unexpected_passes <- traverse decodeUtf =<< takeMVar unexpected_passes_var
            skipped           <- traverse decodeUtf =<< takeMVar skipped_var
            flaky_passes      <- traverse decodeUtf =<< takeMVar flaky_pass_var
            flaky_fails       <- traverse decodeUtf =<< takeMVar flaky_fail_var

            -- print summary
            let sl = show . length
                testSummary =
                  sl all_tests ++ " tests, " ++ sl skipped ++ " skipped, "
                    ++ sl unexpected_passes ++ " unexpected passes, "
                    ++ sl unexpected_fails ++ " unexpected fails, "
                    ++ sl flaky_passes ++ " flaky passes, "
                    ++ sl flaky_fails ++ " flaky fails."
            logAll testSummary

            -- print failed or unexpected ok
            if null (unexpected_fails ++ unexpected_passes)
            then logAll "OK"
            else do
                unless (null unexpected_passes) . logAll $
                    "UNEXPECTED OK: " ++ unwords unexpected_passes
                unless (null unexpected_fails) . logAll $
                    "UNEXPECTED FAIL: " ++ unwords unexpected_fails
                exitFailure

findTests :: IO [OsPath]
findTests = getDirectoryContentsRecursiveOsPath (os ".")

-- | Partition a list of paths into a tuple of test paths and multi-test paths.
--
-- Non-test paths and test paths that don't match the given `Tasty.TestPattern` are dropped.
partitionTests :: Tasty.TestPattern -> [OsPath] -> IO ([OsPath], [OsPath])
partitionTests testPattern paths =
  go [] [] paths
  where
    -- Filter a list, keeping only paths that match the @pattern@.
    keepPatternMatches =
      filterM (\ p -> do
        tasty_path <- toTastyPath p
        pure $ Tasty.testPatternMatches testPattern tasty_path)

    toTastyPath path = do
      path_segments <- traverse decodeUtf $ splitDirectories path
      pure $ Seq.fromList path_segments

    go ts ms [] = do
      (,) <$> keepPatternMatches ts <*> keepPatternMatches ms
    go ts ms (f:fs) =
        -- NB: Keep this synchronized with isTestFile
        case takeExtensions f of
            ext
              | ext == os ".test.hs"      -> go (f:ts) ms fs
              | ext == os ".multitest.hs" -> go ts (f:ms) fs
              | otherwise                 -> go ts ms     fs

outputThread :: Verbosity -> Chan ServerLogMsg -> Handle -> IO ()
outputThread verbosity chan log_handle = go ""
  where
    go prev_hdr = do
        v <- readChan chan
        case v of
            ServerLogEnd -> return ()
            ServerLogMsg t msg -> do
                let ls = lines msg
                    pre s c
                        | verbosityLevel verbosity >= Verbose
                        -- Didn't use printf as GHC 7.4
                        -- doesn't understand % 7s.
                        = replicate (7 - length s) ' ' ++ s ++ " " ++ c : " "
                        | otherwise = ""
                    hdr = case t of
                            AllServers   -> ""
                            ServerMeta s -> pre s ' '
                            ServerIn   s -> pre s '<'
                            ServerOut  s -> pre s '>'
                            ServerErr  s -> pre s '!'
                    ws = replicate (length hdr) ' '
                    mb_hdr l | hdr == prev_hdr = ws ++ l
                             | otherwise = hdr ++ l
                    ls' = case ls of
                            [] -> []
                            r:rs ->
                                mb_hdr r : map (ws ++) rs
                    logmsg = unlines ls'
                hPutStr stderr logmsg
                hPutStr log_handle logmsg
                go hdr

-- Cribbed from tasty
type Time = Double

getTime :: IO Time
getTime = do
    t <- Clock.getTime Clock.Monotonic
    let ns = realToFrac $ Clock.toNanoSecs t
    return $ ns / 10 ^ (9 :: Int)
