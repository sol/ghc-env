{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module GhcEnv (
  main
#ifdef TEST
, modifyPath
#endif
) where

import           Data.List
import           Control.Exception
import           System.Environment
import           System.FilePath (searchPathSeparator, splitSearchPath)
import           System.Process

import           Path
import           Path.IO
import           Network.HTTP.Client (Manager)
import           Control.Monad.Logger

import qualified Stack.Config as Stack
import           Stack.Setup
import           Stack.Setup.Installed
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Compiler
import           Stack.Types.StackT

import           Args

logLevel :: LogLevel
logLevel = LevelInfo

dummyStackFile :: String
dummyStackFile = "resolver: lts-5.8"

main :: IO ()
main = do
  (version, command) <- parseArgs <$> getEnvironment <*> getArgs
  paths <- ensureGhc version
  paths' <- modifyPath paths <$> lookupEnv "PATH"
  case command of
      Exec prog args -> do
          setEnv "PATH" paths'
          spawnProcess prog args >>= waitForProcess >>= throwIO
      Source -> do
          putStrLn $ "PATH=" ++ paths'

modifyPath :: [FilePath] -> Maybe String -> String
modifyPath dirs mPath = intercalate [searchPathSeparator] path
  where
    path = dirs ++ maybe [] splitSearchPath mPath

ensureGhc :: Version -> IO [FilePath]
ensureGhc version = do
  manager <- newTLSManager
  config <- loadConfig manager globalOpts
  miniConfig <- runStackTGlobal manager config globalOpts (Stack.loadMiniConfig config)
  runStackTGlobal manager miniConfig globalOpts $
    maybe [] edBins <$> ensureCompiler SetupOpts {
      soptsInstallIfMissing = True
    , soptsUseSystem = False
    , soptsWantedCompiler = GhcVersion version
    , soptsCompilerCheck = MatchExact
    , soptsStackYaml = Nothing
    , soptsForceReinstall = False
    , soptsSanityCheck = True
    , soptsSkipGhcCheck = False
    , soptsSkipMsys = configSkipMsys config
    , soptsUpgradeCabal = False
    , soptsResolveMissingGHC = Nothing
    , soptsStackSetupYaml = defaultStackSetupYaml
    , soptsGHCBindistURL = Nothing
    }
  where
    globalOpts = GlobalOpts {
      globalReExecVersion = Nothing
    , globalDockerEntrypoint = Nothing
    , globalLogLevel = logLevel
    , globalConfigMonoid = mempty
    , globalResolver = Nothing
    , globalCompiler = Nothing
    , globalTerminal = True
    , globalStackYaml = Nothing
    }

loadConfig :: Manager -> GlobalOpts -> IO Config
loadConfig manager globalOpts@GlobalOpts{..} = withDummyStackFile $ \stackFile -> do
  runStackLoggingTGlobal manager globalOpts $ do
    lcConfig <$> Stack.loadConfig globalConfigMonoid (Just stackFile) globalResolver

withDummyStackFile :: (Path Abs File -> IO a) -> IO a
withDummyStackFile action = withSystemTempDir "" $ \dir -> do
  let stackFile = dir </> $(mkRelFile "stack.yaml")
  writeFile (fromAbsFile stackFile) dummyStackFile
  action stackFile
