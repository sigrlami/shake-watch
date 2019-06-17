{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Concurrent         (newChan, readChan)
import           Control.Exception          (SomeAsyncException (..),
                                             fromException, throwIO, try)
import           Data.Functor               (void)
import qualified Data.Set                   as Set
import           Development.Shake
import           Development.Shake.Database
import           Language.Haskell.Ghcid     as Ghcid
import           System.Directory           (getCurrentDirectory)
import           System.Environment         (getArgs, getEnvironment, withArgs)
import           System.FilePath
import           System.FSNotify            (eventPath, watchTreeChan,
                                             withManager)
import           System.Posix.Process

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    "watch":args ->
      -- Strip off the "watch" arg for the real shake script.
      withArgs args $ do
        -- Run the real main under 'try', ignoring synchronous exceptions
        -- because we don't care if the build fails - we want to watch & rebuild
        -- regardless. Re-throw async exceptions.
        try realMain >>= \case
          Left (fromException -> Just (SomeAsyncException ex)) -> throwIO ex
          _ -> pure ()

        -- Get the list of files that shake considers to be alive. This assumes
        -- we've set
        --
        --   shakeLiveFiles = [".shake/live"]
        --
        cwd <- getCurrentDirectory
        files <- Set.fromList . map (cwd </>) . lines <$> readFile ".shake/live"

        -- Start watching the filesystem, and rebuild once any of these files
        -- changes.
        withManager $ \manager -> do
          chan <- newChan
          void (watchTreeChan manager "." ((`elem` files) . eventPath) chan)
          void (readChan chan) -- We block here

        -- Loop. Here my compiled shakefile is itself a build target, so I exec
        -- it rather than loop here, to get the latest & greatest shake
        -- executable.
        env <- getEnvironment
        executeFile "bin/Shakefile" False ("watch":args) (Just env)

    _ ->
      realMain

realMain :: IO ()
realMain = do
  return $ ()


-- | Start or update a ghcid session.
--
ghcidStart :: CommandArguments -> Shake GhcidEnv ()
ghcidStart copts = do
    currentBufferPath <- fromObjectUnsafe <$> vim_call_function "expand" [ObjectBinary "%:p:h"]
    liftIO (determineProjectSettings' currentBufferPath) >>= \case
        Nothing -> void $
            yesOrNo "Could not determine project settings. This plugin needs a project with a .cabal file to work."
        Just s -> case bang copts of
            Just True ->
                startOrReload s

            _ -> do
                d <- askForDirectory
                        "Specify directory from which ghcid should be started."
                        (Just (rootDir s))
                c <- askForString
                        "Specify the command to execute (e.g. \"ghci\")."
                        (Just (cmd s))

                let s' = ProjectSettings d c
                whenM (yesOrNo "Save settings to file?") .
                    liftIO . BS.writeFile (d </> "ghcid.yaml") $ encode s'
                startOrReload s


-- | Start a new ghcid session or reload the modules to update the quickfix list.
startOrReload :: ProjectSettings -> Shake GhcidEnv ()
startOrReload s@(ProjectSettings d c) = do
    sessions <- atomically . readTVar =<< asks startedSessions
    case Map.lookup d sessions of
        Nothing -> do
            (g, ls) <- liftIO (startGhci c (Just d) (\_ _ -> return ()))
                `catch` \(SomeException e) ->  err . pretty $ "Failed to start ghcid session: " <> show e
            applyQuickfixActions $ loadToQuickfix ls
            void $ vim_command "cwindow"
            ra <- addAutocmd "BufWritePost" Sync def (startOrReload s) >>= \case
                Nothing ->
                    return $ return ()

                Just (Left a) ->
                    return a

                Just (Right rk) ->
                    return $ Resource.release rk

            modifyStartedSessions $ Map.insert d (g,ra >> liftIO (stopGhci g))

        Just (ghci, _) -> do
            applyQuickfixActions =<< loadToQuickfix <$> liftIO (reload ghci)
            void $ vim_command "cwindow"

ghcidStop :: CommandArguments -> Shake GhcidEnv ()
ghcidStop _ = do
    d <- fromObjectUnsafe <$> vim_call_function "expand" [ObjectBinary "%:p:h"]
    sessions <- atomically .readTVar =<< asks startedSessions
    case Map.lookupLE d sessions of
        Nothing ->
            return ()
        Just (p,(_, releaseAction)) -> do
            modifyStartedSessions $ Map.delete p
            releaseAction

-- | Same as @:GhcidStop@ followed by @:GhcidStart!@. Note the bang!
ghcidRestart :: CommandArguments -> Shake GhcidEnv ()
ghcidRestart _ = do
    ghcidStop def
    ghcidStart def { bang = Just True }
