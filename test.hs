module Main where

import System.Environment
import System.Posix.Files
import System.Process (system)
import System.Exit (ExitCode (..))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

newer :: (Monad m, MonadIO m) => FilePath -> [FilePath] -> m ()
newer a bs = do
  aExist <- liftIO $ fileExist a
  unless aExist (fail "")

  aMTime <- (liftIO $ getFileStatus a) >>= return . modificationTime
  forM_ bs
    (\b -> do
      bExist <- liftIO $ fileExist b
      when bExist $ do
        bMTime <- (liftIO $ getFileStatus b) >>= return . modificationTime
        when (aMTime < bMTime) (fail "")
    )

needThen :: (MonadIO m, Monad m) => String -> FilePath -> [FilePath] -> m ()
needThen command target deps = do
  maybeIgnore <- runMaybeT (newer target deps)
  if maybeIgnore == Nothing
    then
      alwaysThen command
    else do
      liftIO $ putStrLn $ "skip: " ++ command

alwaysThen :: (MonadIO m, Monad m) => String -> m ()
alwaysThen command = do
  liftIO $ putStrLn $ "execute: " ++ command
  exitCode <- liftIO $ system command
  unless (exitCode == ExitSuccess) (fail "exec fail")

main = do
  [c, t] <- getArgs

  maybeResult <- runMaybeT $ do
    needThen ("happy -i -o " ++ c ++ ".hs " ++ c ++ ".y") (c ++ ".hs") [c ++ ".y"]
    needThen
      ("ghc --make " ++ c)
      c
      [ c ++ ".hs"
      , "Template.hs"
      , "Lexer.hs"
      , "ParserState.hs"
      , "ParserState2.hs"
      , "ParserState3.hs"
      ]
    alwaysThen ("./" ++ c ++ " < " ++ t ++ ".code > " ++ t ++ ".ll")
    needThen ("llvm-as -o " ++ t ++ ".bc " ++ t ++ ".ll") (t ++ ".bc") [t ++ ".ll"]
    needThen ("llc -o " ++ t ++ ".s " ++ t ++ ".bc") (t ++ ".s") [t ++ ".bc"]
    needThen ("clang -c -o " ++ t ++ ".o " ++ t ++ ".s") (t ++ ".o") [t ++ ".s"]
    needThen ("ld -lc /usr/lib/crt1.o -o " ++ t ++ " " ++ t ++ ".o") t [t ++ ".o"]

  if maybeResult == Nothing
    then
      putStrLn "Compile failed."
    else do
      putStrLn "Compile success. Run the test..."
      void $ runMaybeT $ alwaysThen ("./" ++ t)

