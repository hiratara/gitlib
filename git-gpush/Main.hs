{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Reader
import           Data.Char
import           Data.Foldable hiding (concat, concatMap, elem)
import           Data.List
import           Data.Maybe
import           Data.Tagged
import qualified Data.Text as T
#if MIN_VERSION_shelly(1, 0, 0)
import           Data.Text (Text)
import qualified Data.Text as TL hiding (tails)
#else
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL hiding (tails)
#endif
import           Git hiding (Options)
#if USE_LIBGIT2
import qualified Git.Libgit2 as Lg
#else
import qualified Git.CmdLine as Cli
#endif
import           Options.Applicative
import           Shelly hiding (FilePath)

#if USE_LIBGIT2
factory :: RepositoryFactory (ReaderT Lg.LgRepo (NoLoggingT IO)) IO Lg.LgRepo
factory = Lg.lgFactory
#else
factory :: RepositoryFactory (ReaderT Cli.CliRepo IO) IO Cli.CliRepo
factory = Cli.cliFactory
#endif

toStrict :: TL.Text -> T.Text
#if MIN_VERSION_shelly(1, 0, 0)
toStrict = id
#else
toStrict = TL.toStrict
#endif

fromStrict :: T.Text -> TL.Text
#if MIN_VERSION_shelly(1, 0, 0)
fromStrict = id
#else
fromStrict = TL.fromStrict
#endif

data Options = Options
    { verbose    :: Bool
    , dryRun     :: Bool
    , remote     :: Maybe String
    , args       :: [String]
    }

options :: Parser Options
options = Options
    <$> switch (short 'v' <> long "verbose" <> help "Display info")
    <*> switch (short 'n' <> long "dry-run" <> help "Dry run")
    <*> argument (Just <$> str) (value Nothing)
    <*> many (argument str hidden)

volume :: Options -> Sh a -> Sh a
volume opts = if verbose opts then verbosely else silently

sh :: MonadIO m => Options -> Sh a -> m a
sh opts = shelly . volume opts

git :: [Text] -> Sh Text
git = run "git"

git_ :: [Text] -> Sh ()
git_ = run_ "git"

main :: IO ()
main = execParser opts >>= pushToGitHub
  where
    opts = info (helper <*> options)
                (fullDesc <> progDesc desc <> header hdr)
    hdr  = "git-gpush 3.1.0.1 - push to GitHub with smarter behavior"
    desc = "\nDescription goes here."

pushToGitHub :: Options -> IO ()
pushToGitHub opts = do
    gd <- TL.unpack . TL.init <$> sh opts (run "git" ["rev-parse", "--git-dir"])

    remoteName <- case remote opts of
        Nothing -> sh opts $ getRemoteName gd
        Just x  -> return (TL.pack x)

    remoteHead <-
        head . TL.words . TL.init
            <$> sh opts (git [ "ls-remote", remoteName, "HEAD" ])

    sh opts $ git_ [ "fetch" ]

    withRepository factory gd $ do
        cref <- fromJust <$> resolveReference "HEAD"
        hc   <- lookupCommit (Tagged cref)
        rcoid <- Tagged <$> parseOid (toStrict remoteHead)
        objs <- listObjects (Just rcoid) (commitOid hc) False
        for_ objs $ \obj -> case obj of
            Git.CommitObjOid coid -> do
                commit <- lookupCommit coid
                sh opts $ processCommitTags (commitLog commit)
            _ -> return ()

    unless (dryRun opts) $
        sh opts $ git_ $ [ "push", remoteName ] <> map TL.pack (args opts)

getRemoteName :: FilePath -> Sh Text
getRemoteName gd = do
    mref <- liftIO $ withRepository factory gd $ lookupReference "HEAD"
    case mref of
        Nothing -> error "Could not find HEAD"
        Just (RefObj _) ->
            error "Cannot push from a detached HEAD"
        Just (RefSymbolic (fromStrict -> branch))
            | "refs/heads/" `TL.isPrefixOf` branch ->
                TL.init <$> git [ "config"
                                , "branch." <> base branch <> ".remote" ]
            | otherwise ->
                error "Cannot push from a branch outside refs/heads"
  where
    base = TL.drop (TL.length "refs/heads/")

data CommitTag
    = ConfirmTag
      { confirmIssue  :: Text
      , confirmReport :: Maybe Text
      }
    deriving (Eq, Show)

findTags :: Text -> [CommitTag]
findTags = concatMap go . tails . map norm . TL.words
  where
    go [] = []
    go [_] = []
    go ["confirm", TL.splitOn ":" -> [issue]] =
        [ ConfirmTag (TL.tail issue) Nothing | isIssue issue ]
    go ["confirm", TL.splitOn ":" -> [issue, report]] =
        [ ConfirmTag (TL.tail issue) (Just report) | isIssue issue ]
    go _ = []

    isIssue issue = not (TL.null issue) && TL.head issue == '#'

    norm = TL.filter $ \c -> c `elem` ("#:_-" :: [Char])|| isDigit c || isLetter c

processCommitTags :: T.Text -> Sh ()
processCommitTags msg =
    for_ (findTags (fromStrict msg)) $ \tag -> case tag of
        ConfirmTag issue mreport -> do
            mreport' <- case mreport of
                x@(Just _) -> return x
                Nothing -> errExit False $ do
                    who <- git [ "config", "fpco.directreport" ]
                    ec  <- lastExitCode
                    return $ if ec == 0
                        then Just (TL.init who)
                        else Nothing

            -- jww (2013-04-28): Instead of using ghi here, we should use the
            -- github library, after reading in ghi.token.
            run_ "ghi"
                $ [ "edit" ]
               <> [ x | x <- [ "-u", fromJust mreport' ], isJust mreport' ]
               <> [ issue ]
            run_ "ghi" [ "comment"
                       , "-m", "Please confirm that this has been fixed."
                       , issue ]
            run_ "ghi" [ "label", issue, "-a", "need confirmation" ]

-- Main.hs (git-gpush) ends here
