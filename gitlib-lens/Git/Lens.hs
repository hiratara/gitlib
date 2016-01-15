{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Git.Lens where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.Action (Effective, act)
import           Control.Monad
import           Data.ByteString
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Tagged
import           Data.Text (Text)
import           Git
import           Prelude hiding (FilePath)

makeClassy_ ''Commit

type RepoGetter r' m a b =
    forall (p :: * -> * -> *) (f :: * -> *) r.
        (Conjoined p, Effective m r f, MonadGit r' m)
        => p b (f b) -> p a (f a)

_oid :: MonadGit r' m => RepoGetter r' m Text (Tagged o (Oid r'))
_oid = act parseObjOid

_commit :: MonadGit r' m => RepoGetter r' m (CommitOid r') (Commit r')
_commit = act lookupCommit

_tree :: MonadGit r' m => RepoGetter r' m (Commit r') (Tree r')
_tree = act $ lookupTree . commitTree

_ref :: MonadGit r' m => RepoGetter r' m Text (Maybe (Commit r'))
_ref = act $ resolveReference
           >=> maybe (return Nothing) (fmap Just . lookupCommit . Tagged)

_rtree :: MonadGit r' m => RepoGetter r' m (TreeOid r') (Tree r')
_rtree = act lookupTree

_rblob :: MonadGit r' m => RepoGetter r' m (BlobOid r') (Blob r' m)
_rblob = act lookupBlob

_entry :: (Conjoined p, Effective m r f, MonadGit r' m)
       => TreeFilePath -> p (Maybe (TreeEntry r')) (f (Maybe (TreeEntry r')))
       -> p (Tree r') (f (Tree r'))
_entry n = act $ treeEntry ?? n

_entries :: MonadGit r' m
         => RepoGetter r' m (Tree r') (HashMap TreeFilePath (TreeEntry r'))
_entries = act $ fmap HashMap.fromList . listTreeEntries

_centries :: MonadGit r' m
          => RepoGetter r' m (Commit r') (HashMap TreeFilePath (TreeEntry r'))
_centries = _tree._entries

_blob :: (Conjoined p, Effective m r f, MonadGit r' m)
      => TreeFilePath -> p (Maybe ByteString) (f (Maybe ByteString))
      -> p (Tree r') (f (Tree r'))
_blob n = act (treeEntry ?? n) . act f
  where f (Just (BlobEntry oid _)) = Just <$> catBlob oid
        f _ = return Nothing
