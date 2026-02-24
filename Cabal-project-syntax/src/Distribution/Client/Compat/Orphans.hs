{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Client.Compat.Orphans () where

import Data.Typeable (typeRep)
import Distribution.Compat.Binary (Binary (..))
import Distribution.Utils.Structured (Structure (Nominal), Structured (..))
import Network.URI (URI (..), URIAuth (..))
import Prelude (return)

-------------------------------------------------------------------------------
-- network-uri
-------------------------------------------------------------------------------

-- note, network-uri-2.6.0.3+ provide a Generic instance but earlier
-- versions do not, so we use manual Binary instances here
instance Binary URI where
  put (URI a b c d e) = do put a; put b; put c; put d; put e
  get = do
    !a <- get
    !b <- get
    !c <- get
    !d <- get
    !e <- get
    return (URI a b c d e)

instance Structured URI where
  structure p = Nominal (typeRep p) 0 "URI" []

instance Binary URIAuth where
  put (URIAuth a b c) = do put a; put b; put c
  get = do !a <- get; !b <- get; !c <- get; return (URIAuth a b c)
