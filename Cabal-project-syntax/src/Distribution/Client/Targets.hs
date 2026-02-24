{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Distribution.Client.Targets
-- Copyright   :  (c) Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Handling for user-specified targets
module Distribution.Client.Targets
  ( UserConstraint (..)
  , userConstraintPackageName
  , readUserConstraint
  , userToPackageConstraint
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Package
  ( PackageName
  , mkPackageName

  , unPackageName
  )

import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.PackagePath

import Distribution.Types.Flag
  ( parsecFlagAssignmentNonEmpty
  )
import qualified Distribution.Compat.CharParsing as P

-- ------------------------------------------------------------

-- * Package constraints

-- ------------------------------------------------------------

-- | Version of 'Qualifier' that a user may specify on the
-- command line.
data UserQualifier
  = -- | Top-level dependency.
    UserQualToplevel
  | -- | Setup dependency.
    UserQualSetup PackageName
  | -- | Executable dependency.
    UserQualExe PackageName PackageName
  deriving (Eq, Show, Generic)

instance Binary UserQualifier
instance NFData UserQualifier
instance Structured UserQualifier

-- | Version of 'ConstraintScope' that a user may specify on the
-- command line.
data UserConstraintScope
  = -- | Scope that applies to the package when it has the specified qualifier.
    UserQualified UserQualifier PackageName
  | -- | Scope that applies to the package when it has a setup qualifier.
    UserAnySetupQualifier PackageName
  | -- | Scope that applies to the package when it has any qualifier.
    UserAnyQualifier PackageName
  deriving (Eq, Show, Generic)

instance Binary UserConstraintScope
instance NFData UserConstraintScope
instance Structured UserConstraintScope

fromUserQualifier :: UserQualifier -> Qualifier
fromUserQualifier UserQualToplevel = QualToplevel
fromUserQualifier (UserQualSetup name) = QualSetup name
fromUserQualifier (UserQualExe name1 name2) = QualExe name1 name2

fromUserConstraintScope :: UserConstraintScope -> ConstraintScope
fromUserConstraintScope (UserQualified q pn) =
  ScopeQualified (fromUserQualifier q) pn
fromUserConstraintScope (UserAnySetupQualifier pn) = ScopeAnySetupQualifier pn
fromUserConstraintScope (UserAnyQualifier pn) = ScopeAnyQualifier pn

-- | Version of 'PackageConstraint' that the user can specify on
-- the command line.
data UserConstraint
  = UserConstraint UserConstraintScope PackageProperty
  deriving (Eq, Show, Generic)

instance Binary UserConstraint
instance NFData UserConstraint
instance Structured UserConstraint

userConstraintPackageName :: UserConstraint -> PackageName
userConstraintPackageName (UserConstraint scope _) = scopePN scope
  where
    scopePN (UserQualified _ pn) = pn
    scopePN (UserAnyQualifier pn) = pn
    scopePN (UserAnySetupQualifier pn) = pn

userToPackageConstraint :: UserConstraint -> PackageConstraint
userToPackageConstraint (UserConstraint scope prop) =
  PackageConstraint (fromUserConstraintScope scope) prop

readUserConstraint :: String -> Either String UserConstraint
readUserConstraint str =
  case explicitEitherParsec parsec str of
    Left err -> Left $ msgCannotParse ++ err
    Right c -> Right c
  where
    msgCannotParse =
      "expected a (possibly qualified) package name followed by a "
        ++ "constraint, which is either a version range, 'installed', "
        ++ "'source', 'test', 'bench', or flags. "

instance Pretty UserConstraint where
  pretty (UserConstraint scope prop) =
    pretty $ PackageConstraint (fromUserConstraintScope scope) prop

instance Parsec UserConstraint where
  parsec = do
    scope <- parseConstraintScope
    P.spaces
    prop <-
      P.choice
        [ PackagePropertyFlags <$> parsecFlagAssignmentNonEmpty -- headed by "+-"
        , PackagePropertyVersion <$> parsec -- headed by "<=>" (will be)
        , PackagePropertyInstalled <$ P.string "installed"
        , PackagePropertySource <$ P.string "source"
        , PackagePropertyStanzas [TestStanzas] <$ P.string "test"
        , PackagePropertyStanzas [BenchStanzas] <$ P.string "bench"
        ]
    return (UserConstraint scope prop)
    where
      parseConstraintScope :: forall m. CabalParsing m => m UserConstraintScope
      parseConstraintScope = do
        pn <- parsec
        P.choice
          [ P.char '.' *> withDot pn
          , P.char ':' *> withColon pn
          , return (UserQualified UserQualToplevel pn)
          ]
        where
          withDot :: PackageName -> m UserConstraintScope
          withDot pn
            | pn == mkPackageName "any" = UserAnyQualifier <$> parsec
            | pn == mkPackageName "setup" = UserAnySetupQualifier <$> parsec
            | otherwise = P.unexpected $ "constraint scope: " ++ unPackageName pn

          withColon :: PackageName -> m UserConstraintScope
          withColon pn =
            UserQualified (UserQualSetup pn)
              <$ P.string "setup."
              <*> parsec
