-- | Attach arbitrary labelled values
--
-- Intended for unqualified import.
module GHC.RTS.Events.Util.Decorated (
    -- * Main definitions
    Decorated(..)
  , undecorated
  , addDecoration
  , addDecoration'
  , decorateWith
  , decorateWith'
  , stripDecoration
    -- * Get decorations
  , DecorationType
  , HasDecoration
  , getDecoration
  ) where

import Data.Kind
import Data.Proxy
import Data.Type.Ord
import GHC.TypeError
import GHC.TypeLits

{-------------------------------------------------------------------------------
  Add decoration
-------------------------------------------------------------------------------}

data Decorated :: [(Symbol, Type)] -> Type -> Type where
  Undecorated  :: a -> Decorated '[] a
  DecorateWith :: d -> Decorated ds a -> Decorated ('(s, d) : ds) a

deriving stock instance
     Show a
  => Show (Decorated '[] a)
deriving stock instance
     ( Show d
     , Show (Decorated ds a)
     )
  => Show (Decorated ('(s, d) : ds) a)

undecorated :: a -> Decorated '[] a
undecorated = Undecorated

addDecoration :: Proxy s -> d -> Decorated ds a -> Decorated ('(s, d) : ds) a
addDecoration _ = DecorateWith

-- | Variation on 'addDecoration' when @s@ is clear from context
addDecoration' :: d -> Decorated ds a -> Decorated ('(s, d) : ds) a
addDecoration' = addDecoration Proxy

decorateWith :: Proxy s -> (a -> d) -> Decorated ds a -> Decorated ('(s, d) : ds) a
decorateWith s f a = addDecoration s (f $ stripDecoration a) a

decorateWith' :: (a -> d) -> Decorated ds a -> Decorated ('(s, d) : ds) a
decorateWith' = decorateWith Proxy

stripDecoration :: Decorated d a -> a
stripDecoration (Undecorated    e) = e
stripDecoration (DecorateWith _ e) = stripDecoration e

{-------------------------------------------------------------------------------
  Access specific decorations

  This isnt strictly necessary, but makes this infra somewhat more useable.

  This is carefully written to avoid overlapping instances. The structure of the
  two families (the main and the auxiliary) is echoed exactly in the structure
  of the two classes, with the @C@ variant switching on a conditional @c@.
-------------------------------------------------------------------------------}

type family DecorationType (s :: Symbol) (ds :: [(Symbol, Type)]) :: Type where
  DecorationType s1 ('(s2, d) : ds) = CType (SameSym s1 s2) s1 d ds

type family CType (c :: Bool) s1 d ds :: Type where
  CType True  s1 d ds = d
  CType False s1 d ds = DecorationType s1 ds

class GetDecoration (s :: Symbol) (ds :: [(Symbol, Type)]) where
  getDecoration :: Proxy s -> Decorated ds a -> DecorationType s ds

instance
       Unsatisfiable (Text "Unknown decoration " :<>: ShowType s1)
    => GetDecoration s1 '[] where

instance CGet (SameSym s1 s2) s1 d ds => GetDecoration s1 ('(s2, d) : ds) where
  getDecoration s1 (DecorateWith d a) = cGet (Proxy @(SameSym s1 s2)) s1 d a

class CGet (c :: Bool) s1 d ds where
  cGet :: Proxy c -> Proxy s1 -> d -> Decorated ds a -> CType c s1 d ds

instance CGet True s1 d ds where
  cGet _ _ d _ = d

instance GetDecoration s1 ds => CGet False s1 d ds where
  cGet _ s1 _ ds = getDecoration s1 ds

class    (GetDecoration s ds, DecorationType s ds ~ d) => HasDecoration s ds d
instance (GetDecoration s ds, DecorationType s ds ~ d) => HasDecoration s ds d

{-------------------------------------------------------------------------------
  Internal auxiliary: symbol equality (is this not in base..?)
-------------------------------------------------------------------------------}

type family SameSym (s1 :: Symbol) (s2 :: Symbol) :: Bool where
  SameSym s1 s2 = OrdCond (Compare s1 s2) False True False
