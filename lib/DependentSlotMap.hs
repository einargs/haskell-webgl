-- | A dependent slotmap used for storing information
-- about reactive producers.
--
-- Right now this is just implemented using a dependent
-- map. I want to go and incorporate the way the slotmap
-- package works into it, and dig into how the Uniq
-- from prim-uniq works to incorporate that.
module DependentSlotMap
  ( DSlotMap
  , newDSlotMap
  , Key
  , insert, delete, adjust, lookup
  ) where

import Prelude hiding (lookup)
import Data.Functor.Identity (Identity(..))
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Unique.Tag (Tag, newTag)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar, modifyMutVar')

newtype DSlotMap m = DSlotMap (MutVar (PrimState m) (DMap (Key m) Identity))

type Key m = Tag (PrimState m)

newDSlotMap :: forall m. PrimMonad m => m (DSlotMap m)
newDSlotMap = DSlotMap <$> newMutVar @m DMap.empty

lookup :: PrimMonad m => Key m v -> DSlotMap m -> m (Maybe v)
lookup key (DSlotMap var) = do
  dmap <- readMutVar var
  pure $ fmap runIdentity $ DMap.lookup key dmap

insert :: PrimMonad m => v -> DSlotMap m -> m (Key m v)
insert val (DSlotMap var) = do
  tag <- newTag
  modifyMutVar' var $ DMap.insert tag (Identity val)
  pure tag

delete :: PrimMonad m => Key m v -> DSlotMap m -> m ()
delete key (DSlotMap var) = modifyMutVar' var $ DMap.delete key

-- TODO: (BUG) I think this silently does nothing if the key doesn't exist
-- when we want it to throw an error.
adjust :: PrimMonad m => (v -> v) -> Key m v -> DSlotMap m -> m ()
adjust f key (DSlotMap var) = modifyMutVar' var $ DMap.adjust f' key
  where f' = Identity . f . runIdentity
