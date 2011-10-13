module Lens where

import Control.Applicative
import Prelude hiding (id, (.))
import Control.Category
import qualified Data.Map as Map
import Data.Map (Map)

newtype Lens rec fld = Lens (rec -> Store fld rec)

data Store fld rec = Store (fld -> rec) fld

mapLens :: (Ord k) => k -> Lens (Map k v) (Maybe v)
mapLens k = Lens $ \m ->
            let set Nothing  = Map.delete k m
                set (Just v) = Map.insert k v m
                get          = Map.lookup k m
            in Store set get

(^.) :: rec -> Lens rec fld -> fld
a ^. Lens f = pos (f a)
infixr 9 ^.

pos :: Store fld rec -> fld
pos (Store _ s) = s

(^=) :: Lens rec fld -> fld -> rec -> rec
Lens f ^= b = peek b . f
infixr 4 ^=

peek :: fld -> Store fld rec -> rec
peek s (Store g _) = g s


lens :: (rec -> fld) -> (fld -> rec -> rec) -> Lens rec fld
lens get set = Lens $ \a -> Store (\b -> set b a) (get a)

iso :: (rec -> fld) -> (fld -> rec) -> Lens rec fld
iso f g = Lens (Store g . f)

instance Category Lens where
  id = Lens (Store id)
  Lens f . Lens g = Lens $ \a -> case g a of
    Store wba b -> case f b of
      Store wcb c -> Store (wba . wcb) c
