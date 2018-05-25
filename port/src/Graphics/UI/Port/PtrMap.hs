-- #hide
-----------------------------------------------------------------------------------------
{-| Module      :  PtrMap
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Map from 'Ptr's to something else.
    Just a simple wrapper around the "IntMap" module.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.PtrMap
             ( PtrMap
             , empty, isEmpty
             , insert
             , lookup
             , delete
             , insertWith
             , elems, keys
             , update, adjust
             ) where

import Prelude hiding (lookup)
import Foreign.Ptr
import qualified Data.IntMap as M

-- | A map from @Ptr a@ to elements @b@.
type PtrMap a b  = M.IntMap b

intFromPtr :: Ptr a -> Int
intFromPtr ptr
  = minusPtr ptr nullPtr

ptrFromInt :: Int -> Ptr a
ptrFromInt i
  = plusPtr nullPtr i

empty :: PtrMap a b
empty 
  = M.empty

isEmpty :: PtrMap a b -> Bool
isEmpty 
  = M.null

insert :: Ptr a -> b -> PtrMap a b -> PtrMap a b
insert ptr x map
  = M.insert (intFromPtr ptr) x map

insertWith :: (b -> b -> b) -> Ptr a -> b -> PtrMap a b -> PtrMap a b
insertWith f ptr x map
  = M.insertWith f (intFromPtr ptr) x map

lookup :: Ptr a -> PtrMap a b -> Maybe b
lookup ptr map
  = M.lookup (intFromPtr ptr) map

delete :: Ptr a -> PtrMap a b -> PtrMap a b
delete ptr map
  = M.delete (intFromPtr ptr) map

elems :: PtrMap a b -> [b]
elems map
  = M.elems map

keys :: PtrMap a b -> [Ptr a]
keys m
  = map ptrFromInt (M.keys m)

update ::  (b -> Maybe b) -> Ptr a -> PtrMap a b -> PtrMap a b
update f k m = M.update f (intFromPtr k) m

adjust ::  (b -> b) -> Ptr a -> PtrMap a b -> PtrMap a b
adjust f k m = M.adjust f (intFromPtr k) m
