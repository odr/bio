{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Util.Convert where

import qualified Data.Map           as M
import           Data.Maybe         (isJust)
import qualified Data.Text          as T
import           Data.Type.Equality
import           Database.Bolt

class Convert a b where
  convert :: a -> b
class ConvertB (p::Bool) a b where
  convertB :: a -> b

-- прямые конверты
instance Convert a a where convert = id
instance Convert ()         (Maybe Value) where convert = Just . N
instance Convert Bool       (Maybe Value) where convert = Just . B
instance Convert Int        (Maybe Value) where convert = Just . I
instance Convert Double     (Maybe Value) where convert = Just . F
instance Convert T.Text     (Maybe Value) where convert = Just . T
instance Convert Structure  (Maybe Value) where convert = Just . S
instance ConvertB (a == Char) [a] (Maybe Value)
      => Convert [a] (Maybe Value) where
  convert = convertB @(a==Char)
instance Convert a (Maybe Value)
      => Convert (M.Map T.Text a) (Maybe Value) where
  convert = Just . M . fmap (\(Just x) -> x) . M.filter isJust . fmap convert

instance ConvertB True String (Maybe Value) where convertB = convert . T.pack
instance Convert a (Maybe Value) => ConvertB False [a] (Maybe Value) where
  convertB = Just . L . fmap (\(Just x) -> x) . filter isJust . map convert

instance Convert a (Maybe Value) => Convert (Maybe a) (Maybe Value) where
  convert = (>>= convert)

-- обратные конверты неполные!!!
instance Convert Value ()        where convert (N _) = ()
instance Convert Value Bool      where convert (B v) = v
instance Convert Value Int       where convert (I v) = v
instance Convert Value Double    where convert (F v) = v
instance Convert Value T.Text    where convert (T v) = v
instance Convert Value Structure where convert (S v) = v
instance ConvertB (a == Char) Value [a] => Convert Value [a] where
  convert = convertB @(a==Char)
instance Convert Value a => Convert Value (M.Map T.Text a) where
  convert (M v) = fmap convert v

instance ConvertB True Value String where convertB (T v)= T.unpack v
instance Convert Value a => ConvertB False Value [a] where
  convertB (L v) = map convert v
