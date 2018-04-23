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
module Util.GetValues where
import           Data.Generics.Product.Fields
import           Data.Proxy
import qualified Data.Text                    as T
import           Database.Bolt
import           GHC.TypeLits
import           Lens.Micro
import           Util.Convert

-- По списку имен полей получить из записи список пар (имя, Bolt-значение)
class GetValues (xs :: [Symbol]) a where
  getValues :: a -> [(T.Text, Value)]

instance GetValues '[] a where
  getValues _ = []

instance (KnownSymbol x, HasField' x a b, Convert b (Maybe Value), GetValues xs a)
      => GetValues (x ': xs) a where
  getValues a = case convert $ a ^. field @x of
    Nothing -> getValues @xs a
    Just v  -> (T.pack $ symbolVal (Proxy @x), v) : getValues @xs a
