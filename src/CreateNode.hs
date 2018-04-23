{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module CreateNode where
import           Control.Monad.IO.Class
import           Control.Monad.Trans.RWS
import qualified Data.Map                as M
import           Data.Monoid             ((<>))
import           Data.Proxy
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import           Database.Bolt
import           GHC.Generics
import           GHC.TypeLits            (Symbol)
import           Prelude                 hiding (id, product)

import           Util.Convert
import           Util.GetValues

type CommandM = RWS () ([T.Text], M.Map T.Text Value) Int

runCommand :: MonadIO m => Pipe -> CommandM () -> m ()
runCommand pipe c = run pipe (queryP_ (T.intercalate " " ts) mp)
  where
    (ts,mp) = snd $ execRWS c () 0


--------
-- Для заданного имени метки, имен полей и записи,
-- получаем имя переменной, которуй потом используем при создании отношений.
-- При этом имена параметров и возвращаемых переменных отличаются за счет приписывания номера.
-- Текст запроса и параметры складываются в write-монаду
mergeNode :: GetValues ("id" ': xs) a
          => T.Text -> Proxy (xs :: [Symbol]) -> a -> CommandM T.Text
mergeNode lbl (_::Proxy xs) r = do
  ps <- mapM (\(n,v) -> (\k -> ((n,k),v)) <$> nextNum) $ getValues @("id" ': xs) r
  v <- ("v" <>) <$> nextNum
  tell ( [mergeNodeText v lbl $ map fst ps]
       , M.fromList $ map (\((n,k),v)->(n<>k,v)) ps
       )
  return v

-- Аналогично mergeNode, только без возвращаемых переменных
createRel :: GetValues xs a
          => T.Text -> T.Text -> T.Text -> Proxy (xs :: [Symbol]) -> a
          -> CommandM ()
createRel name from to (_::Proxy xs) r = do
  ps <- mapM (\(n,v) -> (\k -> ((n,k),v)) <$> nextNum) $ getValues @xs r
  tell ( [createRelText name from to $ map fst ps]
       , M.fromList $ map (\((n,k),v)->(n<>k,v)) ps
       )

nextNum :: CommandM T.Text
nextNum = get >>= \n -> put (n+1) >> return (T.pack $ show n)

mergeNodeText :: T.Text -> T.Text -> [(T.Text,T.Text)]-> T.Text
mergeNodeText v x ((i,ni):ps)
  = "merge (" <> v <> ": " <> x <> " { id:{" <> i <> ni <> "} }) set "
  <> (T.intercalate "," $ map prop ps)
  where
    prop (k,n) = v <> "." <> k <> "={" <> k <> n <> "}"

createRelText :: T.Text -> T.Text -> T.Text -> [(T.Text,T.Text)] -> T.Text
createRelText name from to ps
  = "create (" <> from <> ")-[:" <> name <> "{" <> T.intercalate "," (map prop ps)
  <> "}]->(" <> to <> ")"
  where
    prop (k,n) = k <> ": {" <> k <> n <> "}"


{-
merge (r:Reaction {id:2}) set r.name='Two' merge (m1:Molecule {id:1}) on create set m1.smiles='smiles1+',m1.uipackName='ui1' on match set m1.smiles='smiles1+',m1.uipackName='ui1'
merge (m2:Molecule {id:3}) on create set m2.smiles='smiles3',m2.uipackName='ui3' on match set m2.smiles='smiles3',m2.uipackName='ui3'
merge (c:Catalyst {id:2}) on create set c.smiles='smilescat2',c.name='cat2' on match set c.smiles='smilescat2',c.name='cat2'
merge (res:Molecule {id:4}) on create set res.smiles='smiles4',res.uipackName='ui4' on match set res.smiles='smiles4',res.uipackName='ui4'
create (m1)-[:REAGENT_IN{}]->(r)
create (m2)-[r2:REAGENT_IN]->(r)
create (c)-[:ACCELERATE{temperature:18.2, pressure:2123}]->(r)
create (r)-[prod:PRODUCT_FROM{amount:12.2}]->(res)
-}
