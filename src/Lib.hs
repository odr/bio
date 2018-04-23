{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
module Lib where
import           Control.Applicative    (liftA2, liftA3)
import           Control.Monad          ((>=>))
import           Control.Monad.IO.Class
import           Data.List              (find)
import qualified Data.Map               as M
import           Data.Maybe             (mapMaybe)
import           Data.Monoid            ((<>))
import           Data.Proxy
import qualified Data.Text              as T
import           Database.Bolt
import           GHC.Generics
import           Prelude                hiding (id, product)
import           Safe

import           CreateNode
import           Util.Convert
import           Util.GetValues
{-
компонент Molecule должен иметь поля id :: Int, smiles :: String, iupacName :: String;
компонент Reaction должен иметь поля id :: Int, name :: String;
компонент Catalyst должен иметь поля id :: Int, smiles :: String, name :: Maybe String;
компонент PRODUCT_FROM должен иметь поле amount :: Float;
компонент ACCELERATE должен иметь поле temperature :: Float, pressure :: Float;
дополните структуру данных необходимыми на ваш взгляд полями;
населите базу данных представителями (хотя бы по 20 образцов каждого вида).
Между записями в структурах данных могут быть функциональные зависимости
  (например, между представлением smiles и именем IUPAC).

Реализовать функционал на haskell
создайте соответствующие типы в haskell-библиотеке;
напишите функцию, которая умеет принимать реацию на вход и загружать её в базу;
напишите функцию, которая по номеру реакции в базе будет возвращать её в haskell-объект;
напишите функцию, которая по двум заданным молекулам ищет путь через реакции и
  молекулы с наименьшей длинной;
подумайте, как поддерживать функциональные зависимости между записями,
  упомянутые в предыдущем разделе.
-}

newtype Key a = Key { getKey :: Int } deriving (Show, Eq, Ord, Generic)
instance Convert (Key a) (Maybe Value) where
  convert = convert . getKey
instance Convert Value (Key a) where
  convert = Key . convert

data Molecule = Molecule { id        :: Key Molecule
                         , smiles    :: String
                         , iupacName :: String
                         } deriving (Show, Eq, Ord, Generic)

data Reaction = Reaction { id         :: Key Reaction
                         , name       :: String
                         , reagents   :: [Molecule]
                         , catalyst   :: Catalyst
                         , accelerate :: Accelerate
                         , product    :: Molecule
                         , amount     :: Double
                         } deriving (Show, Eq, Ord, Generic)

data Catalyst = Catalyst { id     :: Key Catalyst
                         , smiles :: String
                         , name   :: Maybe String
                         } deriving (Show, Eq, Ord, Generic)

data Accelerate = Accelerate { temperature :: Double
                             , pressure    :: Double
                             } deriving (Show, Eq, Ord, Generic)

addReaction :: MonadIO m => Pipe -> Reaction -> m ()
addReaction p = runCommand p . mergeReaction

mergeReaction :: Reaction -> CommandM ()
mergeReaction r = do
  vr <- mergeNode "Reaction" (Proxy @'["name"]) r
  vms <- mapM mergeMolecule $ reagents r
  vp <- mergeMolecule $ product r
  vc <- mergeCatalyst $ catalyst r
  mapM_ (\vm -> createRel "REAGENT_IN" vm vr (Proxy @'[]) r) vms
  createRel "ACCELERATE" vc vr (Proxy @'["temperature","pressure"]) (accelerate r)
  createRel "PRODUCT_FROM" vr vp (Proxy @'["amount"]) r

mergeMolecule :: Molecule -> CommandM T.Text
mergeMolecule = mergeNode "Molecule" $ Proxy @'["smiles","iupacName"]

mergeCatalyst :: Catalyst -> CommandM T.Text
mergeCatalyst = mergeNode "Catalyst" $ Proxy @'["smiles","name"]

getReaction :: MonadIO m => Pipe -> Key Reaction -> m (Maybe Reaction)
getReaction pipe n = case convert n of
  Nothing -> return Nothing
  Just t -> fmap processRes
              $ run pipe
              $ queryP ("match (r:Reaction {id:{id}})"
                    <> " match (m)-[:REAGENT_IN]->(r)"
                    <> " match (r)-[pf:PRODUCT_FROM]->(p)"
                    <> " match (c)-[a:ACCELERATE]->(r)"
                    <> " return m,r,p,c,pf,a")
              $ M.fromList $ (:[]) $ ("id",) $ t
  where
    processRes = \case
      [] -> Nothing
      (x:xs) -> addM xs <$> getR x
      where
        addM xs r = r { reagents = reagents r
                                ++ mapMaybe (resToMapRec "m" >=> mkMolecule) xs
                      }
        mkMolecule =
          liftA3 Molecule <$> recToFld "id"
                          <*> recToFld "smiles"
                          <*> recToFld "iupacName"
        getR x = do
          [m,r,p,c,pf,a] <- mapM (flip resToMapRec x) ["m","r","p","c","pf","a"]
          Reaction <$> recToFld "id" r
                   <*> recToFld "name" r
                   <*> ((:[]) <$> mkMolecule m)
                   <*> mkCatalyst c
                   <*> mkAccelerate a
                   <*> mkMolecule p
                   <*> recToFld "amount" pf
          where
            mkCatalyst =
              liftA3 Catalyst <$> recToFld "id"
                              <*> recToFld "smiles"
                              <*> (return . recToFld "name")
            mkAccelerate =
              liftA2 Accelerate <$> recToFld "temperature"
                                <*> recToFld "pressure"

recToFld :: Convert Value a => T.Text -> M.Map T.Text Value -> Maybe a
recToFld x = fmap convert . M.lookup x

resToMapRec :: T.Text -> Record -> Maybe (M.Map T.Text Value)
resToMapRec x = recToFld x >=> structToMapRec

structToMapRec :: Structure -> Maybe (M.Map T.Text Value)
structToMapRec = fmap convert . find isMap . fields

isMap :: Value -> Bool
isMap (M _) = True
isMap _     = False

getShortestPath :: MonadIO m => Pipe -> Key Molecule -> Key Molecule
                -> m (Maybe [Key Reaction])
getShortestPath pipe k1 k2 = case mapM convert [k1,k2] of
  Nothing -> return Nothing
  Just [i1, i2] ->
    fmap processRes
      $ run pipe
      $ queryP ("match (m1:Molecule{id:{k1}}),(m2:Molecule{id:{k2}})"
             <> ",p=shortestPath((m1)-[*..20]->(m2)) return p"
               ) $ M.fromList [("k1",i1),("k2",i2)]
  where
    isR (L [T t]) | t == "Reaction" = True
    isR _         = False

    processRes = \case
      []    -> Nothing
      (x:_) -> f1 x >>= f2 >>= f3
      where
        f1 :: M.Map T.Text Value -> Maybe Structure
        f1 = recToFld "p"
        f2 :: Structure -> Maybe Value
        f2 = headMay . fields
        f3 :: Value -> Maybe [Key Reaction]
        f3 = mapM (recToFld "id" . convert)
           . concatMap (filter isMap) . filter (any isR)
           . map (fields . convert @Value) . convert
