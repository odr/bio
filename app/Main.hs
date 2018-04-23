{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Default
import           Database.Bolt

import           Lib


main :: IO ()
main = do
  pipe <- connect $ def { user = "neo4j", password = "neo4j" }
  mapM_ (addReaction pipe) reactions
  mapM_ putStrLn ["","--- Reaction 4 ---",""]
  getReaction pipe (Key 4) >>= print
  mapM_ putStrLn ["","","--- Shortest path ([Key Reaction]) from Molecule 1 to 9 ---",""]
  getShortestPath pipe (Key 1) (Key 9) >>= print


reactions :: [Reaction]
reactions =
  [ Reaction (Key 1)  "name1"  [ms!!0, ms!!1]            (cs!!1)  (Accelerate 1.2 123.21) (ms!!5)  17.2
  , Reaction (Key 2)  "name2"  [ms!!1, ms!!2]            (cs!!2)  (Accelerate 1.2 123.21) (ms!!6)  17.2
  , Reaction (Key 3)  "name3"  [ms!!2, ms!!3]            (cs!!3)  (Accelerate 1.2 123.21) (ms!!7)  17.2
  , Reaction (Key 4)  "name4"  [ms!!3, ms!!4]            (cs!!4)  (Accelerate 1.2 123.21) (ms!!8)  17.2
  , Reaction (Key 5)  "name5"  [ms!!4, ms!!5]            (cs!!5)  (Accelerate 1.2 123.21) (ms!!9)  17.2
  , Reaction (Key 6)  "name6"  [ms!!5, ms!!6]            (cs!!6)  (Accelerate 1.2 123.21) (ms!!0)  17.2
  , Reaction (Key 7)  "name7"  [ms!!6, ms!!7]            (cs!!7)  (Accelerate 1.2 123.21) (ms!!1)  17.2
  , Reaction (Key 8)  "name8"  [ms!!7, ms!!8]            (cs!!8)  (Accelerate 1.2 123.21) (ms!!2)  17.2
  , Reaction (Key 9)  "name9"  [ms!!8, ms!!9]            (cs!!9)  (Accelerate 1.2 123.21) (ms!!3)  17.2
  , Reaction (Key 10) "name10" [ms!!9, ms!!10]           (cs!!10) (Accelerate 1.2 123.21) (ms!!4)  17.2
  , Reaction (Key 11) "name11" [ms!!10,ms!!15]           (cs!!11) (Accelerate 1.2 123.21) (ms!!14) 17.2
  , Reaction (Key 12) "name12" [ms!!11, ms!!12]          (cs!!12) (Accelerate 1.2 123.21) (ms!!10) 17.2
  , Reaction (Key 13) "name13" [ms!!12, ms!!13]          (cs!!13) (Accelerate 1.2 123.21) (ms!!16) 17.2
  , Reaction (Key 14) "name14" [ms!!13, ms!!15, ms!!16]  (cs!!14) (Accelerate 1.2 123.21) (ms!!17) 17.2
  , Reaction (Key 15) "name15" [ms!!14, ms!!17]          (cs!!15) (Accelerate 1.2 123.21) (ms!!18) 17.2
  , Reaction (Key 16) "name16" [ms!!15, ms!!17]          (cs!!16) (Accelerate 1.2 123.21) (ms!!19) 17.2
  , Reaction (Key 17) "name17" [ms!!0, ms!!18]           (cs!!17) (Accelerate 1.2 123.21) (ms!!11) 17.2
  , Reaction (Key 18) "name18" [ms!!16, ms!!19]          (cs!!18) (Accelerate 1.2 123.21) (ms!!12) 17.2
  , Reaction (Key 19) "name19" [ms!!0, ms!!19]           (cs!!19) (Accelerate 1.2 123.21) (ms!!13) 17.2
  , Reaction (Key 20) "name20" [ms!!17, ms!!10]          (cs!!11) (Accelerate 1.2 123.21) (ms!!15) 17.2
  ]

ms :: [Molecule]
ms =
  [ Molecule (Key 1) "smile1" "iup1"
  , Molecule (Key 2) "smile2" "iup2"
  , Molecule (Key 3) "smile3" "iup3"
  , Molecule (Key 4) "smile4" "iup4"
  , Molecule (Key 5) "smile5" "iup5"
  , Molecule (Key 6) "smile6" "iup6"
  , Molecule (Key 7) "smile7" "iup7"
  , Molecule (Key 8) "smile8" "iup8"
  , Molecule (Key 9) "smile9" "iup9"
  , Molecule (Key 10) "smile10" "iup10"
  , Molecule (Key 11) "smile11" "iup11"
  , Molecule (Key 12) "smile12" "iup12"
  , Molecule (Key 13) "smile13" "iup13"
  , Molecule (Key 14) "smile14" "iup14"
  , Molecule (Key 15) "smile15" "iup15"
  , Molecule (Key 16) "smile16" "iup16"
  , Molecule (Key 17) "smile17" "iup17"
  , Molecule (Key 18) "smile18" "iup18"
  , Molecule (Key 19) "smile19" "iup19"
  , Molecule (Key 20) "smile20" "iup20"
  ]

cs :: [Catalyst]
cs =
  [ Catalyst (Key 1) "smile1" (Just "name1")
  , Catalyst (Key 2) "smile2" (Just "name2")
  , Catalyst (Key 3) "smile3" (Just "name3")
  , Catalyst (Key 4) "smile4" (Just "name4")
  , Catalyst (Key 5) "smile5" (Just "name5")
  , Catalyst (Key 6) "smile6" (Just "name6")
  , Catalyst (Key 7) "smile7" (Just "name7")
  , Catalyst (Key 8) "smile8" (Just "name8")
  , Catalyst (Key 9) "smile9" (Just "name9")
  , Catalyst (Key 10) "smile10" Nothing
  , Catalyst (Key 11) "smile11" Nothing
  , Catalyst (Key 12) "smile12" Nothing
  , Catalyst (Key 13) "smile13" Nothing
  , Catalyst (Key 14) "smile14" Nothing
  , Catalyst (Key 15) "smile15" Nothing
  , Catalyst (Key 16) "smile16" Nothing
  , Catalyst (Key 17) "smile17" Nothing
  , Catalyst (Key 18) "smile18" Nothing
  , Catalyst (Key 19) "smile19" Nothing
  , Catalyst (Key 20) "smile20" Nothing
  ]
