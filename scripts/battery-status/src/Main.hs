{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Text.Regex.TDFA ((=~~), (=~))
import BasicPrelude (readMay)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Turtle
import qualified Control.Foldl                 as FL

main :: IO ()
main = do
  energyLevels <- fold run FL.list

  let energyPairs :: [(Float, Float)]
      energyPairs = energyLevels
        & catMaybes
        & map extractNumber
        & pairElements
        & \case
          Nothing -> []
          Just pairs -> pairs
            & map maybePairs
            & catMaybes
            & unpair
            & map readMay
            & pairElements
            & \case
              Nothing -> []
              Just pairs' -> pairs'
                & map maybePairs
                & catMaybes

  let summed :: (Float, Float)
      summed = collect energyPairs

  let percentage = fst summed / snd summed

  let displayPercentage = percentage & show & T.pack & asDisplayPercentage

  TIO.putStrLn displayPercentage

asDisplayPercentage :: T.Text -> T.Text
asDisplayPercentage t =
  (t =~ ("\\.[0-9][0-9][0-9][0-9]" :: T.Text))
    & T.tail
    & T.unpack
    & dotAfterSnd
    & T.pack
  where
    dotAfterSnd (a:b:rst) = a:b:'.':rst
    dotAfterSnd str = str

run :: Shell (Maybe T.Text)
run = do
  dev <- inshell "upower --enumerate" empty
  let devName = lineToText dev

  if T.isInfixOf "BAT" devName
    then do
      info <- inshell ("upower --show-info " <> devName) empty
      let infoText = lineToText info
      if T.isInfixOf "energy-full:" infoText || T.isInfixOf "energy:" infoText
        then
          return $ Just infoText
        else
          return Nothing
    else
      return Nothing

extractNumber :: T.Text -> Maybe T.Text
extractNumber t = t =~~ ("[0-9]+(\\.[0-9]+)?" :: T.Text)

maybePairs :: (Maybe a, Maybe b) -> Maybe (a, b)
maybePairs (Nothing, _) = Nothing
maybePairs (_, Nothing) = Nothing
maybePairs (Just a, Just b) = Just (a, b)

pairElements :: [a] -> Maybe [(a, a)]
pairElements [] = Just []
pairElements (_:[]) = Nothing
pairElements (a:b:values) =
  case pairElements values of
    Nothing -> Nothing
    Just pairs -> Just $ (a,b):pairs

unpair :: [(a, a)] -> [a]
unpair [] = []
unpair ((a, b):pairs) = a:b:(unpair pairs)

collect :: forall a b . (Num a, Num b) => [(a, b)] -> (a, b)
collect [] = (0, 0)
collect ((a, b):others) =
  let
    (othersA, othersB) = collect others
  in
  (a + othersA, b + othersB)

