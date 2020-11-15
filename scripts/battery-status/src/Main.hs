{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import           GHC.Float                      ( int2Float )
import           Text.Regex.TDFA                ( (=~~) )
import           BasicPrelude                   ( readMay )
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Turtle
import qualified Control.Foldl                 as FL

(>.>) :: (a -> b) -> (b -> c) -> a -> c
fun1 >.> fun2 = fun2 . fun1

main :: IO ()
main = do
  percentages <-
    (fold run FL.list)
      >>= (   catMaybes
          >.> map extractNumber
          >.> catMaybes
          >.> map (readMay :: T.Text -> Maybe Float)
          >.> catMaybes
          >.> return
          )

  let average = sum percentages / (int2Float $ length percentages)

  TIO.putStrLn
    $  T.pack (show average)
    <> " ("
    <> ( T.intercalate ", "
       $ map (\percent -> T.pack $ show percent <> "%") percentages
       )
    <> ")"

run :: Shell (Maybe T.Text)
run = do
  dev <- inshell "upower --enumerate" empty
  let devName = lineToText dev

  if T.isInfixOf "BAT" devName
    then do
      info <- inshell ("upower --show-info " <> devName) empty
      let infoText = lineToText info
      if T.isInfixOf "percentage:" infoText
        then return $ Just infoText
        else return Nothing
    else return Nothing

extractNumber :: T.Text -> Maybe T.Text
extractNumber t = t =~~ ("[0-9]+(\\.[0-9]+)?" :: T.Text)

