{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import           Turtle
import qualified Data.Text as T
import Data.Maybe (mapMaybe)

(>.>) :: (a -> b) -> (b -> c) -> a -> c
fun1 >.> fun2 = fun2 . fun1

main :: IO ()
main = do
  inputLines <- collectInput [] >>= pure . reverse
  let texts = convert $ map (T.unpack . lineToText) inputLines
  mapM_ putStrLn texts

collectInput :: [Line] -> IO [Line]
collectInput inputLines = do
  line <- readline
  case line of
    Nothing -> pure inputLines
    Just line' -> collectInput $ line' : inputLines

convert :: [String] -> [String]
convert = mapMaybe convertLine

convertLine :: String -> Maybe String
convertLine ('#':'+':rest) =
  case rest of
    'T':'I':'T':'L':'E':':':' ':title -> Just $ "\\chapter{" ++ title ++ "}"
    _ -> Nothing
convertLine line@('*':rest) =
  case rest of
    ' ':section -> Just $ "\\section{" ++ section ++ "}"
    '*':rest' ->
      case rest' of
        ' ':subsection -> Just $ "\\subsection{" ++ subsection ++ "}"
        '*':rest'' ->
          case rest'' of
            ' ':subsubsection -> Just $ "\\subsubsection{" ++ subsubsection ++ "}"
            '*':rest''' ->
              case rest''' of
                ' ':subparagraph -> Just $ "\\subparagraph{" ++ subparagraph ++ "}"
                _ -> Just line
            _ -> Just line
        _ -> Just line
    _ -> Just line
convertLine line = Just line

