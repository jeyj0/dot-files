{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module MyPrelude 
  ( module ClassyPrelude
  , module MyPrelude
  , module NeatInterpolation
  , (!!)
  ) where

import ClassyPrelude hiding (writeFile)
import Data.List ((!!))
import NeatInterpolation

import qualified Data.Text as T
import qualified Data.TMap as TypeMap
import qualified Data.Typeable as Typeable
import qualified Turtle as Turtle
import qualified Turtle.Pattern as P

-- (|>) :: a -> f -> f a
infixl 8 |>
a |> f = f a
{-# INLINE (|>) #-}

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f list = go 0 f list
  where
    go :: Int -> (Int -> a -> b) -> [a] -> [b]
    go _ _ [] = []
    go lastIdx f (x:xs) = f currentIdx x : go currentIdx f xs
      where
        currentIdx = lastIdx + 1

newtype Context = Context (IORef TypeMap.TMap)

newContext :: IO Context
newContext = do
  fieldsRef <- newIORef TypeMap.empty
  pure $ Context fieldsRef

putContext :: forall value. (?context :: Context, Typeable value) => value -> IO ()
putContext value = do
    let (Context fields) = ?context
    modifyIORef fields $ TypeMap.insert value
    pure ()

fromContext :: forall value. (?context :: Context, Typeable value) => IO (Maybe value)
fromContext = do
  let (Context fields) = ?context
  map <- readIORef fields
  pure $ TypeMap.lookup @value map

fromContext' :: forall value. (?context :: Context, Typeable value) => IO value
fromContext' = fromContext @value >>= \case
  Just v -> pure v
  Nothing -> error $ "Unable to get value of type '" <> show (Typeable.typeRep (Typeable.Proxy @value)) <> "' from context"

showContext :: forall value. (?context :: Context, Typeable value, Show value) => Text -> IO (Maybe value)
showContext t = fromContext @value >>= \case
  Just v -> do
    putStrLn $ t <> tshow v
    pure $ Just v
  Nothing -> do
    putStrLn $ t <> "not in context"
    pure Nothing

mkdirP :: Text -> IO ()
mkdirP folderPath = do
  Turtle.mktree $ Turtle.fromText folderPath

insertLineBefore :: Text -> Text -> Text -> IO ()
insertLineBefore searchLineContent lineToInsert file = do
  Turtle.inplace pattern $ Turtle.fromText file
  where
    pattern :: P.Pattern Text
    pattern = do
      whitespaceBeforeMarker <- P.spaces
      marker <- P.text searchLineContent
      pure [trimming|
        ${whitespaceBeforeMarker}${lineToInsert}
        ${whitespaceBeforeMarker}${marker}
      |]

writeFile :: Text -> Text -> IO ()
writeFile path = writeFileUtf8 (T.unpack path)

