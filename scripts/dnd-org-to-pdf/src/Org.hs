{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Org where

import           Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Prim (ParsecT, Stream)
import           Data.Char (isSpace)
import           Control.Monad (void)

org :: String -> Maybe OrgFile
org input =
  case parse orgFileParser "" input of
    Left _ -> Nothing
    Right o -> Just o

data OrgFile = OrgFile
  { orgMeta :: M.Map String String
  , orgDoc  :: OrgDoc
  } deriving (Eq, Show)

data OrgDoc = OrgDoc
  { docBlocks   :: [Block]
  , docSections :: [Section]
  } deriving (Eq, Show)

data Block
  = Paragraph (NE.NonEmpty TextElement)
  | Table (NE.NonEmpty Row)
  | List ListItems
  deriving (Eq, Show)

data Row = Break | Row (NE.NonEmpty Org.Column)
  deriving (Eq, Show)

data Column = Empty | Column (NE.NonEmpty TextElement)
  deriving (Eq, Show)

data TextElement
  = Bold (NE.NonEmpty TextElement)
  | Italic (NE.NonEmpty TextElement)
  | Monospace (NE.NonEmpty TextElement)
  | Link URL (Maybe (NE.NonEmpty TextElement))
  | Plain String
  | Punct String
  | Whitespace
  deriving (Eq, Show)

data Section = Section
  { sectionHeading :: NE.NonEmpty TextElement
  , sectionTags :: [String]
  , sectionDoc :: OrgDoc
  } deriving (Eq, Show)

newtype ListItems = ListItems (NE.NonEmpty Item)
  deriving (Eq, Show)

data Item = Item (NE.NonEmpty TextElement) (Maybe ListItems)
  deriving (Eq, Show)

newtype URL = URL String
  deriving (Eq, Show)

-----------------------------------------------------------------------

type OrgParser s t = GenParser Char s t
atLeastOne
  :: forall s (m :: * -> *) t u a
  .  Stream s m t
  => ParsecT s u m a -> ParsecT s u m [a]
atLeastOne = many1

followedBy
  :: forall s (m :: * -> *) t u a
  .  Stream s m t
  => ParsecT s u m a -> ParsecT s u m a
followedBy = lookAhead

orgFileParser :: OrgParser s OrgFile
orgFileParser = do
  properties' <- properties
  skipMany newline
  doc' <- doc
  eof
  return OrgFile
    { orgMeta = properties'
    , orgDoc = doc'
    }

properties :: OrgParser s (M.Map String String)
properties = many property <&> M.fromList

property :: OrgParser s (String, String)
property = do
  string "#+"
  key <- atLeastOne $ noneOf ":"
  char ':'
  spaces
  value <- atLeastOne $ noneOf "\n"
  optional newline

  pure (key, value)

doc :: OrgParser s OrgDoc
doc = do
  blocks <- atLeastOne block

  pure OrgDoc
    { docBlocks = blocks
    , docSections = []
    }

block :: OrgParser s Block
block = do
  words <- atLeastOne textElement <&> concat <&> NE.fromList
  optional (newline >> newline)

  pure $ Paragraph words

textElement :: OrgParser s [TextElement]
textElement = do
  w <- choice $ map try
    [ bold
    -- , italic
    , plain
    -- , punctuation
    -- , whitespace
    ]
  pure [w]
  where
    bold = do
      char '*'
      -- atLeastOne textElement
      textElement <- plain -- <|> punctuation
      stoppingChar '*'

      pure $ Bold $ NE.fromList [textElement]

      -- char '*'
      -- words <- atLeastOne word <&> concat <&> NE.fromList
      -- validStopper '*'
      -- pure $ Bold words

    -- italic = do
    --   char '/'
    --   words <- atLeastOne word <&> concat <&> NE.fromList
    --   validStopper '/'
    --   notFollowedBy alphaNum
    --   pure $ Italic words

    plain = do
      characters <- atLeastOne $ alphaNum <|> notStoppingChar '*' -- char '*'

      pure $ Plain characters

      -- let outerChar = alphaNum
      -- let innerChar = try outerChar <|> char '*'

      -- let wordEnd = do
      --       inner <- many innerChar
      --       last <- outerChar
      --       pure $ inner ++ [last]

      -- start <- atLeastOne outerChar
      -- end <- optionMaybe $ try wordEnd
      -- pure $ case end of
      --   Nothing -> Plain start
      --   Just end -> Plain $ start ++ end

    -- punctuation = do
    --   punct <- atLeastOne $ oneOf ".,:;!?'\"[]{}()"
    --   pure $ Punct punct

    whitespace = do
      atLeastOne whitespaceNotNewline
      pure Whitespace
      where
        whitespaceNotNewline :: OrgParser s Char
        whitespaceNotNewline =
          satisfy $ \c ->
            isSpace c && c /= '\n'

    stoppingChar :: Char -> OrgParser s Char
    stoppingChar c = try $ do
      char c
      followedBy eof
        -- void punctuation <|> void whitespace <|> eof
      pure c

    notStoppingChar :: Char -> OrgParser s Char
    notStoppingChar c = try $ do
      char c

      hasReachedEof <- optionMaybe $ followedBy eof
      case hasReachedEof of
        Nothing -> pure c
        Just _ -> unexpected "is a valid stopper"

