{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Org where

import           Data.Functor ((<&>))
import           Data.Functor.Identity (Identity(Identity))
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import           Text.ParserCombinators.Parsec
import           Text.Parsec (runParserT, ParsecT, Stream, modifyState)
import           Data.Char (isSpace)
import           Control.Monad (void)

org :: String -> Maybe OrgFile
org input =
  case runParserT orgFileParser defaultParserState "" input of
    Identity m -> case m of
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

data ParserState = ParserState
  { isInBold :: Bool
  , isInItalic :: Bool
  }

defaultParserState :: ParserState
defaultParserState = ParserState
  { isInBold = False
  , isInItalic = False
  }

setIsInBold :: Bool -> ParserState -> ParserState
setIsInBold isInBold' state = ParserState
  { isInBold = isInBold'
  , isInItalic = isInItalic state
  }

setIsInItalic :: Bool -> ParserState -> ParserState
setIsInItalic isInItalic' state = ParserState
  { isInItalic = isInItalic'
  , isInBold = isInBold state
  }

type OrgParser t = ParsecT String ParserState Identity t

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

orgFileParser :: OrgParser OrgFile
orgFileParser = do
  properties' <- properties
  skipMany newline
  doc' <- doc
  eof
  return OrgFile
    { orgMeta = properties'
    , orgDoc = doc'
    }

properties :: OrgParser (M.Map String String)
properties = many property <&> M.fromList

property :: OrgParser (String, String)
property = do
  string "#+"
  key <- atLeastOne $ noneOf ":"
  char ':'
  spaces
  value <- atLeastOne $ noneOf "\n"
  optional newline

  pure (key, value)

doc :: OrgParser OrgDoc
doc = do
  blocks <- atLeastOne block

  pure OrgDoc
    { docBlocks = blocks
    , docSections = []
    }

block :: OrgParser Block
block = do
  words <- atLeastOne textElement <&> concat <&> NE.fromList
  optional (newline >> newline)

  pure $ Paragraph words

textElement :: OrgParser [TextElement]
textElement = do
  w <- choice $ map try
    [ bold
    , italic
    , plain
    , punctuation
    , whitespace
    ]
  pure [w]
  where
    bold = do
      state <- getState

      if isInBold state
        then unexpected "cannot nest bold text"
        else do
          char '*'

          modifyState $ setIsInBold True

          firstTextElement <- plain <|> punctuation
          textElements <- many textElement <&> concat
          stoppingChar '*'

          modifyState $ setIsInBold False

          pure $ Bold $ firstTextElement NE.:| textElements

    italic = do
      state <- getState

      if isInItalic state
        then unexpected "cannot nest italic text"
        else do
          char '/'

          modifyState $ setIsInItalic True

          firstTextElement <- plain <|> punctuation
          textElements <- many textElement <&> concat
          stoppingChar '/'

          modifyState $ setIsInItalic False

          pure $ Italic $ firstTextElement NE.:| textElements

    plain = do
      state <- getState

      characters <- if isInBold state
        then atLeastOne $ alphaNum <|> notStoppingChar '*'
        else atLeastOne $ alphaNum <|> char '*'

      pure $ Plain characters

    punctuation = do
      punct <- atLeastOne $ oneOf ".,:;!?'\"[]{}()"
      pure $ Punct punct

    whitespace = do
      atLeastOne whitespaceNotNewline
      pure Whitespace
      where
        whitespaceNotNewline :: OrgParser Char
        whitespaceNotNewline =
          satisfy $ \c ->
            isSpace c && c /= '\n'

    stoppingChar :: Char -> OrgParser Char
    stoppingChar c = try $ do
      char c
      followedBy $ eof <|> void whitespace
        -- void punctuation <|> void whitespace <|> eof
      pure c

    notStoppingChar :: Char -> OrgParser Char
    notStoppingChar c = try $ do
      isStoppingNext <- optionMaybe $ followedBy $ stoppingChar c
      case isStoppingNext of
        Just _ -> unexpected "is a valid stopper"
        Nothing -> char c

