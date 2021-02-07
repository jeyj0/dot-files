{-# LANGUAGE FlexibleContexts #-}
module OrgSpec where

import           Test.Hspec
-- import           Test.QuickCheck

import           Data.Functor.Identity (Identity(Identity))
import           Text.Parsec (runParserT, ParsecT, Stream)
import           Text.Parsec.Error (ParseError)
import           Data.Functor.Identity (Identity)
import           Data.Either.Combinators (fromRight')
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

import           Org

parse'' :: Stream s Identity t =>
  u -> ParsecT s u Identity a -> s -> Either ParseError a
parse'' st p i =
  case runParserT p st "" i of
    Identity i -> i

parse' :: ParsecT String ParserState Identity a
  -> String -> Either ParseError a
parse' = parse'' defaultParserState

main :: IO ()
main = hspec $ do
  describe "full" $ do
    it "simple doc" $ do
      -- when
      let res = fromRight' $ parse' orgFileParser
            "#+TITLE: My simple document\n\
            \\n\
            \With one *paragraph* of one line."
      -- then
      res `shouldBe` OrgFile
        { orgMeta = M.fromList [("TITLE", "My simple document")]
        , orgDoc = OrgDoc
          { docBlocks =
            [ Paragraph $ NE.fromList
              [ Plain "With"
              , Whitespace
              , Plain "one"
              , Whitespace
              , Bold (NE.fromList [Plain "paragraph"])
              , Whitespace
              , Plain "of"
              , Whitespace
              , Plain "one"
              , Whitespace
              , Plain "line"
              , Punct "."
              ]
            ]
          , docSections = []
          }
        }

    it "handles multiple newlines between meta and doc" $ do
      -- when
      let res = fromRight' $ parse' orgFileParser
            "#+k: v\n\
            \\n\
            \\n\
            \p"
      -- then
      res `shouldBe` OrgFile
        { orgMeta = M.fromList [("k", "v")]
        , orgDoc = OrgDoc
          { docBlocks = [ Paragraph $ NE.fromList [ Plain "p" ] ]
          , docSections = []
          }
        }

    it "simple doc with two paragraphs" $ do
      -- when
      let res = fromRight' $ parse' orgFileParser 
            "#+TITLE: Two paragraphs\n\
            \\n\
            \This is the first paragraph\n\
            \\n\
            \This is the second paragraph."
      -- then
      res `shouldBe` OrgFile
        { orgMeta = M.fromList [("TITLE", "Two paragraphs")]
        , orgDoc = OrgDoc
          { docBlocks =
            [ Paragraph $ NE.fromList
                [ Plain "This"
                , Whitespace
                , Plain "is"
                , Whitespace
                , Plain "the"
                , Whitespace
                , Plain "first"
                , Whitespace
                , Plain "paragraph"
                ]
            , Paragraph $ NE.fromList
                [ Plain "This"
                , Whitespace
                , Plain "is"
                , Whitespace
                , Plain "the"
                , Whitespace
                , Plain "second"
                , Whitespace
                , Plain "paragraph"
                , Punct "."
                ]
            ]
          , docSections = []
          }
        }

  describe "meta" $ do
    describe "property" $ do
      it "parses a title" $ do
        -- when
        let res = fromRight' $ parse' property
              "#+TITLE: document title"
        -- then
        res `shouldBe` ("TITLE", "document title")

      it "parses only one pair" $ do
        -- when
        let res = fromRight' $ parse' property
              "#+KEY1: first value\n\
              \#+key2: second value"
        -- then
        res `shouldBe` ("KEY1", "first value")

    describe "properties" $ do
      it "parses multiple properties" $ do
        -- when
        let res = fromRight' $ parse' properties
              "#+key1: first value\n\
              \#+key2: second value"
        -- then
        res `shouldBe` M.fromList
          [ ("key1", "first value")
          , ("key2", "second value")
          ]

  describe "doc" $ do
    it "paragraph" $ do
      -- when
      let res = fromRight' $ parse' doc
            "Hello there!"
      -- then
      res `shouldBe` OrgDoc
        { docBlocks =
          [ Paragraph $ NE.fromList
            [ Plain "Hello"
            , Whitespace
            , Plain "there"
            , Punct "!"
            ]
          ]
        , docSections = []
        }

    describe "block" $ do
      it "paragraph" $ do
        -- when
        let res = fromRight' $ parse' block
              "Hello"
        -- then
        res `shouldBe` Paragraph (NE.fromList [Plain "Hello"])

      it "try tricking bold" $ do
        -- when
        let res = fromRight' $ parse' block
              "*not *bold"
        -- then
        res `shouldBe` Paragraph (NE.fromList
          [Plain "*not", Whitespace, Plain "*bold"])

      it "bold - pretended nesting" $ do
        -- when
        let res = fromRight' $ parse' block
              "*This *is* nested*"
        -- then
        res `shouldBe` Paragraph (NE.fromList
          [ Bold (NE.fromList
              [ Plain "This"
              , Whitespace
              , Plain "*is"
              ])
          , Whitespace
          , Plain "nested*"
          ])

    describe "textElement" $ do
      it "plain" $ do
        -- when
        let res = fromRight' $ parse' textElement "Hello"
        -- then
        head res `shouldBe` Plain "Hello"

      it "plain - single asterisk in middle" $ do
        -- when
        let res = fromRight' $ parse' textElement "ast*erisk"
        -- then
        head res `shouldBe` Plain "ast*erisk"

      it "plain - single asterisk at start" $ do
        -- when
        let res = fromRight' $ parse' textElement "*asterisk"
        -- then
        head res `shouldBe` Plain "*asterisk"

      it "plain - end asterisk in word" $ do
        -- when
        let res = fromRight' $ parse' textElement "*aster*isk"
        -- then
        head res `shouldBe` Plain "*aster*isk"

      it "bold" $ do
        -- when
        let res = fromRight' $ parse' textElement "*Hello*"
        -- then
        head res `shouldBe` Bold (NE.fromList [Plain "Hello"])

      it "bold - over punctuation" $ do
        -- when
        let res = fromRight' $ parse' textElement
              "*This. is. bold.*"
        -- then
        head res `shouldBe` Bold (NE.fromList
          [ Plain "This"
          , Punct "."
          , Whitespace
          , Plain "is"
          , Punct "."
          , Whitespace
          , Plain "bold"
          , Punct "."
          ])

      it "bold - including asterisk" $ do
        -- when
        let res = fromRight' $ parse' textElement "*Th*is*"
        -- then
        head res `shouldBe` Bold (NE.fromList [Plain "Th*is"])

      it "italic" $ do
        -- when
        let res = fromRight' $ parse' textElement "/italic/"
        -- then
        head res `shouldBe` Italic (NE.fromList [Plain "italic"])

