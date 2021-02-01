{-# LANGUAGE FlexibleContexts #-}
module OrgSpec where

import           Test.Hspec
-- import           Test.QuickCheck

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Prim (Parsec, Stream)
import           Data.Functor.Identity (Identity)
import           Data.Either.Combinators (fromRight')
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

import           Org

parse'
  :: Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity t
  => Text.Parsec.Prim.Parsec s () a -> s -> Either ParseError a
parse' p = parse p ""

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

    describe "words" $ do
      it "plain" $ do
        -- when
        let res = fromRight' $ parse' word "Hello"
        -- then
        head res `shouldBe` Plain "Hello"

      it "plain - single asterisk in middle" $ do
        -- when
        let res = fromRight' $ parse' word "ast*erisk"
        -- then
        head res `shouldBe` Plain "ast*erisk"

      it "plain - single asterisk at start" $ do
        -- when
        let res = fromRight' $ parse' word "*asterisk"
        -- then
        head res `shouldBe` Plain "*asterisk"

      it "plain - end asterisk in word" $ do
        -- when
        let res = fromRight' $ parse' word "*aster*isk"
        -- then
        head res `shouldBe` Plain "*aster*isk"

      it "bold" $ do
        -- when
        let res = fromRight' $ parse' word "*Hello*"
        -- then
        head res `shouldBe` Bold (NE.fromList [Plain "Hello"])

      it "bold - pretended nesting" $ do
        -- when
        let res = fromRight' $ parse' word
              "*This *is* nested*"
        -- then
        head res `shouldBe` Bold (NE.fromList
          [ Plain "This"
          , Whitespace
          , Punct "*"
          , Plain "is"
          ])

      it "bold - over punctuation" $ do
        -- when
        let res = fromRight' $ parse' word
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
        let res = fromRight' $ parse' word "*Th*is*"
        -- then
        head res `shouldBe` Bold (NE.fromList [Plain "Th*is"])

      it "italic" $ do
        -- when
        let res = fromRight' $ parse' word "/italic/"
        -- then
        head res `shouldBe` Italic (NE.fromList [Plain "italic"])

