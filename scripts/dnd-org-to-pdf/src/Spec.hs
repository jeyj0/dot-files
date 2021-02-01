{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Spec where

import           Test.Hspec
-- import           Test.QuickCheck

import qualified Data.Text as T
import           NeatInterpolation
import           Data.Either.Combinators (fromRight')

import           Main (convert')

main :: IO ()
main = hspec $ do
  describe "convert'" $ do
    describe "extracts title" $ do
      it "UPPERCASE" $ do
        -- when
        let latex = fromRight' $ convert' "#+TITLE: My awesome title"

        -- then
        latex `shouldBe` "\\chapter{My awesome title}"

      it "lowercase" $ do
        -- when
        let latex = fromRight' $ convert' "#+title: Another awesome title"

        -- then
        latex `shouldBe` "\\chapter{Another awesome title}"

      it "miXEd caSE" $ do
        -- when
        let latex = fromRight' $ convert' "#+tItLe: Best one"

        -- then
        latex `shouldBe` "\\chapter{Best one}"

    it "handles bold text" $ do
      -- when
      let latex = fromRight' $ convert' "This has *bold text*."

      -- then
      latex `shouldBe` "This has \\textbf{bold text}."

    it "handles italic text" $ do
      -- when
      let latex = fromRight' $ convert' "This has /italic text/."

      -- then
      latex `shouldBe` "This has \\textit{italic text}."

    it "handles monospace text" $ do
      -- when
      let latex = fromRight' $ convert' "This has ~monospace text~."

      -- then
      latex `shouldBe` "This has \\texttt{monospace text}."

    describe "handles links" $ do
      it "with destination and text" $ do
        -- when
        let latex = fromRight' $ convert'
              "This has a [[destination][complex link]]."

        -- then
        latex `shouldBe` "This has a \\emph{\\textcolor{rulered}{complex link\\ExtLink}}."

      it "without separate destination" $ do
        -- when
        let latex = fromRight' $ convert' "This has a [[simple link]]."

        -- then
        latex `shouldBe` "This has a \\emph{\\textcolor{rulered}{simple link}}."

    describe "handles tables" $ do
      it "simple table" $ do
        -- when
        let latex = fromRight' $ convert' "| header 1 | header 2 |\n| cell 1 | cell 2 |"

        -- then
        latex `shouldBe`
          "\\begin{DndTable}[]{lX}\nheader 1 & header 2 \\\\\ncell 1 & cell 2 \\\\\n\\end{DndTable}"

      it "with empty row" $ do
        -- when
        let latex = fromRight' $ convert' "| header 1 |  | header 3 |"

        -- then
        latex `shouldBe` "\\begin{DndTable}[]{lX}\nheader 1 &  & header 3 \\\\\n\\end{DndTable}"

    it "handles headers" $ do
      -- when
      let latex = fromRight' $ convert' $
            "* First-level header\n" <>
            "** Second-level header\n" <>
            "** Second-level area :area:\n" <>
            "*** Third-level header\n" <>
            "*** Third-level area :subarea:\n" <>
            "**** Fourth-level header"

      -- then
      latex `shouldBe`
          "\\section{First-level header}\n" <>
          "\\subsection{Second-level header}\n" <>
          "\\DndArea{Second-level area}\n" <>
          "\\subsubsection{Third-level header}\n" <>
          "\\DndSubArea{Third-level area}\n" <>
          "\\subparagraph{Fourth-level header}"

    describe "handles lists" $ do
      it "basic" $ do
        -- when
        let latex = fromRight' $ convert' "- item 1\n- item 2"

        -- then
        latex `shouldBe` "\\begin{itemize}\n\\item{item 1}\n\\item{item 2}\n\\end{itemize}"

      it "nested" $ do
        -- when
        let latex = fromRight' $ convert' "- item 1\n  - subitem 1\n  - subitem 2\n- item 2"

        -- then
        latex `shouldBe` "\\begin{itemize}\n" <>
          "\\item{item 1}\n" <>
            "\\begin{itemize}\n" <>
              "\\item{subitem 1}\n" <>
              "\\item{subitem 2}\n" <>
            "\\end{itemize}\n" <>
            "\\item{item 2}\n" <>
          "\\end{itemize}"

      it "definition" $ do
        -- when
        let latex = fromRight' $ convert' "- Name :: definition"

        -- then
        latex `shouldBe` "\\paragraph{Name} definition"

      it "definition with sublist" $ do
        -- when
        let latex = fromRight' $ convert' "- Name :: definition\n  - subList item"

        -- then
        latex `shouldBe` "\\paragraph{Name} definition\n\\begin{itemize}\n\\item{subList item}\n\\end{itemize}"

    it "handles a simple document" $ do
      -- when
      let latex = fromRight' $ convert' $ T.unpack [text|
          #+TITLE: My awesome NPC

          - Gender :: Male
          - Race :: Elf
          - Location :: Somewhere in the world

          * Some important headline
          With text inside

          ** And a second-level headline
        |]

      -- then
      latex `shouldBe` T.unpack [text|
          \chapter{My awesome NPC}
          \paragraph{Gender} Male

          \paragraph{Race} Elf

          \paragraph{Location} Somewhere in the world
          \section{Some important headline}
          With text inside
          \subsection{And a second-level headline}
        |]
