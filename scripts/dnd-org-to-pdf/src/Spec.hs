{-# LANGUAGE OverloadedStrings #-}
module Spec where

import           Test.Hspec
-- import           Test.QuickCheck

import qualified Data.Text as T
import           Data.Either.Combinators (fromRight')

import           Main (convert')

main :: IO ()
-- main = putStrLn "Tests"
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

    it "handles headers" $ do
      -- when
      let latex = fromRight' $ convert' $
            "* First-level header\n" <>
            "** Second-level header\n" <>
            "*** Third-level header\n" <>
            "**** Fourth-level header"

      -- then
      latex `shouldBe`
          "\\section{First-level header}\n" <>
          "\\subsection{Second-level header}\n" <>
          "\\subsubsection{Third-level header}\n" <>
          "\\subparagraph{Fourth-level header}"

      -- it "first level" $ do
      --   -- when
      --   let latex = fromRight' $ convert' "* First-Level header"

      --   -- then
      --   latex `shouldBe` "\\section{First-Level header}"

      -- it "second level" $ do
      --   -- when
      --   let latex = fromRight' $ convert' "* First-level header\n** Second-level header"

      --   -- then
      --   latex `shouldBe` "\\section{First-level header}\n\\subsection{Second-level header}"
