module Spec where

import Test.Hspec
-- import Test.QuickCheck

import Main (convert)

main :: IO ()
main = hspec $ do
    describe "convert" $ do
        it "handles the title" $ do
            -- when
            let out = convert ["#+TITLE: My title"]

            -- then
            out `shouldBe` ["\\chapter{My title}"]

        it "handles bold text" $ do
            -- when
            let out = convert ["This line has *bold text*."]

            -- then
            out `shouldBe` ["This line has \\textbf{bold text}."]

        it "handles italic text" $ do
            -- when
            let out = convert ["This line has /italic text/."]

            -- then
            out `shouldBe` ["This line has \\textit{bold text}."]

        it "handles monospace text" $ do
            -- when
            let out = convert ["This line has ~monospace text~."]

            -- then
            out `shouldBe` ["This line has \\texttt{monospace text}."]

        it "handles simple table" $ do
            -- when
            let out = convert
                    [ "| header cell 1 | header cell 2 |"
                    , "|---------------+---------------|"
                    , "| body cell 1   | body cell 2   |"
                    ]

            -- then
            out `shouldBe`
                [ "\\begin{DndTable}[]{lX}"
                , " header cell 1 & header cell 2 \\\\"
                , "\\hline \\\\"
                , " body cell 1   & body cell 2   \\\\"
                , "\\end{DndTable}"
                ]

        it "handles links" $ do
            -- when
            let out = convert
                    [ "This line contains a [[basic link]]."
                    , "This line contains an [[destination][external link]]."
                    ]

            -- then
            out `shouldBe`
                [ "This line contains a \\emph{\\textcolor{rulered}{basic link}}."
                , "This line contains a \\emph{\\textcolor{rulered}{external link\\\\ExtLink}}."
                ]

        describe "handles headers" $ do
            it "first level as section" $ do
                -- when
                let out = convert ["* My section"]

                -- then
                out `shouldBe` ["\\section{My section}"]

            it "second level as subsection" $ do
                -- when
                let out = convert ["** My subsection"]

                -- then
                out `shouldBe` ["\\subsection{My subsection}"]

            it "second level tagged area as area" $ do
                -- when
                let out = convert ["** My area :area:"]

                -- then
                out `shouldBe` ["\\DndArea{My area}"]

            it "third level as subsubsection" $ do
                -- when
                let out = convert ["*** My subsubsection"]

                -- then
                out `shouldBe` ["\\subsubsection{My subsubsection}"]

            it "third level tagged subarea as subarea" $ do
                -- when
                let out = convert ["*** My subarea :subarea:"]

                -- then
                out `shouldBe` ["\\DndSubArea{My subarea}"]

            it "fourth level as sub paragraph" $ do
                -- when
                let out = convert ["**** My sub paragraph"]

                -- then
                out `shouldBe` ["\\subparagraph{My sub paragraph}"]

        it "handles list definitions" $ do
            -- when
            let out = convert ["- Name :: Definition"]

            -- then
            out `shouldBe` ["\\paragraph{Name} Definition"]

        it "removes unhandled org options" $ do
            -- when
            let out = convert ["#+RANDOM_STUFF"]

            -- then
            out `shouldBe` []
