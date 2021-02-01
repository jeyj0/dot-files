{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import           Turtle
import qualified Data.Text as T
import           System.Exit (exitWith)
import           Data.Functor as F ((<&>))
import qualified Data.Map.Internal as M
import qualified Data.List.NonEmpty as NE
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Maybe (catMaybes)
import           Data.List.Extra as E (lower)

import           Util
import           Org as O

(>.>) :: (a -> b) -> (b -> c) -> a -> c
fun1 >.> fun2 = fun2 . fun1

map' :: (a -> b) -> Maybe a -> Maybe b
map' fun = \case
  Nothing -> Nothing
  Just v -> Just $ fun v

main :: IO ()
main =
  getInputStream >>= convert >.> \case
    Left err -> do
      putStrLn err
      exitWith $ ExitFailure 1
    Right out -> putStrLn out

getInputStream :: IO [Line]
getInputStream = collectInput []

collectInput :: [Line] -> IO [Line]
collectInput inputLines =
  collectInput' inputLines F.<&> reverse
  where
    collectInput' :: [Line] -> IO [Line]
    collectInput' inputLines' = do
      line <- readline
      case line of
        Nothing -> pure inputLines'
        Just line' -> collectInput' $ line' : inputLines'

convert :: [Line] -> Either String String
convert = convert' . unlines . map (T.unpack . lineToText)

convert' :: String -> Either String String
convert' orgString =
  case org orgString of
    Nothing -> Left "Couldn't parse org"
    Just orgFile -> Right $ latex orgFile

latex :: OrgFile -> String
latex orgFile =
  intercalate "\n" $ catMaybes [title, latexBlocks blocks, sectionsContent]
  where
    doc = orgDoc orgFile
    blocks = docBlocks doc
    sections = docSections doc

    title = latexTitle orgFile

    sectionsContent = latexSections sections

latexBlocks :: [Block] -> Maybe String
latexBlocks [] = Nothing
latexBlocks blocks =
  Just $ intercalate "\n\n" $ map latexBlock blocks

latexSections :: [Section] -> Maybe String
latexSections =
  latexSections' 0
  where
    latexSections' _ [] = Nothing
    latexSections' n sections =
      Just $ intercalate "\n" $ map (latexSection n) sections
      where
        latexSection :: Int -> Section -> String
        latexSection n section =
          intercalate "\n" $ catMaybes [Just $ latexHeader n tags header,
            latexBlocks $ docBlocks doc,
              latexSections' (n+1) $ docSections doc]
          where
            header = latexWords $ sectionHeading section
            tags = sectionTags section
            doc = sectionDoc section

        latexHeader :: Int -> [String] -> String -> String
        latexHeader 0 _ h = "\\section{" <> h <> "}"
        latexHeader 1 tags h =
          if "area" `elem` tags
            then "\\DndArea{" <> h <> "}"
            else "\\subsection{" <> h <> "}"
        latexHeader 2 tags h =
          if "subarea" `elem` tags
            then "\\DndSubArea{" <> h <> "}"
            else "\\subsubsection{" <> h <> "}"
        latexHeader 3 _ h = "\\subparagraph{" <> h <> "}"
        latexHeader _ _ _ = error "No definition for headers below fourth level"

latexBlock :: Block -> String
latexBlock = \case
  Paragraph words -> latexWords words
  Table rows ->
    "\\begin{DndTable}[]{lX}\n" <> unlines (NE.toList $ NE.map handleRow rows) <> "\\end{DndTable}"
  List listItems -> latexList listItems
  -- _ -> error "Unhandled type of block"

latexList :: ListItems -> String
latexList (ListItems items) =
  case list of
    Itemize items ->
      "\\begin{itemize}\n" <>
      items <>
      "\n\\end{itemize}"
    NamedParagraphs paragraphs ->
      paragraphs
  where
    list :: LatexList
    list = items
      |> NE.map latexItem
      |> latexList'

    latexList' :: NE.NonEmpty LatexListItem -> LatexList
    latexList' items =
      case NE.head items of
        Itemized {} ->
          items
            |> NE.map (\case
                Itemized t subList -> "\\item{" <> t <> "}" <>
                  (case subList of
                    Nothing -> ""
                    Just subList' -> "\n" <> subList')
                NamedParagraph n d subList -> "\\item{" <> n <> " :: " <> d <> "}" <>
                  (case subList of
                    Nothing -> ""
                    Just subList' -> "\n" <> subList'))
            |> NE.toList
            |> intercalate "\n"
            |> Itemize
        NamedParagraph {} ->
          items
            |> NE.map (\case
                NamedParagraph n d subList -> "\\paragraph{" <> n <> "} " <> d <>
                  (case subList of
                    Nothing -> ""
                    Just subList' -> "\n" <> subList')
                Itemized t subList -> "\\paragraph{" <> t <> "}" <>
                  (case subList of
                    Nothing -> ""
                    Just subList' -> "\n" <> subList'))
            |> NE.toList
            |> intercalate "\n\n"
            |> NamedParagraphs

data LatexList
  = Itemize String
  | NamedParagraphs String

data LatexListItem
  = Itemized String (Maybe String)
  | NamedParagraph String String (Maybe String)

latexItem :: Item -> LatexListItem
latexItem (Item words subItems) =
  case splitOn ":: " content of
    [content'] -> Itemized content' subList
    [name, definition] -> NamedParagraph name definition subList
    _ -> error "Can't have multiple :: in a list item."
  where
    content = latexWords words

    subList :: Maybe String
    subList = fmap latexList subItems

handleRow :: Row -> String
handleRow Break = " \\hline{} \\\\"
handleRow (O.Row column) = intercalate " & " (NE.toList $ NE.map handleColumn column) <> " \\\\"

handleColumn :: Column -> String
handleColumn Empty = ""
handleColumn (Column words) = latexWords words

data LatexWord
  = WithSpace String
  | NoSpace String

latexWords :: NE.NonEmpty Words -> String
latexWords = reduceLatexWords . NE.map latexWord

reduceLatexWords :: NE.NonEmpty LatexWord -> String
reduceLatexWords words =
  go (NE.toList words) ""
  where
    go :: [LatexWord] -> String -> String
    go [] t = t
    go ((WithSpace w):ws) "" = go ws w
    go ((NoSpace w):ws) "" = go ws w
    go ((WithSpace w):ws) t = go ws $ t <> " " <> w
    go ((NoSpace w):ws) t = go ws $ t <> "" <> w

latexWord :: Words -> LatexWord
latexWord = \case
  Plain t -> WithSpace t
  -- Punct c -> NoSpace $ c `cons` ""
  Bold t -> WithSpace $ "\\textbf{" <> t <> "}"
  Italic t -> WithSpace $ "\\textit{" <> t <> "}"
  Monospace t -> WithSpace $ "\\texttt{" <> t <> "}"
  Link (URL url) description ->
    WithSpace $ "\\emph{\\textcolor{rulered}{" <> t <> "}}"
      where
        t :: String
        t = case description of
              Nothing -> url
              Just t -> t <> "\\ExtLink"
  -- Tags tags -> NoSpace ""
  -- _ -> error "Unhandled word type"

latexTitle :: OrgFile -> Maybe String
latexTitle orgFile =
  let
    meta = M.mapKeys E.lower $ orgMeta orgFile
  in
  (\t -> "\\chapter{" <> t <> "}") <$> meta M.!? "title"

