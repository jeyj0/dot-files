{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import MyPrelude
import qualified Data.Text as T

type Action = (Text, IO ())
newtype Actions = Actions [Action]

addActionToContext :: (?context :: Context) => Action -> IO ()
addActionToContext action = do
  mActions <- fromContext @Actions
  case mActions of
    Nothing -> putContext $ Actions [action]
    Just (Actions actions) -> putContext $ Actions (actions <> [action])

main :: IO ()
main = do
  theContext <- newContext
  let ?context = theContext
  creatableType <- chooseOne
    "❓ What do you want to create?"
    [HomeModule, SystemModule, Package]
  generateActions creatableType
  mActions <- fromContext @Actions

  case mActions of
    Nothing -> putStrLn "Nothing to do"
    Just (Actions actions) -> do
      let runActions = sequence_ $ map snd actions

      shouldRunActions <- confirm "❓ Are you sure you want to run everything?"
      if shouldRunActions then runActions else putStrLn "Cancelled"

confirm :: Text -> IO Bool
confirm prompt = do
  putStrLn $ prompt <> " [y/n]"
  l <- getLine
  pure $ l == "y"

homeManagerModulesMarker :: Text
homeManagerModulesMarker = "# dotgen home module marker"

hmModuleSimplePackageFile :: Text -> Text -> Text -> Text
hmModuleSimplePackageFile moduleName packageName pkgs = [trimming|
  { config
  , pkgs
  , lib
  , ...
  }:
  with lib;
  {
    options.jeyj0.$moduleName = {
      enable = mkEnableOption "${moduleName}";
    };

    config = mkIf config.jeyj0.${moduleName}.enable {
      home.packages = with ${pkgs}; [ ${packageName} ];
    };
  }
|]

generateActions :: (?context :: Context) => Creatable -> IO ()

generateActions HomeModule = do
  typeHomeModule <- chooseOne
    "❓ What type of module do you want?"
    [SimplePackage, HomeManagerProgram]
  case typeHomeModule of
    SimplePackage -> do
      packageName <- askLine "What is the package's name in nixpkgs?"

      moduleName <- askLineDefault packageName "What do you want to call the module?"
      putStrLn $ "Using " <> moduleName <> " as module name"

      folderPath <- addHMModule moduleName

      let filePath = folderPath <> "/default.nix"

      useUnstable <- confirm "❓ Do you want to use unstable nixpkgs?"

      let pkgs = if useUnstable then "pkgs.unstable" else "pkgs"

      let fileContents = hmModuleSimplePackageFile moduleName packageName pkgs

      addActionToContext
        ( "Write generated module code to " <> filePath
        , writeFile filePath fileContents
        )

    HomeManagerProgram -> do
      programName <- askLine "What is the program's name in home-manager?"
      moduleName <- askLineDefault programName "What do you want to call the module?"

      folderPath <- addHMModule moduleName

      overridePackage <- confirm "Do you want to override the program's package?"

      overridePackageText <- if overridePackage
        then do
          useUnstable <- confirm "Do you want to use unstable nixpkgs?"
          let pkgs = if useUnstable then "pkgs.unstable" else "pkgs"
          let empty = ""
          pure [trimming|
            ${empty}
            package = ${pkgs}.${programName};
          |]
        else pure ""

      addActionToContext
        ( "Write generated module code to " <> folderPath <> "/default.nix"
        , writeFile (folderPath <> "/default.nix") [trimming|
          { config
          , pkgs
          , lib
          , ...
          }:
          with lib;
          {
            options.jeyj0.$moduleName = {
              enable = mkEnableOption "${moduleName}";
            };

            config = mkIf config.jeyj0.${moduleName}.enable {
              programs.${programName} = {
                enable = true;${overridePackageText}
              };
            };
          }
        |]
        )

      pure ()

generateActions SystemModule = error "Sorry, I haven't implemented that yet"

generateActions Package = do
  packageName <- askLine "What should be the package's name?"
  rootFolder <- getRootFolder
  let folderPath = rootFolder <> "/packages/" <> packageName

  addActionToContext
    ( "Create directory: " <> folderPath
    , mkdirP folderPath
    )

  let flakeNixPath = rootFolder <> "/flake.nix"

  let packagesMarker = "# dotgen package marker"
  let packageEntryLine = packageName <> " = pkgs.unstable.callPackage (import ./packages/" <> packageName <> ") {};"

  addActionToContext
    ( "Add package to list in " <> flakeNixPath
    , insertLineBefore packagesMarker packageEntryLine flakeNixPath
    )

  let derivationPath = folderPath <> "/default.nix"

  addActionToContext
    ( "Create basic package derivation in " <> derivationPath
    , writeFile derivationPath [trimming|
      { lib
      , stdenv
      , pkgs
      , ...
      }:
      stdenv.mkDerivation {
        name = "${packageName}";
        src = ./.;
        installPhase = ''
          mkdir -p $$out/bin
          cat <<'EOF' >$$out/bin/hello-${packageName}
          #!/bin/sh
          echo "Hello, ${packageName}"
          EOF
          chmod +x $$out/bin/hello-${packageName}
        '';
      }
    |]
    )

addHMModule :: (?context :: Context) => Text -> IO Text
addHMModule moduleName = do
  rootFolder <- getRootFolder
  let folderPath = rootFolder <> "/modules/home-manager/" <> moduleName

  addActionToContext
    ( "Create directory: " <> folderPath
    , mkdirP folderPath
    )

  let homeManagerModulesListFilePath = rootFolder <> "/modules/home-manager/default.nix"

  addActionToContext
    ( "Add module to list in " <> homeManagerModulesListFilePath
    , insertLineBefore homeManagerModulesMarker ("./" <> moduleName) homeManagerModulesListFilePath
    )

  pure folderPath


askLine :: Text -> IO Text
askLine question = do
  putStrLn $ "❓" <> question
  getLine

askLineDefault :: Text -> Text -> IO Text
askLineDefault fallback question = do
  answer <- askLine question
  if answer == ""
    then do
      putStrLn $ "Using fallback: " <> fallback
      pure fallback
    else pure answer

chooseOne :: (Show a) => Text -> [a] -> IO a
chooseOne q [] = error $ "Can't choose from nothing"
chooseOne _ (c:[]) = pure c
chooseOne question choices = do
  putStrLn question

  choices
    |> mapWithIndex (\idx choice -> do
      putStrLn $ tshow idx <> ". " <> tshow choice)
    |> sequence_

  chosenIdx <- readLineOneOf "Type a number: " [1..length choices]

  pure $ choices !! (chosenIdx - 1)

readLineOneOf :: Show a => Text -> [a] -> IO a
readLineOneOf prompt opts = do
  putStrLn prompt
  l <- getLine
  case getChosen opts l of
    Nothing -> readLineOneOf prompt opts
    Just x -> pure x
  where
    getChosen :: Show a => [a] -> Text -> Maybe a
    getChosen [] l = Nothing
    getChosen (c:cs) l = if tshow c == l then Just c else getChosen cs l

data Creatable
  = HomeModule
  | SystemModule
  | Package
  deriving (Eq)

instance Show Creatable where
  show HomeModule = "home module"
  show SystemModule = "system module"
  show Package = "package"

data HomeModuleType
  = SimplePackage
  | HomeManagerProgram
  deriving (Eq)

instance Show HomeModuleType where
  show SimplePackage = "simple package"
  show HomeManagerProgram = "home manager program"

