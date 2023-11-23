{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Apigen.Language.Haskell as Haskell
import           Data.List               (intercalate)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Language.Cimple.IO      (parseFiles)
import           System.Environment      (getArgs)


langs :: [String]
langs = ["go", "hs", "js", "kt", "py"]


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->
            fail $ "Usage: apigen <" <> langFlags <> "> [FILE]..."
        ('-':lang):_ | lang `notElem` langs ->
            fail $ "Invalid language. Supported: " <> langFlags
        _lang:srcs -> do
            asts <- parseFiles srcs >>= getRight
            mapM_ (\(file, text) -> do
                Text.putStrLn $ "-- " <> Text.pack file
                Text.putStrLn text) $ map Haskell.generate asts
    where
        getRight (Left err) = fail err
        getRight (Right ok) = return ok

        langFlags = intercalate "|" . map ("-"<>) $ langs
