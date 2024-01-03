{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Apigen.Language.Apidsl     as Apidsl
import qualified Apigen.Language.PyDsl      as PyDsl
import qualified Apigen.Parser              as Parser
import           Apigen.Parser.SymbolTable  (Name)
import           Apigen.Types               (Model)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO               as Text
import           Language.Cimple            (Lexeme)
import           Language.Cimple.IO         (parseProgram)
import qualified Language.Cimple.Program    as Program
import           System.Environment         (getArgs)


generate :: String -> FilePath -> Model (Lexeme Name) -> IO ()
generate "-json" output model = write output (LBS.putStrLn . Aeson.encode) Aeson.encodeFile model
generate "-py" output model   = write output Text.putStr Text.writeFile $ PyDsl.generate model
generate "-api" output model  = write output Text.putStr Text.writeFile $ Apidsl.generate model
generate lang _ _             = fail $ "invalid output: " <> lang

write :: FilePath -> (a -> IO ()) -> (FilePath -> a -> IO ()) -> a -> IO ()
write "-" wo _ = wo
write out _ wf = wf out


main :: IO ()
main = do
    args <- getArgs
    case args of
        (lang:output:srcs) -> do
            asts <- Program.toList <$> (parseProgram srcs >>= getRight)
            let model = Parser.parseModel asts
            generate lang output model
        _ -> fail "Usage: dump-model <-py|-json|-api> <output> [FILE]..."
    where
        getRight (Left err) = fail err
        getRight (Right ok) = return ok
