{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.ENEX (tests) where

import Control.Monad (liftM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Text (Text, unpack)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Class
import qualified Text.Pandoc.UTF8 as UTF8


enex :: Text -> Pandoc
enex = purely $ readEnex def

html :: Text -> Pandoc
html = purely $ readHtml def

tests :: [TestTree]
tests = [ testGroup "simple edge cases"
          [ test enex "empty" $
            "<en-export></en-export>" =?>
            doc mempty
          ]
        , testGroup "enex/*.enex gets parsed as enex/*.native" $
          map (testInputMatchesOutput enexToNativeConfig)
          [ "simple" ]
        ]

-- Helpers for file-based tests

newtype NoNormPandoc = NoNormPandoc {unNoNorm :: Pandoc}
  deriving ( Show )

instance ToString NoNormPandoc where
  toString d = unpack $
               purely (writeNative def{ writerTemplate = s }) $ toPandoc d
   where s = case d of
                  NoNormPandoc (Pandoc (Meta m) _)
                    | M.null m  -> Nothing
                    | otherwise -> Just "" -- need this for Meta output

instance ToPandoc NoNormPandoc where
  toPandoc = unNoNorm

getNoNormVia :: (a -> Pandoc) -> String -> Either PandocError a -> NoNormPandoc
getNoNormVia _ readerName (Left  _) = error (readerName ++ " reader failed")
getNoNormVia f _          (Right a) = NoNormPandoc (f a)

data ComparisonConfig m = ComparisonConfig
  { fixtureDir :: String
  , inputExtension :: String
  , outputExtension :: String
  , inputReaderOptions :: ReaderOptions
  , outputReaderOptions :: ReaderOptions
  , inputReader :: ReaderOptions -> Text -> m Pandoc
  , outputReader :: ReaderOptions -> Text -> m Pandoc
  }

enexToNativeConfig :: PandocMonad m => ComparisonConfig m
enexToNativeConfig = ComparisonConfig
  { fixtureDir = "enex/"
  , inputExtension = "enex"
  , outputExtension = "native"
  , inputReaderOptions = def
  , outputReaderOptions = def
  , inputReader = readEnex
  , outputReader = readNative
  }

testInputMatchesOutput :: ComparisonConfig PandocIO -> String -> TestTree
testInputMatchesOutput config basename =
  unsafePerformIO $ liftM (test id basename) (getInputAndOutput basename)
  where getInputAndOutput basename = do
          output <- getNoNormVia id (outputExtension config) <$> runOutputReader basename
          input <- getNoNormVia id (inputExtension config) <$> runInputReader basename
          return (input,output)

        runInputReader basename = do
          let inputPath = (fixtureDir config) ++ basename ++ "." ++ (inputExtension config)
          inputContent <- UTF8.toText <$> BS.readFile inputPath
          runIO $ (inputReader config) (inputReaderOptions config) inputContent

        runOutputReader basename = do
          let outputPath= (fixtureDir config) ++ basename ++ "." ++ (outputExtension config)
          outputContent <- UTF8.toText <$> BS.readFile outputPath
          runIO $ (outputReader config) (outputReaderOptions config) outputContent

{-|
          , test enex "simple" $
            "<en-export><note><title>hello</title><content><![CDATA[<!DOCTYPE en-note SYSTEM \"http://xml.evernote.com/pub/enml2.dtd\"><en-note><div>world</div></en-note>]]></content></note></en-export>" =?>
            (doc $ header 1 "hello" <> divWith nullAttr (para $ text "world"))
          ]
-}
