module Tests.Lambda.Common (run) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import Data.ByteString.Lazy       as BL
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL

import System.FilePath (takeDirectory, replaceBaseName, replaceDirectory, (</>))

run :: Show a => String -> (String -> a) -> IO TestTree
run name testFun = do
  srcPaths' <- srcPaths
  tests <- mapM (mkTest name testFun) srcPaths'

  return $ testGroup name tests 

srcPaths :: IO [FilePath]
srcPaths = findByExtension [".lam"] $ "test" </> "Tests" </> "Lambda" </> "Golden" </> "src"

stringToByteString :: String -> ByteString
stringToByteString = TL.encodeUtf8 . TL.pack

mkResultPath :: FilePath -> String -> FilePath
mkResultPath srcPath name =
  let dir = takeDirectory srcPath
      dir' = replaceBaseName dir name
  in replaceDirectory srcPath dir'

mkTest :: Show a => String -> (String -> a) -> FilePath -> IO TestTree
mkTest name testFun srcPath  = do
  src <- Prelude.readFile srcPath

  let result = testFun src
  let bsResult = stringToByteString $ show result
  let resultPath = mkResultPath srcPath name
  let ioResult = return bsResult

  return $ golden resultPath ioResult

golden :: FilePath -> IO BL.ByteString -> TestTree
golden resultPath result =
  let diff :: FilePath -> FilePath -> [FilePath]
      diff ref new = ["diff", "-u", ref, new]

  in goldenVsStringDiff resultPath diff resultPath result