module Tests.Lambda.Common (runShow, runString) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import Data.ByteString.Lazy       as BL
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL

import System.FilePath (takeDirectory, replaceBaseName, replaceDirectory, (</>))

srcPaths :: IO [FilePath]
srcPaths = findByExtension [".lam"] $ "test" </> "Tests" </> "Lambda" </> "Golden" </> "src"

---------------------- Helpers ----------------------------------

mkResultPath :: FilePath -> String -> FilePath
mkResultPath srcPath name =
  let dir = takeDirectory srcPath
      dir' = replaceBaseName dir name
  in replaceDirectory srcPath dir'

stringToByteString :: String -> ByteString
stringToByteString = TL.encodeUtf8 . TL.pack

showToBS :: Show a => a -> ByteString
showToBS item = stringToByteString $ show item

-------------------------- Special -----------------------------

runShow :: Show a => String -> (String -> a) -> IO TestTree
runShow name testFun = run name (showToBS . testFun)

runString :: String -> (String -> String) -> IO TestTree
runString name testFun = run name (stringToByteString . testFun)

------------------------- General ---------------------------------

run :: String -> (String -> ByteString) -> IO TestTree
run name testFun = do
  srcPaths' <- srcPaths
  tests <- mapM (mkTest name testFun) srcPaths'

  return $ testGroup name tests

mkTest :: String -> (String -> ByteString) -> FilePath -> IO TestTree
mkTest name testFun srcPath  = do
  src <- Prelude.readFile srcPath

  let result = testFun src
  let bsResult = result
  let resultPath = mkResultPath srcPath name
  let ioResult = return bsResult

  return $ golden resultPath ioResult

golden :: FilePath -> IO BL.ByteString -> TestTree
golden resultPath result =
  let diff :: FilePath -> FilePath -> [FilePath]
      diff ref new = ["diff", "-u", ref, new]

  in goldenVsStringDiff resultPath diff resultPath result