{-# LANGUAGE OverloadedStrings #-}
import Text.JSON
import Control.Monad
import System.Directory
import System.FilePath
import System.Random
import System.IO hiding (withFile)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EI
import Network.HTTP.Enumerator
import Data.Ascii
import Data.Maybe
import Control.Exception
import qualified System.Process as P
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy as T

object :: String -> Result [String]
object json = do
  j <- decode json
  children <- valFromObj "data" j >>= valFromObj "children"
  posts <- safeposts =<< mapM (valFromObj "data") children
  fmap (filter img) $ mapM (valFromObj "url") posts
  where img x = takeExtension x `elem` [".jpg", ".png"]
        safeposts = filterM (fmap not . valFromObj "over_18")

withFile :: FilePath -> (Handle -> IO ()) -> IO ()
withFile path f = bracket
  (openTempFile "/tmp" "img.png")
  (\(p,h) -> hClose h >> copyFile p path >> removeFile p)
  (\(_,h) -> f h)

download :: String -> IO String
download url =
  let (_, filename) = splitFileName url
  in do
    withFile filename $ \h -> withManager $ \m ->  do
      request <- parseUrl $ url
      E.run_ $ httpRedirect request (\_ _ -> EI.iterHandle h) m
    return filename

makeBackground :: String -> IO ()
makeBackground path = P.readProcess "feh" ["--bg-scale", path] "" >> return ()

main :: IO ()
main = do
  json <- fmap (T.unpack . TE.decodeUtf8) (simpleHttp "http://www.reddit.com/r/wallpaper.json")
  case resultToEither $ object json of
       Left x -> putStrLn x
       Right x -> do
         r <- randomRIO (0, length x - 1)
         let img = x !! r
         download img >>= makeBackground
