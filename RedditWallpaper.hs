{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (mzero)
import System.Directory (copyFile, removeFile)
import System.FilePath (splitFileName, takeExtension)
import System.Random (randomRIO)
import System.IO hiding (withFile)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EI
import Network.HTTP.Enumerator
import Control.Exception
import qualified System.Process as P
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.Enumerator
import Data.Enumerator (Iteratee)
import Data.ByteString (ByteString)
import Control.Applicative
import Data.List (isSuffixOf)

data Post = Post {
    nsfw :: Bool
  , uri :: String
  , title :: String
  } deriving Show
newtype Posts = Posts {getPosts :: [Post]} deriving Show

instance FromJSON Posts where
  parseJSON (Object v) = do
    v' <- v .: "data"
    Posts <$> v' .: "children"
  parseJSON _ = mzero

instance FromJSON Post where
  parseJSON (Object v) = do
    v' <- v .: "data"
    Post <$>
      v' .: "over_18" <*>
      v' .: "url" <*>
      v' .: "title"
  parseJSON _ = mzero

withFile :: FilePath -> (Handle -> IO ()) -> IO ()
withFile path f = bracket
  (openTempFile "/tmp" "img.png")
  (\(p,h) -> hClose h >> copyFile p path >> removeFile p)
  (\(_,h) -> f h)

runIt r iteratee = E.run_ . httpRedirect r iteratee
download :: String -> IO String
download url =
  let (_, filename) = splitFileName url
  in do
    request <- parseUrl url
    withFile filename $ \h -> withManager (runIt request (\_ _ -> EI.iterHandle h))
    return filename

makeBackground :: String -> IO ()
makeBackground path = P.readProcess "feh" ["--bg-scale", path] "" >> return ()

json' :: (Monad m) => Iteratee ByteString m Value
json' = iterParser json

posts :: String -> IO (Result Posts)
posts url = do
  request <- parseUrl url
  fromJSON <$> withManager (runIt request (\_ _ -> json'))

sfw = filter (not . nsfw)
img = filter ((`elem` filetypes) . takeExtension . uri)

filetypes :: [String]
filetypes = [".png", ".bmp", ".jpg"]

main :: IO ()
main = do
  p <- fmap (map uri . sfw . img . getPosts) <$> posts "http://www.reddit.com/r/wallpaper.json"
  case p of
       Error x -> putStrLn x
       Success x -> do
         r <- randomRIO (0, length x - 1)
         let img = x !! r
         download img >>= makeBackground
