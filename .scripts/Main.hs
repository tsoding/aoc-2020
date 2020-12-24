module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import Data.Functor
import Control.Monad

-- NOTE: Yt stands for YouTube. It's a YouTube link to the video
type Yt = T.Text

ytId :: Yt -> T.Text
ytId = head . T.splitOn "&" . (!! 1) . T.splitOn "?v="

ytThumbnail :: Yt -> T.Text
ytThumbnail yt = "http://i3.ytimg.com/vi/" <> ytId yt <> "/hqdefault.jpg"

ytMarkdownLink :: Yt -> T.Text
ytMarkdownLink yt = "[![screencast](" <> ytThumbnail yt <> ")](" <> yt <> ")"

ytMarkdownSection :: Yt -> [T.Text]
ytMarkdownSection yt = ["## Screencast", "", ytMarkdownLink yt]

-- NOTE: Ep stands for Episode
type Ep = T.Text

epFolder :: Ep -> IO FilePath
epFolder ep = head . filter (\dirPath -> ep `T.isPrefixOf` T.pack dirPath) <$> listDirectory "."

epREADME :: Ep -> IO FilePath
epREADME ep = (<> "/README.md") <$> epFolder ep

-- NOTE: Sc stands for Screencast
data Sc = Sc { scEp :: Ep, scYt :: Yt } deriving Show

parseSc :: T.Text -> Sc
parseSc s = Sc a b
  where [a, b] = T.splitOn ": " s

addSc :: Sc -> IO ()
addSc (Sc ep yt) = do
  readmePath <- epREADME ep
  readme <- T.lines <$> T.readFile readmePath
  let section = ytMarkdownSection yt
  T.writeFile readmePath $ T.unlines $ [head readme, ""] <> section <> tail readme

noSc :: Sc -> IO Bool
noSc (Sc ep _) =
  epREADME ep >>=
  T.readFile <&>
  T.lines <&>
  filter (T.isPrefixOf "## Screencast") <&>
  null

scs :: IO [Sc]
scs = map parseSc . T.lines <$> T.readFile "./.scripts/screencasts.txt"

main :: IO ()
main = scs >>= filterM noSc >>= mapM_ addSc
