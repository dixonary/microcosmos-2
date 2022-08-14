module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.FileEmbed (embedStringFile)
import Data.String.Interpolate
import GHC.Generics hiding (from)
import Data.Attoparsec.Text

import Database.SQLite.Simple

import System.FilePath
import System.Directory
import System.FSNotify
import System.Process
import System.ProgressBar
import Control.Concurrent 
import System.IO

import Data.Function 
import Data.Maybe
import Data.Functor
import Control.Applicative hiding ((<|>))
import Control.Applicative.Combinators (sepEndBy)
import Control.Monad
import Control.Monad.Extra (unlessM)

import Network.Wai
import Network.Wai.Handler.Warp 
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static
import Witch (from)
import System.Environment (getEnv)
import System.Environment.Blank (getEnvDefault)

-------------------------------------------------------------------------------
-- DATA TYPES AND CONSTANTS 

data CachedPost = CachedPost
  { pPath       :: FilePath
  , pWebPath    :: FilePath
  , pWebExtPath :: FilePath
  , pDir        :: FilePath
  , pSlug       :: FilePath
  , pTitle      :: Text
  , pDesc       :: Maybe Text
  , pContent    :: Text
  } deriving (Generic, Show)
instance FromRow CachedPost
instance ToRow CachedPost

data CachedDir = CachedDir
  { dPath     :: FilePath
  , dWebPath  :: FilePath
  , dDir      :: FilePath
  , dSlug     :: FilePath
  , dTitle    :: Text
  , dListing  :: Text
  } deriving (Generic, Show)
instance FromRow CachedDir
instance ToRow CachedDir

postDir           = "posts"
templateDir       = "templates"
staticDir         = "static"

getTemplatePath name = templateDir </> name <.> "html"

responsePlain st t = responseLBS st [("Content-Type", "text/plain")] t
responseHTML st t = responseLBS st [("Content-Type", "text/html")] t
notFound = responsePlain status404 "Not found"


-------------------------------------------------------------------------------
-- MAIN SERVER

main :: IO ()
main = do
  initialise
  conn <- open ":memory:"
  port <- getEnvDefault "" "MC_PORT" <&> \case { "" -> 3000; s -> read s }

  hSetBuffering stdout NoBuffering
  
  execute_ conn 
    [i| CREATE TABLE posts 
      ( path             TEXT PRIMARY KEY
      , web_path         TEXT
      , dir              TEXT
      , slug             TEXT UNIQUE
      , title            TEXT
      , desc             TEXT
      , content_fragment TEXT
      )
    |]

  execute_ conn
    [i| CREATE TABLE dirs
      ( path             TEXT PRIMARY KEY
      , web_path         TEXT
      , dir              TEXT
      , slug             TEXT UNIQUE
      , title            TEXT
      , listing_fragment TEXT
      )
    |]

  -- Create indices for faster lookup on valid paths
  execute_ conn [i|CREATE UNIQUE INDEX post_slug ON posts ( slug )|]
  execute_ conn [i|CREATE INDEX post_web_path ON posts ( web_path )|]
  execute_ conn [i|CREATE UNIQUE INDEX post_web_ext_path ON posts ( web_ext_path )|]
  execute_ conn [i|CREATE UNIQUE INDEX dir_slug ON dirs ( slug )|]
  execute_ conn [i|CREATE UNIQUE INDEX dir_web_path ON dirs ( web_path )|]

  cacheAllPosts conn
  cacheAllDirs conn

  forkIO $ watch conn
  forkIO $ watchTemplates conn
  
  run port 
    $ serve conn
    & staticPolicyWithOptions defaultOptions 
      (hasPrefix staticDir <|> only [("favicon.ico", "static/favicon.ico")])

-- Generate all the default files and folders.
initialise :: IO ()
initialise = do
  putStr "Initialising directories ..."

  doesDirectoryExist postDir `unlessM` do
    createDirectory postDir
    writeFile (postDir </> "home.md") 
             $(embedStringFile "def/posts/home.md")

  doesDirectoryExist templateDir `unlessM` do
    createDirectory templateDir
    writeFile (getTemplatePath "dir") 
             $(embedStringFile "def/templates/dir.html")
    writeFile (getTemplatePath "post") 
             $(embedStringFile "def/templates/post.html")

  doesDirectoryExist staticDir `unlessM` do
    createDirectory staticDir
    createDirectory $ staticDir </> "css"
    writeFile (staticDir </> "css" </> "main.css")
             $(embedStringFile "def/static/css/main.css")

  putStrLn " done."

-- Check if any of our beloved files have been updated, and recache it
watch :: Connection -> IO ()
watch conn = withManager \mgr -> do
  basePath <- getCurrentDirectory
  watchTree mgr postDir (const True) \case
    (Removed _ _ _) -> cacheAllPosts conn >> cacheAllDirs conn
    (Unknown _ _ _) -> pure ()
    ev              -> do
      -- A small delay (10ms) to ensure the write is complete
      threadDelay 10000
      let r = makeRelative basePath $ eventPath ev
      if eventIsDirectory ev
        then 
          putStr [i|Indexing #{r}...|]  >> cacheDir  r conn >> putStrLn "done."
        else 
          putStr [i|Compiling #{r}...|] >> cachePost r conn >> putStrLn "done."
  forever (threadDelay 1000000)


-- If one of the templates is updated, recache all files
watchTemplates :: Connection -> IO ()
watchTemplates conn = withManager \mgr -> do
  watchDir mgr templateDir (const True) \case
    (Modified fp _ _) 
      | takeBaseName fp == "dir"  -> cacheAllDirs conn
      | takeBaseName fp == "post" -> cacheAllPosts conn
    _                             -> pure ()
  forever (threadDelay 1000000)

  
-- Serve the relevant post or directory page
serve :: Connection -> Application
serve conn req respond = 
  case pathInfo req of { [] -> "home"; p -> joinPath $ from <$> p }
  & \slug -> do
      post <- query conn "SELECT * FROM posts WHERE slug = ? OR web_path = ?" 
              [slug,slug]
      dir  <- query conn "SELECT * FROM dirs WHERE slug = ? OR web_path = ?"
              [slug,slug]
      respond $ case (post, dir) of
        (p:_,_) -> responseHTML status200 $ from $ pContent p
        (_,d:_) -> responseHTML status200 $ from $ dListing d
        ([],[]) -> notFound


-------------------------------------------------------------------------------
-- FILE CACHING 

-- Perform actions for a number of elements in a list, with a progress bar
progressAll :: (a -> Text) -> (a -> IO ()) -> [a] -> IO ()
progressAll s a xs = do
  bar <- newProgressBar 
    (defStyle { stylePrefix = Label \p _ -> progressCustom p }) 
    50 (Progress 0 (length xs) "")
  forM_ xs \x -> do
    updateProgress bar (\b -> b { progressCustom = from $ s x })
    a x
    incProgress bar 1

fillTemplate :: FilePath -> [(Text,Text)] -> IO Text
fillTemplate templateName params = do
  let templatePath = getTemplatePath templateName
      fillMap = (\(x,y) -> ([i|<PLACEHOLDER name="#{x}">|],y)) <$> params

  rawTemplate <- T.readFile templatePath
  return $ foldr (uncurry T.replace) rawTemplate fillMap

cacheAllDirs :: Connection -> IO ()
cacheAllDirs conn = do
  putStrLn "Caching all dirs ..."
  getDirs "posts" 
    >>= progressAll (\d->[i|Indexing #{d}...|]) (`cacheDir` conn)

cacheAllPosts :: Connection -> IO ()
cacheAllPosts conn = do
  putStrLn "Caching all posts ..."
  getPosts "posts"
    <&> (filter $ not.null . takeBaseName)
    >>= progressAll (\p->[i|Compiling #{p}...|]) (`cachePost` conn)

cachePost :: FilePath -> Connection -> IO ()
cachePost d conn = do
  fileRaw <- T.readFile d

  case parseOnly postP fileRaw of
    Left err -> error err
    Right (headers, body) -> do
  
    (_,fragment,_) <- readProcessWithExitCode "pandoc" 
      ["-t", "html"] $ from body

    let 
      webExtPath = joinPath $ tail $ splitDirectories d
      webPath    = dropExtension webExtPath
      dir = takeDirectory webPath
      title = fromMaybe (from $ takeBaseName d) $ lookup "title" headers
      desc = lookup "description" headers

    let placeholderMap = 
          [("title", title)
          ,("parent-dir", "/" <> from dir)
          ,("parent-title", "up a level")
          ,("date", fromMaybe "" $ lookup "date" headers ) 
          ,("description", fromMaybe "" $ desc)
          ,("content", from fragment)
          ]
    content <- fillTemplate "post" placeholderMap

    execute conn [i|DELETE FROM posts WHERE path = ? |] [d]
    slug <- makeSlug d (from <$> lookup "slug" headers) conn
    execute conn [i| INSERT INTO posts VALUES (?, ?, ?, ?, ?, ?, ?, ?)|] 
      $ CachedPost d webPath dir slug webExtPath title desc content

cacheDir :: FilePath -> Connection -> IO ()
cacheDir d conn = do
  let webPath = joinPath $ tail $ splitDirectories d

  let 
    listDir CachedDir{..} = [i|<li class='dir'><a href='/#{dSlug}'>
      <p class='name'>#{dTitle}</p></a>
    </a></li>|]

    listPost CachedPost{..} = [i|<li class='post'><a href='/#{pSlug}'>
      <p class='name'>#{pTitle}</p><p class='date'></p>
    </a></li>|]

  dirListings <- map listDir
    <$> query conn [i|SELECT * FROM dirs  WHERE dir = ?|] [webPath] 
  postListings <- map listPost
    <$> query conn [i|SELECT * FROM posts WHERE dir = ?|] [webPath]
    
  let 
    title = from $ takeBaseName d
    listing = [i|<ul class="listing">
      <li class='dir'><a href="/#{takeDirectory webPath}">up a level</a></li>
      #{T.intercalate "\n" dirListings}
      #{T.intercalate "\n" postListings}
      </ul>|]
  
  dirContent <- fillTemplate "dir" [("title", title),("listing", listing)]

  execute conn [i|DELETE FROM dirs WHERE path = ?|] [d]
  slug <- makeSlug d Nothing conn
  execute conn [i| INSERT INTO dirs VALUES (?, ?, ?, ?, ?, ?)|] 
    $ CachedDir d webPath (takeDirectory webPath) slug title dirContent

-- Choose a slug for a path based on whether the obvious choice is taken,
-- and/or whether the route defines its own.
makeSlug :: FilePath -> (Maybe String) -> Connection -> IO String
makeSlug fp customSlug conn = do
  let s = fromMaybe (takeBaseName fp) customSlug
  usedP <- query conn [i|SELECT path FROM posts WHERE slug = ?|] [s]
  usedD <- query conn [i|SELECT path FROM dirs  WHERE slug = ?|] [s]
  return $ case usedP <> usedD :: [[String]] of
    []   -> from s
    _    -> fp

-- Get all directories in a root.
getDirs :: FilePath -> IO [FilePath]
getDirs fp = do
  fs <- map (fp </>) <$> listDirectory fp
  dirs <- filterM doesDirectoryExist fs
  (++dirs) . concat <$> traverse getDirs dirs

-- Get all posts in a root.
getPosts :: FilePath -> IO [FilePath]
getPosts fp = do
  fs    <- map (fp </>) <$> listDirectory fp
  files <- filterM doesFileExist fs
  dirs  <- filterM doesDirectoryExist fs
  (++files) . concat <$> traverse getPosts dirs

-- Parse the YAML header section out of a post.
postP :: Parser ([(Text, Text)], Text)
postP = let 
  fence       = trim $ takeWhile1 (=='-')
  skipHSpace  = Data.Attoparsec.Text.takeWhile isHorizontalSpace
  trim a      = skipHSpace *> a <* skipHSpace
  remQuotes s = if [T.head s,T.last s] == "\"\"" then T.init $ T.tail s else s
  record      = ((,) `on` (remQuotes . T.strip))
                 <$> (trim $ takeWhile1 (`notElem` ("-:"::String)))
                 <*> (":" *> (trim $ takeWhile1 $ not . isEndOfLine))
  header      = fmap (fromMaybe []) $ optional
                 $ fence *> endOfLine *> record `sepEndBy` endOfLine <* fence
    
  in (,) <$> header <*> takeText
  