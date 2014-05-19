{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveDataTypeable   #-}


-- | The FileDialog snaplet requires an Auth snaplet and a file system
-- path, and gives you a splice containing a file dialog area
-- consisting of (1) a list of files already uploaded, (2) a checkbox
-- for each file and a deletion button, (3) an upload window, and (4)
-- a list of event notifications (file deleted etc.).  The current
-- setup is very simple.  See the TODO file for a list of limitations
-- and planned features.
-- 
-- snaplet-file-dialog is built around a `snap init` project skeleton
-- that has been extended by a few lines into a sample application.
-- Please go there for cut & paste instructions on how to use it.  The
-- steps are:
-- 
--   (1) Extend @Application.App@ with a FileDialog snaplet field.
-- 
--   (2) Call @FileDialogInit@ in @Site.app@.
-- 
--   (3) Use the <fileDialog/> tag in your templates (the name of the
--       tag is chosen in the call to @FileDialogInit@).
-- 
--   (4) Set routes.  Make sure that if no session is available, the
--       user gets an informative error message and a login prompt,
--       and not a @404@ (the FileDialog splice calls "pass" if no
--       user is available, and leaves handling of that case ot you.)
-- 
--   (5) (optional) Copy the css classes from @static/screen.css@ from
--       the distribution and put them in your own css file to tweak
--       the file dialog layout.  (there is currently no snaplet-style
--       css file handling; if you want this, please drop us an email.)

module Snap.Snaplet.FileDialog
  ( FileDialog ( .. )
  , CreateDirsOptions ( .. )
  , fileDialogInit
  , getUserHomeDir
  , createDirs
  )
where

import Control.Applicative ((<$>), (<|>))
import Control.Monad.State
import Data.List (sort)
import Data.Monoid
import Data.String.Conversions
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import Heist
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.Directory
import System.FilePath
import System.IO.Error
import System.Locale
import System.Posix.Files

import qualified Data.ByteString as S
import qualified Data.Map as M
import qualified Data.Text as T

-- cabal data file path handling magic.  check out neil mitchell's
-- blog article on this if it is bothering you.
import Paths_snaplet_file_dialog



-- * Snaplet

-- | The global "FileDialog" status type.
data FileDialog a = FileDialog
  { fileDialogRootDir    :: FilePath
      -- ^ home path in the file system.  (the home path in the URL is
      -- found by getSnapletRootURL.)

  , fileDialogCreateUserDirs :: Bool
      -- ^ if the home path of a user does not exist, create it.
      -- If @fileDialogRootDir </> "home"@ does not exist, trigger an
      -- error.

  , fileDialogAuth       :: SnapletLens a (AuthManager a)
      -- ^ the authentication snaplet that does the user
      -- identification.

  , fileDialogSizeLimit  :: Integer
      -- ^ files larger than this byte value are rejected with an
      -- error in the messages on the web page.
  }


-- | should the snaplet initializer implicitly create only user home
-- directories when needed, or also <root>, <root>/home, <root>/tmp,
-- or even all parents of <root>?
data CreateDirsOptions = CreateNone | CreateUserHomeOnly | CreateRootTree | CreateRootRecursive
  deriving (Typeable, Enum, Bounded, Eq, Ord, Show, Read)


-- | The application initializer.  This creates a splice containing
-- the "FileDialog" area and binds it to @tag@.
fileDialogInit ::
    HasHeist a =>
    T.Text                             -- ^ Name of the tag to be bound to the FileDialog splice.
 -> FilePath                           -- ^ File system root.
 -> CreateDirsOptions                  -- ^ Which directories should be created if missing?
 -> SnapletLens a (AuthManager a)      -- ^ Auth lens.
 -> SnapletLens a (FileDialog a)       -- ^ FileDialog lens.
 -> Snaplet (Heist a)                  -- ^ Heist Snaplet.
 -> Integer                            -- ^ Upload file size limit (bytes).
 -> SnapletInit a (FileDialog a)
fileDialogInit tag rootDir createDirsOptions authLens fileDialogLens heistSnaplet sizeLimit =
  makeSnaplet "filedialog" "The FileDialog snaplet." (Just getDataDir) $ do
    let fileDialog = FileDialog rootDir (createDirsOptions >= CreateUserHomeOnly) authLens sizeLimit
    snapletFilePath <- getSnapletFilePath
    addTemplates heistSnaplet ""

    addConfig heistSnaplet $ mempty
        { hcInterpretedSplices = [(tag, handleFileDialog fileDialogLens)] }

    addRoutes [ ("/c", fdSendFile)
              , ("/static", serveDirectory (snapletFilePath </> "static"))
              ]

    liftIO $ case createDirsOptions of
      CreateRootRecursive -> createDirs True rootDir
      CreateRootTree      -> createDirs False rootDir
      _                   -> return ()

    return fileDialog



-- * The FileDialog Heist Splice

-- | Handle requests of the file transfer page.  If user directory is
-- missing, it is silently created.
handleFileDialog :: forall a . HasHeist a => SnapletLens a (FileDialog a) -> SnapletISplice a
handleFileDialog fileDialogLens =
  do
    userHomeDir     :: FilePath <- lift $ with fileDialogLens getUserHomeDir
    tmpDir          :: FilePath <- lift $ with fileDialogLens getTmpDir
    r               :: Request  <- lift getRequest
    createUserDirs  :: Bool     <- fileDialogCreateUserDirs <$> lift (with fileDialogLens get)

    -- make sure directories exist.
    when createUserDirs $ liftIO $ createDirectoryIfMissing False userHomeDir

    -- delete all files that have been marked for deletion.
    deleteResult
      :: [T.Text]
      <- liftIO $ deleteFiles userHomeDir . M.filter (== ["del"]) . rqParams $ r

    -- process uploads, if any.
    uploadResult
      :: [T.Text]
      <- lift $ with fileDialogLens $ recvFiles userHomeDir tmpDir

    -- generate list of all files
    fs :: [FilePath] <- liftIO $ getDirectoryContents userHomeDir
                                 >>= filterM (doesFileExist . (userHomeDir </>))
                                     -- (ignore directories)

    renderFileTransferPage fileDialogLens (deleteResult ++ uploadResult) userHomeDir fs


-- | Render file transfer area with list of messages of what last
-- happened, a file directory with deletion checkboxes, and an upload
-- field.
renderFileTransferPage :: forall a .
                          SnapletLens a (FileDialog a) ->
                          [T.Text] -> [Char] -> [FilePath] ->
                          SnapletISplice a
renderFileTransferPage fileDialogLens msgs currentDir fileNames =
  do
    rootUrl :: S.ByteString <- lift $ with fileDialogLens getSnapletRootURL
    fileDialog :: FileDialog a <- lift $ with fileDialogLens get
    callTemplate (rootUrl <> "/" <> "filedialog") $
      ("filedialogHome", textSplice $ cs rootUrl) :
      ("filedialogMessages", mapSplices msgSplice msgs) :
      ("filedialogList", fileList $ cs rootUrl) :
      ("filedialogSizeLimit", textSplice . cs . showFileSize . fileDialogSizeLimit $ fileDialog) :
      []
  where
    msgSplice :: Monad m => T.Text -> Splice m
    msgSplice = runChildrenWithText . (:[]) . ("filedialogMessage",)
    -- (think: "msgSplice = textSplice", but that would not use the
    -- child node in the fileMessages tag.)

    fileList :: MonadIO m => T.Text -> Splice m
    fileList rootUrl = if null fileNames
      then textSplice "(empty directory)"
      else mapSplices (fileSplice rootUrl) $ sort fileNames

    fileSplice :: MonadIO m => T.Text -> FilePath -> Splice m
    fileSplice rootUrl = runChildrenWith . mkBindings rootUrl

    mkBindings :: MonadIO m => T.Text -> FilePath -> [(T.Text, Splice m)]
    mkBindings rootUrl fileName = bindings
      where
        filePath = currentDir </> fileName
        bindings =
          ("fileName",
             textSplice $ cs fileName) :
          ("fileURL",
             textSplice $ rootUrl <> "/c/" <> cs fileName) :
          ("fileDescription",
             textSplice $ cs fileName) :
          ("fileSize",
             liftIO (getFileSize filePath) >>= textSplice . cs . showFileSize) :
          ("fileLastWrite",
             liftIO (getFileLastWrite filePath) >>= textSplice . cs . showUTCTime) :
          []



-- * Handlers (API for other snaplets)

-- | get home directory of logged-in user (as registered in Auth snaplet).
getUserHomeDir :: Handler a (FileDialog a) FilePath
getUserHomeDir =
  do
    fileDialog <- get
    user <- withTop (fileDialogAuth fileDialog) currentUser
    case user of
      Nothing -> pass
      Just u -> return (fileDialogRootDir fileDialog </> "home" </> cs (userLogin u))


-- | Create missing directories, namely <root>, <root>/home,
-- <root>/tmp.  Do not create user homes: The @Snap.Snaplet.Auth@ API
-- does not provide a full list of users, and arguably you may not
-- want to create home directories for all of them.
--
-- There should be no need to call this function from outside this
-- module.  Snaplet initialization has a flag that lets you call it
-- implicitly.
createDirs :: Bool {- ^ create parents, too? -} -> FilePath {- ^ file dialog home -} -> IO ()
createDirs recursive root =
  do
    createDirectoryIfMissing recursive root
    createDirectoryIfMissing recursive (root </> "home")
    createDirectoryIfMissing recursive (root </> "tmp")



-- * Handlers (internal)

-- | Handle file deletion.
-- 
-- (We use posix @removeLink@, which deletes write-protected files,
-- but not directories.  See comment regarding use of posix rename in
-- @recvFiles@ below.)
deleteFiles :: FilePath -> Params -> IO [T.Text]
deleteFiles userHomeDir  params =
  if M.null params
    then return []
    else mapM delete_ . M.keys $ params
  where
    delete_ :: S.ByteString -> IO T.Text
    delete_ fn =
        catchIOError
          (removeLink (userHomeDir </> cs fn) >>
           return ("file " <> cs fn <> " deleted."))
          (\ e -> return $ "could not delete file " <> cs fn <> ": " <> cs (show e))


-- | Handle file upload.
-- 
-- (This will use posix @rename@ for moving temporary files to their
-- final destination.  @rename@ does not honour write permissions, and
-- it doesn't cross file system boundaries.  For our purposes that's
-- ok, but once we write down a more sophisticated authorisation
-- concept, we need to re-consider what to use from the OS level and
-- what to re-do from scratch.)
recvFiles :: FilePath -> FilePath -> Handler a (FileDialog a) [T.Text]
recvFiles userHomeDir tmpDir =
  do
    -- getRequest >>= liftIO . putStrLn . show
    sizeLimit <- fileDialogSizeLimit <$> get
    handleFileUploads tmpDir globalPolicy (localPolicy sizeLimit) ((join <$>) . mapM handle_)
     <|> return []
  where
    globalPolicy :: UploadPolicy
    globalPolicy = defaultUploadPolicy

    localPolicy :: Integer -> PartInfo -> PartUploadPolicy
    localPolicy sizeLimit _ = allowWithMaximumSize (fromIntegral sizeLimit)

    -- sample input for handle_
    -- (PartInfo {partFieldName = "upload",
    --            partFileName = Just "bookmarks.txt",
    --            partContentType = "text/plain"}
    -- ,Right "./static_private/snap-XN8X5fG")

    handle_ :: (PartInfo, Either PolicyViolationException FilePath) -> Handler a b [T.Text]
    handle_ (info, Left e) =
        logError msgInternal >> return [msgExternal]
      where
        msgInternal :: S.ByteString  = ("upload failed: " <> cs (show (info, e)))
        msgExternal :: T.Text        = cs $ show e

    handle_ (PartInfo { .. }, Right from) =
        liftIO $ do
          now <- showTime <$> getCurrentTime
          let to = case partFileName of
                     Just x -> cs x
                     Nothing -> now ++ "-" ++ takeFileName from
          if null to
            then return ["upload failed: please enter a file name or browse."]
            else do
              -- directories are not welcome on this server.  we just
              -- delete them without comment.  (XXX: implement
              -- sub-directory handling.)
              b <- doesDirectoryExist (userHomeDir </> to)
              when b $ removeDirectoryRecursive (userHomeDir </> to)

              -- move file, and report errors if there are any.
              catchIOError
                (rename from (userHomeDir </> to) >>
                 return ["file " <> cs to <> " uploaded."])
                (\ e -> return ["upload of file " <> cs to <> " failed: " <> cs (show e)])
      where
        showTime :: UTCTime -> String
        showTime = formatTime defaultTimeLocale "UTC %Y-%m-%d %H.%m.%s"


-- | Handle file download.
fdSendFile :: forall a . Handler a (FileDialog a) ()
fdSendFile =
  do
    r <- getRequest
    when (S.null $ rqPathInfo r) pass

    userHomeDir <- getUserHomeDir

    let e1 s = "Could not find filename " ++ show s ++ " for download."
        e2 = "Could not decode filename " ++ show (rqPathInfo r) ++ " for download."

    case urlDecode $ rqPathInfo r of
      Just s   -> sendFile (userHomeDir </> cs s) <|> error (e1 s)
      Nothing  -> error e2



-- * Auxiliaries

-- | get file size.
getFileSize :: FilePath -> IO Integer
getFileSize n =
  fromIntegral . fileSize <$> getFileStatus n


-- | re-implementation of System.Directory.getModificationTime (donno why I did this...)
getFileLastWrite :: FilePath -> IO UTCTime
getFileLastWrite n =
  posixSecondsToUTCTime . fromIntegral . fromEnum . modificationTime <$> getFileStatus n


-- | show all times in UTC, which is least confusing.  (XXX: build in
-- support for time zones.)
showUTCTime :: UTCTime -> String
showUTCTime =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"


-- | pretty printing of file sizes with kB, MB, GB rounding.  (this is
-- duplicated in the javaascript code.  we could probably do with one
-- implementation instead of two.)
showFileSize :: Integer -> String
showFileSize n
  | n `div` (1024*1024*1024) > 0 = show (n `div` (1024*1024*1024)) ++ " GB"
  | n `div` (1024*1024)      > 0 = show (n `div` (1024*1024))      ++ " MB"
  | n `div` (1024)           > 0 = show (n `div` (1024))           ++ " kB"
  | True                         = show n                          ++ " B"


-- | get tmp path.
getTmpDir :: Handler a (FileDialog a) FilePath
getTmpDir =
  do
    fileDialog <- get
    return (fileDialogRootDir fileDialog </> "tmp")
