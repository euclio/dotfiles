module Lib ( captureScreen, captureScreenRegion ) where

import Data.Time.Format
import Data.Time.LocalTime
import System.Environment.XDG.UserDir
import System.Directory
import System.FilePath
import XMonad.Util.Run

-- | Captures the active screen and saves a PNG in a subdirectory of the XDG
-- user pictures directory.
--
-- The file name is based on the current date and time.
captureScreen :: IO ()
captureScreen = do
    path <- newScreenshotPath
    createDirectoryIfMissing True (takeDirectory path)
    safeSpawn "maim" [path]

-- | Prompts the user to select a region of the active screen, then captures
-- that region and saves a PNG in a subdirectory of the XDG user pictures
-- directory.
--
-- The file name is based on the current date and time.
captureScreenRegion :: IO ()
captureScreenRegion = do
    path <- newScreenshotPath
    createDirectoryIfMissing True (takeDirectory path)
    safeSpawn "maim" ["-s", "--noopengl", path]

newScreenshotPath :: IO FilePath
newScreenshotPath = do
    time <- getZonedTime
    picturesDir <- getUserDir "PICTURES"
    let timestamp = formatTime defaultTimeLocale "%FT%T" time
    return $ picturesDir </> "screenshot" </> timestamp <.> "png"
