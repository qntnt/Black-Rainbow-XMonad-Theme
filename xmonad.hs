import XMonad

import XMonad.Config.Desktop
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.PerWorkspace

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import System.IO

import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath

------------
-- COLORS --
------------

primaryColor = "#E0444F"
bgColor = "#2F2A30"
fgColor = "#B9B9B9"

------------------
-- BASIC CONFIG --
------------------

baseConfig = desktopConfig
getWallpaperDir = do
	homeDir <- getHomeDirectory
	return $ homeDir </> "Pictures/wallpapers"
myTerminal = "urxvt"
myModMask = mod4Mask
myBorderWidth = 3
myFocusedBorderColor = primaryColor
myNormalBorderColor = bgColor
getXMobarConfig = do
	homeDir <- getHomeDirectory
	return $ homeDir </> ".xmonad/.xmobarrc"

-------------
-- LAYOUTS --
-------------

gapLayout = gaps [(U, 20)]
spacedLayout = spacing 10
tiledLayout ratio = 
	spacedLayout $
	Tall nmaster delta ratio where
		nmaster = 1
		delta = 5/100
fullLayout = noBorders Full

defaultLayout = 
	gapLayout $ tiledLayout (2/3) ||| 
	Mirror (tiledLayout (1/2)) ||| 
	fullLayout
gimpLayout = gapLayout $ 
	spacedLayout $
	ThreeCol 2 (3/100) (3/4)
webLayout = gapLayout $ fullLayout ||| tiledLayout (2/3)

----------------
-- WORKSPACES --
----------------

myWorkspaces = ["1:main", "2:dev", "3:media", "4:gimp"]
myLayoutHook =
	onWorkspaces ["3:media"] webLayout $
	onWorkspaces ["4:gimp"] gimpLayout $
	defaultLayout

-----------------
-- MANAGEHOOKS --
-----------------

myManageHook = composeAll . concat $
	[ [ resource =? r --> doIgnore | r <- myIgnores]
	, [ className =? "Gimp" --> doShift "4:gimp"
	  , className =? "Gimp" --> doFloat
	  , className =? "Atom" --> doShift "2:dev"
	  , className =? "Nuvolaplayer3" --> doShift "3:media"
	  ]
	] where
		myIgnores = ["desktop", "desktop_window", "notify-osd"]

--------------------
-- SETUP DEFAULTS --
--------------------

defaults = baseConfig
	{ modMask = myModMask
	, terminal = myTerminal
	, borderWidth = myBorderWidth
	, layoutHook = myLayoutHook
	, workspaces = myWorkspaces
	, manageHook = myManageHook <+> manageHook defaultConfig
	, focusedBorderColor = myFocusedBorderColor
	, normalBorderColor = myNormalBorderColor
	}

----------
-- BARS --
----------

dmenuCommand = "dmenu"
genXMobarCommand = do
	xConfig <- getXMobarConfig
	return $ "xmobar --bgcolor="++ bgColor ++" --fgcolor="++ fgColor ++" "++ xConfig

----------------
-- BACKGROUND --
----------------

genBackgroundCommand = do
	wallpaperDir <- getWallpaperDir
	image <- chooseItem . listDirectory $ wallpaperDir
	homeDir <- getHomeDirectory
	return $ "feh --bg-fill "++ homeDir ++"/Pictures/wallpapers/"++ image

----------
-- MAIN --
----------

main = do
	xmobarCommand <- genXMobarCommand
	xmproc <- spawnPipe xmobarCommand
	bgCommand <- genBackgroundCommand
	bgproc <- spawnPipe bgCommand
	xmonad defaults
		{ logHook = dynamicLogWithPP xmobarPP
		  { ppOutput = hPutStrLn xmproc
		  , ppTitle = xmobarColor fgColor "" . shorten 75
		  , ppLayout = const ""
		  }
		}

-----------------------
-- UTILITY FUNCTIONS --
-----------------------

getTime :: IO Int
getTime = do
	time <- getPOSIXTime
	return $ round time

listDirectory :: String -> IO [ String ]
listDirectory dir = do
	files <- getDirectoryContents dir
	return $ filter dots files where
		dots x = and [x /= ".", x /= ".."]

chooseItem :: IO [ String ] -> IO String
chooseItem list = do
	time <- getTime
	l <- list
	return $ l !! (time `mod` (length l))
