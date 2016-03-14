import XMonad

import XMonad.Actions.SpawnOn

import XMonad.Config.Desktop

import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.PerWorkspace

import XMonad.Hooks.FadeWindows
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

defaultLayout = gapLayout $
	tiledLayout (2/3) |||
	Mirror (tiledLayout (1/2)) |||
	fullLayout
gimpLayout = gapLayout $
	spacedLayout $
	ThreeCol 2 (3/100) (3/4)
mediaLayout = gapLayout $ tiledLayout (1/2) ||| tiledLayout (2/3) ||| fullLayout
devLayout = gapLayout $ tiledLayout (1/2) ||| tiledLayout (2/3)

----------------
-- WORKSPACES --
----------------
-- I:home II:dev III:media IV:gimp
myWorkspaces = ["I", "II", "III", "IV"]
myLayoutHook =
	onWorkspaces ["II"] devLayout $
	onWorkspaces ["III"] mediaLayout $
	onWorkspaces ["IV"] gimpLayout $
	defaultLayout

------------------
-- STARTUP HOOK --
------------------
myStartupHook = do
	spawnOn "III" "nuvolaplayer3"
	spawnOn "II" "urxvt"
	spawnOn "IV" "gimp"
	spawnOn "II" "atom"

-----------------
-- MANAGEHOOKS --
-----------------

myManageHook = composeAll . concat $
	[ [ resource =? r --> doIgnore | r <- myIgnores]
	, [ className =? "Gimp" --> doShift "IV"
	  , className =? "Nuvolaplayer3" --> doShift "III"
	  , className =? "Atom" --> doShift "II"
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
	, manageHook = manageSpawn <+> myManageHook <+> manageHook defaultConfig
	, focusedBorderColor = myFocusedBorderColor
	, normalBorderColor = myNormalBorderColor
	, startupHook = myStartupHook
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

myFadeHook = composeAll
	[ isUnfocused --> transparency 0.2
	, opaque
	]

genComptonCommand = do
	homeDir <- getHomeDirectory
	return $ "compton --config "++ homeDir ++"/.xmonad/compton.conf"

----------
-- MAIN --
----------

main = do
	comptonCommand <- genComptonCommand
	comptonproc <- spawnPipe comptonCommand
	xmobarCommand <- genXMobarCommand
	xmproc <- spawnPipe xmobarCommand
	bgCommand <- genBackgroundCommand
	bgproc <- spawnPipe bgCommand
	xmonad defaults
		{ logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor fgColor "" . shorten 75
			, ppLayout = const ""
			, ppCurrent = xmobarColor primaryColor ""
			, ppVisible = xmobarColor fgColor ""
			, ppHiddenNoWindows = const ""
			, ppWsSep = " "
			, ppSep = "   "
			}
		}

-----------------------
-- UTILITY FUNCTIONS --
-----------------------

getTime :: IO Int
getTime = do
	time <- getPOSIXTime
	return $ round time

--listDirectory :: String -> IO [ String ]
--listDirectory dir = do
--	files <- getDirectoryContents dir
--	return $ filter dots files where
--		dots x = and [x /= ".", x /= ".."]

chooseItem :: IO [ String ] -> IO String
chooseItem list = do
	time <- getTime
	l <- list
	return $ l !! (time `mod` (length l))
