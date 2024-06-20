{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad (join, liftM, when, unless, forM_, liftM2)
import Control.Monad.Reader (asks, liftIO, ask)
import Control.Monad.State (gets)
import System.Exit (exitWith, ExitCode(..))

import Data.Maybe (maybeToList, isJust, fromMaybe)
import Data.Monoid (All)
import Data.List (intersperse, isPrefixOf)
import qualified Data.Map as M (fromList)

import XMonad.Config (def)
import XMonad.Core
import XMonad.Layout (Tall(..), Resize(..))
import XMonad.Main (xmonad)
import XMonad.ManageHook (composeAll, className, (=?), (<&&>), (<||>), (-->), doFloat, title, liftX, doShift)
import XMonad.Operations (kill, windows, sendMessage, withFocused, screenWorkspace, focus, mouseMoveWindow, isClient, float, mouseDrag, applySizeHintsContents)
import qualified XMonad.StackSet as W (focusDown, focusMaster, swapMaster, swapDown, swapUp, sink, greedyView, shift, view, shiftMaster, currentTag, workspace, current, visible, hidden, tag, findTag, stack)

import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces (appendWorkspace)
import XMonad.Actions.ToggleFullFloat
import qualified XMonad.Actions.FlexibleManipulate as Flex

import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare (WorkspaceSort, getSortByIndex)
import qualified XMonad.Util.ExtensibleState as XS (gets, modify, put)

import XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)
import XMonad.Hooks.DynamicLog (ppOutput, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppSort, ppUrgent, ppOrder, dynamicLogString, xmobarPP, xmobarColor, wrap, PP, ppWsSep, ppVisibleNoWindows)
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Hooks.ManageHelpers ((/=?), currentWs, isDialog, isInProperty)
import XMonad.Hooks.StatusBar (statusBarPipe, StatusBarConfig, dynamicSBs)
import XMonad.Hooks.StatusBar.PP (PP(..))

import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.Fullscreen

import Graphics.X11 (Dimension, KeyMask, Atom, mod4Mask, button1, button3, Window, raiseWindow, Position, resizeWindow)
import Graphics.X11.Xlib.Cursor (xC_left_ptr)
import Graphics.X11.Xlib.Extras (Event, getWindowProperty32, changeProperty32, propModeAppend, getWindowAttributes, getWMNormalHints, wa_width, wa_height)
import Graphics.X11.Xlib.Misc (queryPointer)

import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig))

import Brightness (setBrightness, incBrightness, decBrightness)
import GtkFrameExtents (gtkFrameExtents)


-- Variables

-- Set terminal emulator
terminalEmulator :: String
terminalEmulator = "kitty"
-- Focus doesn't follow the mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
-- Clicking on a window passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False
-- Border width
myBorderWidth :: Dimension
myBorderWidth = 2
-- Gap size between borders
gapSize :: Integer
gapSize = 8
-- Modkey (windows key)
myModMask :: KeyMask
myModMask = mod4Mask
-- Normal border color
myNormalBorderColor :: String
myNormalBorderColor = "#181818"
-- Focused border color
myFocusedBorderColor :: String
myFocusedBorderColor = "#86c1b9"
-- workspace names
myWorkspaces :: [String]
myWorkspaces = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
-- Status bar PP
statusBarPP :: PP
statusBarPP = def
    { ppCurrent         = xmobarColor "#A1B56C" "" . wrap "[" "]"
    , ppVisible         = xmobarColor "#F8F8F8" "" . wrap "(" ")"
    , ppHidden          = xmobarColor "#B8B8B8" "" . wrap "-" "-"
    , ppHiddenNoWindows = xmobarColor "#585858" "" . wrap " " " "
    , ppUrgent          = xmobarColor "#f7ca88" "" . wrap "!" "!"
    , ppSep             = ""
    , ppTitle           = \_ -> ""
    , ppLayout          = \_ -> ""
    , ppSort            = (.filterOutHideWs) <$> getSortByIndex
    }

filterOutHideWs :: WorkspaceSort
filterOutHideWs = filter (\workspace -> not $ isPrefixOf "hide-" (W.tag workspace))


-- Keybinds
myKeys conf = mkKeymap conf $
    [ ("M-<Return>",              spawn terminalEmulator)
    , ("M-o",                     spawn "rofi -show run")
    , ("M-<Backspace>",           kill)
    , ("M-<Tab>",                 windows W.focusDown)
    , ("M-m",                     windows W.focusMaster)
    , ("M-S-m",                   windows W.swapMaster)
    , ("M-S-j",                   windows W.swapDown)
    , ("M-S-k",                   windows W.swapUp)
    , ("M-h",                     sendMessage Shrink)
    , ("M-l",                     sendMessage Expand)
    , ("M-t",                     withFocused $ windows . W.sink)
    , ("M-S-q",                   io $ exitWith ExitSuccess)
    , ("M-x",                     spawn "dmenu-shutdown -h 24")
    , ("M-r",                     spawn "xmonad --restart")
    , ("M-S-r",                   spawn "xmonad --recompile; xmonad --restart")
    , ("M-b",                     spawn "librewolf")
    , ("M-C-b",                   spawn "librewolf --private-window")
    , ("<Print>",                 spawn "screenshot")
    , ("M-C-l",                   spawn "i3lock -f --color 000000")
    , ("M-d",                     toggleWindows)
    , ("M-w",                     focusScreen 0)
    , ("M-e",                     focusScreen 1)
    , ("M-f",                     withFocused $ sendMessage . maximizeRestore)
    , ("M-S-f",                   withFocused $ toggleFullFloat)
    , ("M-n",                     toggleWS)
    , ("M-<Space>",               spawn "dunstctl close-all")
    , ("<XF86MonBrightnessUp>",   incBrightness 0.1)
    , ("<XF86MonBrightnessDown>", decBrightness 0.1)
    ]
    ++
    -- Select or shift to workspace
    [("M-" ++ m ++ k, f i)
        | (i, k) <- zip myWorkspaces $ map show [0..9]
        , (f, m) <- [(changeWorkspace, ""), (windows . W.shift, "S-")]
        ]


changeWorkspace :: WorkspaceId -> X ()
changeWorkspace w = do
    windows $ W.greedyView w
    XS.put $ Hidden
        { hiddenWS = []
        , activeWS = ""
        }

data Hidden = Hidden
    { hiddenWS :: [(ScreenId, WorkspaceId)]
    , activeWS :: WorkspaceId
    } deriving (Typeable, Show, Read)

instance ExtensionClass Hidden where
    initialValue = Hidden
        { hiddenWS = []
        , activeWS = ""
        }
    extensionType = PersistentExtension


focusScreen :: ScreenId -> X ()
focusScreen si = screenWorkspace si >>= flip whenJust (windows . W.view)

toggleWindows :: X ()
toggleWindows = do
    ws <- XS.gets hiddenWS
    when (null ws) $ do
        screenNum <- countScreens
        aws <- withWindowSet (pure . W.currentTag)
        forM_ [0 .. (screenNum - 1)] $ \i -> do
            w <- screenWorkspace $ S i
            whenJust w $ \x -> do
                focusScreen $ S i
                appendWorkspace $ "hide-" ++ show i
                XS.modify $ (\h -> Hidden
                    { hiddenWS = h ++ [(S i, x)]
                    , activeWS = aws
                    }) . hiddenWS
    unless (null ws) $ do
        aws <- XS.gets activeWS
        forM_ ws $ \w -> do
            focusScreen $ fst w
            windows $ W.greedyView $ snd w
        windows $ W.view aws
        XS.put $ Hidden
            { hiddenWS = []
            , activeWS = ""
            }

myMouseBindings (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.position w))
    , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w))
    ]

-- Layouts
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

myLayoutHook = smartBorders
    $ avoidStruts
    $ maximizeWithPadding (fromIntegral gapSize)
    $ mySpacing gapSize
    $ Tall 1 (3/100) (1/2)

myManageHook :: ManageHook
myManageHook = composeAll
    [ ((className =? "steam" <||> className =? "steamwebhelper") <&&> title =? "Friends List") --> doFloat
    , (isHideWs) --> doShift (myWorkspaces !! 0)
    , (isDialog <||> isModal) --> doFloat
    ]
    where
        isModal = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"
        isHideWs = do
            ws <- liftX (withWindowSet $ return . W.currentTag)
            return $ isPrefixOf "hide-" ws

myEventHook :: Event -> X All
myEventHook = mempty

myStartupHook :: X ()
myStartupHook = do
    setDefaultCursor xC_left_ptr
    spawnOnce "picom"

myLogHook :: X ()
myLogHook = do
    currentWorkspaceOnTop

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner screen = do
    dir <- getXdgDirectory XdgConfig "xmobar"
    let command = concat ["xmobar -x ", (show . fromEnum) screen, " ", dir ++ "/config.hs"]
    statusBarPipe command (pure statusBarPP)

myConfig = def
    { terminal           = terminalEmulator
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook
    , logHook            = myLogHook
    }

main :: IO ()
main = xmonad
    $ dynamicSBs barSpawner
    $ toggleFullFloatEwmhFullscreen
    $ ewmhFullscreen
    $ ewmh
    $ gtkFrameExtents
    $ docks
    $ myConfig

