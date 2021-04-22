{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import System.Exit (exitWith, ExitCode(..))

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.EZConfig (mkKeymap)
import qualified XMonad.Util.ExtensibleState as XS (gets, modify)

import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Hooks.DynamicLog (ppOutput, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppUrgent, ppOrder, dynamicLogString, xmobarPP, xmobarColor, wrap, PP)
import XMonad.Hooks.ManageHelpers ((/=?))
import qualified XMonad.Hooks.EwmhDesktops as EW (fullscreenEventHook, ewmh)

import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)
import XMonad.Layout.IndependentScreens (countScreens)

import XMonad.Actions.DynamicWorkspaces (appendWorkspace)

import qualified XMonad.StackSet as W (focusDown, focusMaster, swapMaster, swapDown, swapUp, sink, greedyView, shift, view, shiftMaster)
import qualified Data.Map        as M (fromList)
import Data.Maybe (maybeToList)
import Control.Monad (join, liftM, when)
import GHC.IO.Handle (Handle)


-- Variables

-- Set terminal emulator
terminalEmulator :: String
terminalEmulator = "alacritty"
-- Set web browser
webBrowser :: String
webBrowser = "qutebrowser"
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
myWorkspaces = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9: Discord"]
-- Status bar PP
statusBarPP :: PP
statusBarPP = def
    { ppCurrent         = xmobarColor "#A1B56C" "" . wrap "[" "]"
    , ppVisible         = xmobarColor "#F8F8F8" "" . wrap "(" ")"
    , ppHidden          = xmobarColor "#B8B8B8" "" . wrap "-" "-"
    , ppHiddenNoWindows = xmobarColor "#585858" "" . wrap " " " "
    , ppUrgent          = xmobarColor "#ff0000" "" . wrap "!" "!"
    , ppOrder           = \(ws:_:_) -> [ws]
    }


-- Keybinds
myKeys conf = mkKeymap conf $
    -- Launch terminal
    [ ("M-<Return>", spawn terminalEmulator)
    -- Launch dmenu
    , ("M-o",        spawn "dmenu_run -h 24")
    -- Close focused window
    , ("M-<Backspace>",      kill)
    -- Resize viewed windows to the correct size
    , ("M-n",        refresh)
    -- Move focus to the next window
    , ("M-<Tab>",    windows W.focusDown)
    -- Focus the master window
    , ("M-m",        windows W.focusMaster)
    -- Swap the focused window with the master window
    , ("M-S-m",      windows W.swapMaster)
    -- Swap the focused window with the next window
    , ("M-S-j",      windows W.swapDown)
    -- Swap the focosed window with the previous window
    , ("M-S-k",      windows W.swapUp)
    -- Shrink the master area
    , ("M-h",        sendMessage Shrink)
    -- Expand the master area
    , ("M-l",        sendMessage Expand)
    -- Push window back into tiling 
    , ("M-t",        withFocused $ windows . W.sink)
    -- Quit xmonad
    , ("M-S-q",      io (exitWith ExitSuccess))
    -- Lauch custom dmenu script to prompt shutdown
    , ("M-x",        spawn "dmenu-shutdown -h 24")
    -- Restart xmonad
    , ("M-r",        spawn "xmonad --restart")
    -- Recompile and restart xmonad
    , ("M-S-r",      spawn "xmonad --recompile; xmonad --restart")
    -- Open browse
    , ("M-b",        spawn webBrowser)
    -- Screenshot
    , ("<Print>",    spawn "sleep 0.2 && screenshot")
    -- Lock screen
    , ("M-S-l",      spawn "slock")
    -- Toggle windows
    , ("M-d",        toggleWindows)
    -- Focus screens 0 and 1
    , ("M-w",        focusScreen 0)
    , ("M-e",        focusScreen 1)
    , ("M-f",        withFocused $ sendMessage . maximizeRestore)
    ]
    ++
    -- Select or shift to workspace
    [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip myWorkspaces $ map show [0..9]
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
        ]


focusScreen :: ScreenId -> X ()
focusScreen si = do
    ws <- screenWorkspace si
    case ws of
        Nothing -> return ()
        Just x  -> windows $ W.view x

hideScreen :: Int -> X ()
hideScreen si = do
    focusScreen $ S si
    appendWorkspace $ "hide" ++ show si

hideScreens :: X ()
hideScreens = do
    screenNum <- countScreens
    hideScreens' screenNum

hideScreens' :: Int -> X ()
hideScreens' 0 = return ()
hideScreens' i = do
    hideScreen (i-1)
    hideScreens' (i-1)

-- Hides every visible window on screen and reveals them when called again
toggleWindows :: X ()
toggleWindows = do
    hideScreens


-- Mouse bindings
myMouseBindings (XMonad.XConfig {modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


-- Layouts
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

myLayoutHook = smartBorders
    $ avoidStruts
    $ maximizeWithPadding (fromIntegral gapSize)
    $ mySpacing gapSize
    $ (masterStack ||| Full)
    where
        masterStack = Tall 1 (3/100) (1/2)


-- Window rules
myManageHook = composeAll
    [ (className =? "Steam" <&&> title /=? "Steam") --> doFloat
    ]


-- Event hook
myEventHook = EW.fullscreenEventHook

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

data Bars = Bars { bars :: [Handle] } deriving (Typeable)
instance ExtensionClass Bars where
    initialValue = Bars []

spawnBars' :: Int -> X()
spawnBars' 0 = return ()
spawnBars' i = do
    handle <- spawnPipe $ "xmobar -x " ++ show (i-1) ++ " ~/.config/xmobar/config.hs"
    hs <- XS.gets bars
    XS.modify $ \_ -> Bars $ hs ++ [handle]
    spawnBars' (i - 1)

spawnBars :: X ()
spawnBars = do
    screenNum <- countScreens
    spawnBars' screenNum

myStartupHook :: X ()
myStartupHook = do
    spawnBars
    addEWMHFullscreen
    spawnOnce "chromium --app=https://discord.com/app &"
    spawnOnce "transmission-gtk &"

sendStatusToBars' :: [Handle] -> String -> IO ()
sendStatusToBars' [] s = return ()
sendStatusToBars' h  s = do
    hPutStrLn (head h) s
    sendStatusToBars' (tail h) s

sendStatusToBars :: PP -> X ()
sendStatusToBars pp = do
    bs <- XS.gets bars
    s <- dynamicLogString pp
    io $ sendStatusToBars' bs s

main :: IO ()
main = do
    xmonad 
        $ EW.ewmh
        $ fullscreenSupport
        $ docks def
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
        , logHook            = sendStatusToBars statusBarPP
        }


