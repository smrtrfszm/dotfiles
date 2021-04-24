{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import System.Exit (exitWith, ExitCode(..))

import XMonad.Actions.CycleWS (toggleWS)

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.EZConfig (mkKeymap)
import qualified XMonad.Util.ExtensibleState as XS (gets, modify, put)

import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Hooks.DynamicLog (ppOutput, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppUrgent, ppOrder, dynamicLogString, xmobarPP, xmobarColor, wrap, PP)
import XMonad.Hooks.ManageHelpers ((/=?))
import qualified XMonad.Hooks.EwmhDesktops as EW (fullscreenEventHook, ewmh)
import XMonad.Hooks.DynamicBars (DynamicStatusBar, dynStatusBarStartup, dynStatusBarEventHook, multiPP)
import XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)

import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)
import XMonad.Layout.IndependentScreens (countScreens)

import XMonad.Actions.DynamicWorkspaces (appendWorkspace)

import qualified XMonad.StackSet as W (focusDown, focusMaster, swapMaster, swapDown, swapUp, sink, greedyView, shift, view, shiftMaster)
import qualified Data.Map        as M (fromList)
import Data.Maybe (maybeToList)
import Data.Monoid (All)
import Control.Monad (join, liftM, when, unless)
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
    , ("M-n",        toggleWS)
    , ("M-S-d",      spawn "chromium --app=https://discord.com/app")
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
    XS.put $ Hidden []

data Hidden = Hidden { hidden :: [(ScreenId, WorkspaceId)] } deriving (Typeable, Show, Read)
instance ExtensionClass Hidden where
    initialValue = Hidden []
    extensionType = PersistentExtension


focusScreen :: ScreenId -> X ()
focusScreen si = screenWorkspace si >>= flip whenJust (windows . W.view)

hideScreen :: Int -> X ()
hideScreen si = do
    ws <- screenWorkspace $ S si
    whenJust ws $ \x -> do
        focusScreen $ S si
        appendWorkspace $ "hide" ++ show si
        XS.modify $ (\h -> Hidden $ h ++ [(S si, x)]) . hidden

hideScreens :: X ()
hideScreens = do
    screenNum <- countScreens
    hideScreens' screenNum

hideScreens' :: Int -> X ()
hideScreens' 0 = return ()
hideScreens' i = do
    hideScreen (i-1)
    hideScreens' (i-1)

showScreens' :: [(ScreenId, WorkspaceId)] -> X ()
showScreens' [] = return ()
showScreens' x = do
    focusScreen $ fst y
    windows $ W.greedyView $ snd y
    showScreens' $ tail x
    where
        y = head x

showScreens :: [(ScreenId, WorkspaceId)] -> X ()
showScreens ws = do
    showScreens' ws
    XS.put $ Hidden []

toggleWindows :: X ()
toggleWindows = do
    ws <- XS.gets hidden
    when (null ws) $ do
        hideScreens
    unless (null ws) $ do
        showScreens ws


-- Mouse bindings
myMouseBindings (XMonad.XConfig {modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
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

myEventHook :: Event -> X All
myEventHook = do
    dynStatusBarEventHook spawnStatusBar (return ())
    EW.fullscreenEventHook

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

spawnStatusBar :: DynamicStatusBar
spawnStatusBar i = spawnPipe $ "xmobar -x " ++ (show . fromEnum) i ++ " ~/.config/xmobar/config.hs"

myStartupHook :: X ()
myStartupHook = do
    dynStatusBarStartup spawnStatusBar (return ())
    addEWMHFullscreen
    spawnOnce "chromium --app=https://discord.com/app &"
    spawnOnce "transmission-gtk &"

myLogHook :: X ()
myLogHook = do
    currentWorkspaceOnTop
    multiPP statusBarPP statusBarPP

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
        , logHook            = myLogHook
        }

