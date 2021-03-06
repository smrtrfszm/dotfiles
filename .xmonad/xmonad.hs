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
import XMonad.ManageHook (composeAll, className, (=?), (<&&>), (-->), doFloat, title, liftX, doShift)
import XMonad.Operations (kill, windows, sendMessage, withFocused, screenWorkspace, focus, mouseMoveWindow, isClient, float, mouseDrag, applySizeHintsContents)
import qualified XMonad.StackSet as W (focusDown, focusMaster, swapMaster, swapDown, swapUp, sink, greedyView, shift, view, shiftMaster, currentTag, workspace, current, visible, hidden, tag, findTag, stack)

import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces (appendWorkspace)

import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare (WorkspaceSort)
import qualified XMonad.Util.ExtensibleState as XS (gets, modify, put)

import XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)
import XMonad.Hooks.DynamicBars (DynamicStatusBar, dynStatusBarStartup, dynStatusBarEventHook, multiPPFormat, multiPP)
import XMonad.Hooks.DynamicLog (ppOutput, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppSort, ppUrgent, ppOrder, dynamicLogString, xmobarPP, xmobarColor, wrap, PP, ppWsSep, ppVisibleNoWindows)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Hooks.ManageHelpers ((/=?), currentWs, isDialog)
import XMonad.Hooks.UrgencyHook (readUrgents, withUrgencyHook, NoUrgencyHook(..))

import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (spacingRaw, Border(..))

import Graphics.X11 (Dimension, KeyMask, Atom, mod4Mask, button1, button3, Window, raiseWindow, Position, resizeWindow)
import Graphics.X11.Xlib.Cursor (xC_left_ptr)
import Graphics.X11.Xlib.Extras (Event, getWindowProperty32, changeProperty32, propModeAppend, getWindowAttributes, getWMNormalHints, wa_width, wa_height)
import Graphics.X11.Xlib.Misc (queryPointer)


-- Variables

-- Set terminal emulator
terminalEmulator :: String
terminalEmulator = "alacritty"
-- Set web browser
webBrowser :: String
webBrowser = "firefox-nightly"
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
    , ppUrgent          = xmobarColor "#f7ca88" "" . wrap "!" "!"
    }


-- Keybinds
myKeys conf = mkKeymap conf $
    [ ("M-<Return>",    spawn terminalEmulator)
    , ("M-o",           spawn "dmenu_run -h 24")
    , ("M-<Backspace>", kill)
    , ("M-<Tab>",       windows W.focusDown)
    , ("M-m",           windows W.focusMaster)
    , ("M-S-m",         windows W.swapMaster)
    , ("M-S-j",         windows W.swapDown)
    , ("M-S-k",         windows W.swapUp)
    , ("M-h",           sendMessage Shrink)
    , ("M-l",           sendMessage Expand)
    , ("M-t",           withFocused $ windows . W.sink)
    , ("M-S-q",         io $ exitWith ExitSuccess)
    , ("M-x",           spawn "dmenu-shutdown -h 24")
    , ("M-r",           spawn "xmonad --restart")
    , ("M-S-r",         spawn "xmonad --recompile; xmonad --restart")
    , ("M-b",           spawn webBrowser)
    , ("<Print>",       spawn "screenshot")
    , ("M-S-l",         spawn "slock")
    , ("M-d",           toggleWindows)
    , ("M-w",           focusScreen 0)
    , ("M-e",           focusScreen 1)
    , ("M-f",           withFocused $ sendMessage . maximizeRestore)
    , ("M-n",           toggleWS)
    , ("M-S-d",         spawn "dc")
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

mouseResizeWindow :: Window -> X ()
mouseResizeWindow w = whenX (isClient w) $ withDisplay $ \d -> do
    XConf { theRoot = root, display = d } <- ask
    wa                           <- io $ getWindowAttributes d w
    sh                           <- io $ getWMNormalHints d w
    (_, _, _, frx, fry, _, _, _) <- io $ queryPointer d root

    io $ raiseWindow d w
    float w

    mouseDrag (\ex ey -> do
        let dx = (fromIntegral frx) - ex
            dy = (fromIntegral fry) - ey
            width  = fromIntegral $ (wa_width  wa) - (fromIntegral dx)
            height = fromIntegral $ (wa_height wa) - (fromIntegral dy)

        io $ resizeWindow d w `uncurry` applySizeHintsContents sh (width, height)
        )
        (return ())


myMouseBindings (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))
    ]


-- Layouts
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

myLayoutHook = smartBorders
    $ avoidStruts
    $ maximizeWithPadding (fromIntegral gapSize)
    $ mySpacing gapSize
    $ Tall 1 (3/100) (1/2)
        

isHideWs :: Query Bool
isHideWs = do
    ws <- liftX (withWindowSet $ return . W.currentTag)
    return $ isPrefixOf "hide-" ws

-- Window rules
myManageHook = composeAll
    [ (className =? "Steam" <&&> title /=? "Steam") --> doFloat
    , (isHideWs) --> doShift (myWorkspaces !! 0)
    , (isDialog) --> doFloat
    ]

myEventHook :: Event -> X All
myEventHook = do
    dynStatusBarEventHook spawnStatusBar (return ())
    fullscreenEventHook

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

spawnStatusBar :: DynamicStatusBar
spawnStatusBar i = spawnPipe $ "xmobar -x " ++ (show . fromEnum) i ++ " ~/.config/xmobar/config.hs"

myStartupHook :: X ()
myStartupHook = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

    dynStatusBarStartup spawnStatusBar (return ())
    setDefaultCursor xC_left_ptr
    spawnOnce "dc"
    spawnOnce "transmission-gtk"

sepBy :: String -> [String] -> String
sepBy sep = concat . intersperse sep . filter (not . null)

pprWindowSet :: WorkspaceSort -> [Window] -> PP -> WindowSet -> String
pprWindowSet sort' urgents pp s = do
    let ws = map W.workspace (W.current s : W.visible s) ++ W.hidden s
    sepBy (ppWsSep pp) . map fmt . sort' . (filter filterHideWS) $ ws
    where
        filterHideWS = \x -> not $ isPrefixOf "hide-" (W.tag x)
        this         = W.currentTag s
        visibles     = map (W.tag . W.workspace) (W.visible s)
        fmt w = printer pp (W.tag w)
            where printer | any (\x -> maybe False (== W.tag w) (W.findTag x s)) urgents  = ppUrgent
                          | W.tag w == this                                               = ppCurrent
                          | W.tag w `elem` visibles && isJust (W.stack w)                 = ppVisible
                          | W.tag w `elem` visibles                                       = liftM2 fromMaybe ppVisible ppVisibleNoWindows
                          | isJust (W.stack w)                                            = ppHidden
                          | otherwise                                                     = ppHiddenNoWindows

formatter :: PP -> X String
formatter pp = do
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp
    return $ pprWindowSet sort' urgents pp winset


myLogHook :: X ()
myLogHook = do
    currentWorkspaceOnTop
    multiPPFormat formatter statusBarPP statusBarPP

main :: IO ()
main = do
    xmonad 
        $ ewmh
        $ fullscreenSupport
        $ withUrgencyHook NoUrgencyHook
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

