import XMonad
import Data.Monoid
import System.Exit

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (doHideIgnore, (/=?))
import qualified XMonad.Hooks.EwmhDesktops as EW (fullscreenEventHook, ewmh)

import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

import XMonad.Actions.DynamicWorkspaces (appendWorkspace)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


--------------------------------------------------------------------------------
-- VARIABLES                                                                  --
--------------------------------------------------------------------------------

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
gapSize = 10
-- Modkey (windows key)
myModMask :: KeyMask
myModMask = mod4Mask
-- Normal border color
myNormalBorderColor :: String
myNormalBorderColor = "#181818"
-- Focused border color
myFocusedBorderColor :: String
myFocusedBorderColor = "#cb2520"
-- workspace names
myWorkspaces :: [String]
myWorkspaces = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9: Discord"]


--------------------------------------------------------------------------------
-- KEYBINDS                                                                   --
--------------------------------------------------------------------------------

myKeys conf = mkKeymap conf $
    -- Launch terminal
    [ ("M-<Return>", spawn terminalEmulator)
    -- Launch dmenu
    , ("M-o",        spawn "dmenu_run -h 20")
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
    -- Restart xmonad
    , ("M-r",        spawn "xmonad --restart")
    -- Recompile and restart xmonad
    , ("M-S-r",      spawn "xmonad --recompile;\
                     \killall xmobar; xmonad --restart")
    -- Open browse
    , ("M-b",        spawn webBrowser)
    -- Screenshot
    , ("<Print>",    spawn "sleep 0.2; scrot -sf\
                     \$HOME/Screenshots/%Y-%m-%d-%D:%M:%S.png")
    -- Toggle windows
    , ("M-d",        toggleWindows)
    -- Focus screens 0 and 1
    , ("M-w",        focusScreen 0)
    , ("M-e",        focusScreen 1)
    ]
    ++
    -- Select or shift to workspace
    [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip myWorkspaces $ map show [0..9]
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
        ]

-- Returns the number of screens
getScreenNum :: X Int
getScreenNum = getScreenNum' 0

-- Loop for getScreenNum
getScreenNum' :: Int -> X Int
getScreenNum' i = do
    ws <- screenWorkspace $ S i
    case ws of
        Nothing -> return i
        Just x  -> getScreenNum' (i+1)

-- Focuses the given screen or if invalid then does nothing
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
    screenNum <- getScreenNum
    hideScreens' screenNum

hideScreens' :: Int -> X ()
hideScreens' i
    | i == 0 = return ()
    | otherwise = do
        hideScreen (i-1)
        hideScreens' (i-1)
    

-- Hides every visible window on screen and reveals them when called again
toggleWindows :: X ()
toggleWindows = do
    hideScreens


--------------------------------------------------------------------------------
-- MOUSE BINDINGS                                                             --
--------------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

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


--------------------------------------------------------------------------------
-- LAYOUTS                                                                    --
--------------------------------------------------------------------------------

mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

myLayoutHook = smartBorders
    $ avoidStruts
    $ mySpacing gapSize
    $ (masterStack ||| Full)
    where
        masterStack = Tall 1 (3/100) (1/2)


--------------------------------------------------------------------------------
-- WINDOW RULES                                                               --
--------------------------------------------------------------------------------

myManageHook = composeAll
    [ className =? "discord"        --> doShift ( myWorkspaces !! 9 ) 
    , className =? "stalonetray"    --> doHideIgnore 
    , (className =? "Steam" <&&> title /=? "Steam") --> doFloat
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = EW.fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

--------------------------------------------------------------------------------
-- STARTUP HOOK                                                               --
--------------------------------------------------------------------------------

-- This executes everytime when xmonad starts or restarted (Mod + r)

myStartupHook = do
    spawnOnce "nitrogen --restore &"
    -- spawnOnce "compton &"
    spawn "xsetroot -cursor_name left_ptr"
    spawnOnce "dunst &"
    spawnOnce "stalonetray --window-type normal &"
    spawnOnce "discord &"


--------------------------------------------------------------------------------
-- MAIN                                                                       --
--------------------------------------------------------------------------------

-- This is the main entry point to the window manager

main = do
    -- Launch xmobar for both monitors
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/config.hs"
    xmproc1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/config.hs"
    xmonad 
        $ EW.ewmh
        $ fullscreenSupport
        $ docks def
        -- Configs
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
        , logHook            = myLogHook <+> dynamicLogWithPP xmobarPP
            -- Give workspace data to xmobar
            { ppOutput          = \x -> do
                hPutStrLn xmproc0 x
                hPutStrLn xmproc1 x
            , ppCurrent         = xmobarColor "#00ff00" "#333333" . wrap "[" "]"
            , ppVisible         = wrap "(" ")"
            , ppHidden          = xmobarColor "#cccccc" "" . wrap "-" "-"
            , ppHiddenNoWindows = xmobarColor "#888888" "" . wrap " " " "
            , ppUrgent          = xmobarColor "#ff0000" "" . wrap "!" "!"
            , ppOrder           = \(ws:_:_) -> [ws]
            }
        }
    
