--------------------------------------------------------------------------------
-- IMPORTS                                                                    --
--------------------------------------------------------------------------------

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


--------------------------------------------------------------------------------
-- VARIABLES                                                                  --
--------------------------------------------------------------------------------

-- Set terminal emulator
terminalEmulator :: String
terminalEmulator = "alacritty"
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
myNormalBorderColor = "#dddddd"
-- Focused border color
myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"
-- workspace names
myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9: Discord"]


--------------------------------------------------------------------------------
-- KEYBINDS                                                                   --
--------------------------------------------------------------------------------

myKeys conf = mkKeymap conf $
    -- Launch terminal
    [ ("M-<Return>", spawn terminalEmulator)
    -- Launch dmenu
    , ("M-o", spawn "dmenu_run")
    -- Close focused window
    , ("M-S-c", kill)
    -- Resize viewed windows to the correct size
    , ("M-n", refresh)
    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)
    -- Focus the master window
    , ("M-m", windows W.focusMaster)
    -- Swap the focused window with the master window
    , ("M-S-m", windows W.swapMaster)
    -- Swap the focused window with the next window
    , ("M-S-j", windows W.swapDown)
    -- Swap the focosed window with the previous window
    , ("M-S-k", windows W.swapUp)
    -- Shrink the master area
    , ("M-h", sendMessage Shrink)
    -- Expand the master area
    , ("M-l", sendMessage Expand)
    -- Push window back into tiling 
    , ("M-t", withFocused $ windows . W.sink)
    -- Quit xmonad
    , ("M-S-q", io (exitWith ExitSuccess))
    -- Restart xmonad
    , ("M-r", spawn "killall xmobar; xmonad --recompile; xmonad --restart")
    -- Open browse
    , ("M-b", spawn "qutebrowser")
    -- Toggle windows
    -- , ("M-d," toggleWindows)
    ]
    ++
    [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip myWorkspaces $ map show [1..9]
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
        ]

toggleWindows = ()

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

mySpacing i = spacingRaw True (Border 0 i 0 i) True (Border i 0 i 0) True

myLayout = smartBorders
    $ avoidStruts
    $ mySpacing gapSize
    $ (masterStack ||| Full)
    where
        masterStack = Tall 1 (3/100) (1/2)


--------------------------------------------------------------------------------
-- WINDWO RULES                                                               --
--------------------------------------------------------------------------------

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "discord"        --> doShift "9: Discord"
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "compton &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/config"
    xmproc1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/config"
    xmonad $ fullscreenSupport $ docks def
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
        , layoutHook         = myLayout
        , startupHook        = myStartupHook 
        , logHook            = myLogHook <+> dynamicLogWithPP xmobarPP
            { ppOutput          = \x -> do
                hPutStrLn xmproc0 x
                hPutStrLn xmproc1 x
            , ppCurrent         = xmobarColor "#00ff00" "#333333" . wrap "[" "]"
            , ppVisible         = wrap "(" ")"
            , ppHidden          = xmobarColor "#cccccc" "" . wrap "*" "*"
            , ppHiddenNoWindows = xmobarColor "#888888" "" . wrap " " " "
            , ppUrgent          = xmobarColor "#ff0000" "" . wrap "!" "!"
            , ppOrder           = \(ws:_:_) -> [ws]
            }
        }
    

-- xmobarConfig = def {
--     font = "xft:Liberation Sans:pixelsize=14:antialias=true:hinting=true"
--     , bgColor = "#1d1d1d"
--     , fgColor = "white"
--     , alpha = 255
--     , position = Top
--     , lowerOnStart = True
--     , pickBroadest = False
--     , persistent = True
--     , hideOnStart = False
--     , allDesktops = True
--     , overrideRedirect = True
--     , commands =
--         [ Run Cpu ["-t", "Cpu: <total>%"] 10
--         , Run Memory ["-t", "Mem: <usedratio>%"] 10
--         , Run Date "%a %b %_d %H:%M:%S" "date" 10
--         ]
--     , sepChar = "%"
--     , alignSep = "}{"
--     , template = "}%date%{%cpu% | %memory% "
--     }
