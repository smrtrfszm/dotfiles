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

import XMonad.Layout.Spacing

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


--------------------------------------------------------------------------------
-- VARIABLES                                                                  --
--------------------------------------------------------------------------------

-- Set terminal emulator
myTerminal = "alacritty"
-- Focus doesn't follow the mouse pointer
myFocusFollowsMouse = False
-- Clicking on a window passes the click to the window
myClickJustFocuses = False
-- Border width
myBorderWidth = 2
-- Gap size between borders
gapSize = 10
-- Modkey (windows key)
myModMask = mod4Mask
-- Normal border color
myNormalBorderColor = "#dddddd"
-- Focused border color
myFocusedBorderColor = "#ff0000"
-- workspace names
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]


--------------------------------------------------------------------------------
-- KEYBINDS                                                                   --
--------------------------------------------------------------------------------

myKeys conf = mkKeymap conf $
    -- Launch terminal
    [ ("M-<Return>", spawn myTerminal)
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
    , ("M-r", spawn "xmonad --recompile; xmonad --restart")]

    ++

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    --, ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip myWorkspaces $ map show [1..9]--["1", "2", "3", "4", "5", "6", "7", "8", "9"]
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
        ]


--------------------------------------------------------------------------------
-- Mouse Bindings                                                             --
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
-- Layouts                                                                    --
--------------------------------------------------------------------------------

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

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
    , className =? "Gimp"           --> doFloat
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
        xmproc <- spawnPipe "xmobar -x 0 /home/sztibi/.config/xmobar/config"
        xmproc <- spawnPipe "xmobar -x 1 /home/sztibi/.config/xmobar/config"
        xmonad $ docks defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
