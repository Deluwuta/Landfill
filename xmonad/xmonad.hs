--             __  __                       _   _                                           _     _ 
--     /\     |  \/  |                     | | (_)                                         (_)   | |
--    /  \    | \  / | ___  _ __   __ _  __| |  _ ___    __ _   _ __ ___   ___  _ __   ___  _  __| |
--   / /\ \   | |\/| |/ _ \| '_ \ / _` |/ _` | | / __|  / _` | | '_ ` _ \ / _ \| '_ \ / _ \| |/ _` |
--  / ____ \  | |  | | (_) | | | | (_| | (_| | | \__ \ | (_| | | | | | | | (_) | | | | (_) | | (_| |
-- /_/    \_\ |_|  |_|\___/|_| |_|\__,_|\__,_| |_|___/  \__,_| |_| |_| |_|\___/|_| |_|\___/|_|\__,_|
--                                                                                                  
-- DeltΔ's config file.
--

import XMonad
import Data.Monoid
import System.Exit

 -- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeysP)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doRectFloat)
import XMonad.Layout.Spacing (spacingWithEdge)

 -- Hooks
-- import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, PP(..))
import XMonad.Hooks.EwmhDesktops -- Useful for Polybar!

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--

myTerminal :: String
myTerminal = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 3

-- mod1Mask = left alt | mod3Mask = right alt | mod4Mask = windows key (super)
myModMask :: KeyMask
myModMask = mod4Mask

-- myWorkspaces = ["web", "irc", "code" ] ++ map show [4..9]
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces = ["A", "B", "C", "D", "E", "F"]

-- (I think) Useful if you want Xmobar to be clickable (not fully)
-- myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor :: String
myNormalBorderColor  = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#00ff00"

-- Keys 2.0
myKeys :: [(String, X())]
myKeys = 
    [
      -- Quit and Restart, in that order
        ("M-S-q"      , io (exitWith ExitSuccess)),
        ("M-S-r"      , spawn "xmonad --recompile; xmonad --restart"),
        
      -- Oh? You're approaching me?
        ("M-S-c"      , kill),

      -- Common actions
        ("M-<Return>" , spawn (myTerminal)),
        ("M-d"        , spawn ("rofi -show run")),
        ("M-b"        , spawn ("brave")),
        ("M-f"        , spawn ("thunar")),

      -- Scratchpads (ye)
        
      -- Window swapin
        ("M-S-<Space>", windows W.swapMaster),
        ("M-S-j"      , windows W.swapDown),
        ("M-S-k"      , windows W.swapUp),

      -- Window navigation
        ("M-<Tab>"    , windows W.focusDown),
        ("M-j"        , windows W.focusDown),
        ("M-k"        , windows W.focusUp),
        ("M-<Space>"  , windows W.focusMaster),

      -- Window size modification
        ("M-S-h"      , sendMessage Shrink),
        ("M-S-l"      , sendMessage Expand),

      -- Refresh size | Force to Tile
        ("M-C-n"      , refresh), -- Refresh w size
        ("M-S-t"      , withFocused $ windows . W.sink),

      -- Unused / useless functionalities
--      -- Increment | Deincrement number of windows in master
--        ("M-,"        , sendMessage (IncMasterN 1)),
--        ("M-."        , sendMessage (IncMasterN (-1))),

      -- Layout switching | Hide bar 
        ("M-C-<Tab>"  , sendMessage NextLayout),
        ("M-S-<Tab>"  , sendMessage ToggleStruts) -- Tapa la barra (No va con Polybar)
   ]

------------------------------------------------------------------------
-- Leftovers of old keybindings (might be helpful)
--      --
--      -- mod-[1..9], Switch to workspace N
--      -- mod-shift-[1..9], Move client to workspace N
--      --
--        [((m .|. modm, k), windows $ f i)
--            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--        ++
--      --
--      -- mod-{w,e,y}, Switch to physical/Xinerama screens 1, 2, or 3
--      -- mod-shift-{w,e,y}, Move client to screen 1, 2, or 3
--      --
--        [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--            | (key, sc) <- zip [xK_w, xK_e, xK_y] [0..]
--            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
--
--    
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
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

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (spacingWithEdge 6 $ tiled ||| Mirror tiled ||| Full)
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
--
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
myManageHook = composeAll [ 
    className =? "MPlayer"        --> doFloat, 
    className =? "Gimp"           --> doFloat,
    appName   =? "galculator"     --> doRectFloat(W.RationalRect 0.33 0.2 0.25 0.5), -- x y w h
    resource  =? "desktop_window" --> doIgnore,
    resource  =? "kdesktop"       --> doIgnore 
    ] 

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.

myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = do
   spawnOnce "nitrogen --restore &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
    handle <- spawnPipe "/home/delta/.config/polybar/launch.sh"
    xmonad $ docks $ ewmh $ def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
      -- key (mouse) bindings
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = myLogHook
    } `additionalKeysP` myKeys
