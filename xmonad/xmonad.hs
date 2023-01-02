--             __  __                       _   _                                           _     _ 
--     /\     |  \/  |                     | | (_)                                         (_)   | |
--    /  \    | \  / | ___  _ __   __ _  __| |  _ ___    __ _   _ __ ___   ___  _ __   ___  _  __| |
--   / /\ \   | |\/| |/ _ \| '_ \ / _` |/ _` | | / __|  / _` | | '_ ` _ \ / _ \| '_ \ / _ \| |/ _` |
--  / ____ \  | |  | | (_) | | | | (_| | (_| | | \__ \ | (_| | | | | | | | (_) | | | | (_) | | (_| |
-- /_/    \_\ |_|  |_|\___/|_| |_|\__,_|\__,_| |_|___/  \__,_| |_| |_| |_|\___/|_| |_|\___/|_|\__,_|
--                                                                                                  
-- DeltΔ's config file.
--

 -- Base
import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

 -- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad as NS
import XMonad.Util.EZConfig (additionalKeysP)

 -- Layout
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutModifier

import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

 -- Hooks
-- import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, PP(..))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doRectFloat)
import XMonad.Hooks.EwmhDesktops -- Useful for Polybar!
import XMonad.Hooks.WindowSwallowing

myTerminal :: String
myTerminal = "kitty"

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

-- (I think) Useful if you want Xmobar to be clickable (needs clickable)
-- myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

myNormalBorderColor :: String
myNormalBorderColor  = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#00ff00"

------------------------------------------------------------------------
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
        ("M-d"        , spawn ("rofi -show run -display-run 'Rufos ~>>'")),
        ("M-b"        , spawn ("brave")),
        ("M-f"        , spawn ("thunar")),
        -- ("Print"      , spawn ("flameshot gui")),

      -- Scratchpads (WITH KEYCHORDS: Alt+s key) (ye)
        ("M1-s t", namedScratchpadAction myScratchPads "terminal"),
        ("M1-s c", namedScratchpadAction myScratchPads "calculator"),
        
      -- Window swapin
        ("M-S-<Space>", windows W.swapMaster),
        ("M-S-j"      , windows W.swapDown),
        ("M-S-k"      , windows W.swapUp),

      -- Window navigation
        ("M-<Tab>"    , windows W.focusDown),
        ("M-j"        , windows W.focusDown),
        ("M-k"        , windows W.focusUp),
        -- ("M-<Space>"  , windows W.focusMaster),

      -- Window size modification
        ("M-S-h"      , sendMessage Shrink),
        ("M-S-l"      , sendMessage Expand),

      -- Refresh size | Force to Tile
        ("M-C-n"      , refresh), -- Refresh w size
        ("M-S-t"      , withFocused $ windows . W.sink),

        ("M-C-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts),

      -- Unused / useless functionalities
--      -- Increment | Deincrement number of windows in master
--        ("M-,"        , sendMessage (IncMasterN 1)),
--        ("M-."        , sendMessage (IncMasterN (-1))),

      -- Layout switching | Hide bar 
        ("M-<Space>"  , sendMessage NextLayout),
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
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
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
-- Layouts
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall = renamed [Replace "tall"]
        $ smartBorders
        $ mySpacing 6
        $ ResizableTall 1 (3/100) (1/2) []

floats = renamed [Replace "floats"]
        $ smartBorders
        $ simplestFloat

myLayoutHook = avoidStruts
           $ T.toggleLayouts floats
           $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
    where
        myDefaultLayout = withBorder myBorderWidth tall |||
                                                   Mirror tall -- Horizontal like

------------------------------------------------------------------------
-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [
        NS "terminal" spawnTerm findTerm manageTerm,
        NS "calculator" spawnCalc findCalc manageCalc
    ]
    where
        spawnTerm  = myTerminal ++ " -T scratchKitty"
        findTerm   = title =? "scratchKitty"
        manageTerm = customFloating $ W.RationalRect x y w h
            where
                x = 0.9 -w 
                y = 0.62 -h
                w = 0.8
                h = 0.6

        spawnCalc  = "galculator"
        findCalc   = title =? "galculator"
        manageCalc = customFloating $ W.RationalRect x y w h
            where
                x = 0.87 -w
                y = 0.62 -h
                w = 0.75
                h = 0.6

------------------------------------------------------------------------
-- Window rules
myManageHook = composeAll [ 
    className =? "MPlayer"        --> doFloat, 
    className =? "Gimp"           --> doFloat,
    -- appName   =? "galculator"     --> doRectFloat(W.RationalRect 0.12 0.15 0.75 0.55), -- x y w h
    resource  =? "desktop_window" --> doIgnore,
    resource  =? "kdesktop"       --> doIgnore 
    ] <+> namedScratchpadManageHook myScratchPads

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
myEventHook = swallowEventHook (className =? "kitty" <||> className =? "alacritty") (return True)

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
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = myLogHook
    } `additionalKeysP` myKeys
