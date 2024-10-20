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
import XMonad.Util.Run (spawnPipe, runProcessWithInput, safeSpawn)
import XMonad.Util.EZConfig (additionalKeysP)
-- import XMonad.Util.Hacks (windowedFullscreenFixEventHook)

-- Layout
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat

-- Layout modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doRectFloat, doFullFloat)
-- import XMonad.Hooks.StatusBar
import XMonad.Hooks.WindowSwallowing

-- Data 
import Data.Maybe (fromJust)

-- Polybar exclusive
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8


myTerminal :: String
myTerminal = "alacritty"

myLauncher :: String
myLauncher = "rofi -modi combi -show combi -display-combi 'Rufos ~>>' -combi-modi run,drun -show-icons"
-- myLauncher = "exe=`dmenu_path | dmenu` && eval \"exec $exe\""

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 3

-- mod1Mask = left alt | mod3Mask = right alt | mod4Mask = super key
modKey :: KeyMask
modKey = mod4Mask

altKey :: KeyMask
altKey = mod1Mask

-- myWorkspaces = ["web", "irc", "code" ] ++ map show [4..9]
-- myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces = ["α","β","γ","Δ","λ","Κ","η","Ψ","Ω"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor :: String
myNormalBorderColor  = "#232526"

myFocusedBorderColor :: String
myFocusedBorderColor = "#f5c2e7"

scriptPath :: String
scriptPath = "$HOME/.config/xmobar/scripts/"

------------------------------------------------------------------------
-- Cheese... I mean, Keys.
myKeys :: [(String, X())]
myKeys = [

  -- Quit and Restart, in that order
  ("M-C-q", io (exitWith ExitSuccess)),
  ("M-C-r", spawn ("killall xmobar; xmonad --recompile; xmonad --restart")),
  -- ("M-C-r", sequence_ [spawn "killall xmobar; xmonad --recompile; xmonad --restart", spawn (scriptPath ++ "xvolume.sh"), spawn (scriptPath ++ "xbright.sh")]),

  -- Killin
  ("M-S-c", kill),

  -- Common
  ("M-<Return>", spawn (myTerminal)),
  ("M-p"       , spawn (myLauncher)),
  ("M-f"       , spawn ("thunar")),
  ("M-S-s"     , spawn ("flameshot gui")),

  -- Multimedia keys
  ("<XF86AudioRaiseVolume>" , spawn "amixer -D pulse set Master 5%+ unmute > /dev/null"),
  ("<XF86AudioLowerVolume>" , spawn "amixer -D pulse set Master 5%- unmute > /dev/null"),

  -- Brightness
  ("<XF86MonBrightnessUp>"  , spawn "brightnessctl s 5%+"),
  ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-"),

  -- Kbd backlight
  ("<XF86KbdBrightnessUp>"  , spawn "$HOME/.config/scripts/kbdbacklight.sh up"),
  ("<XF86KbdBrightnessDown>", spawn "$HOME/.config/scripts/kbdbacklight.sh down"),

  -- Keyboard layout
  ("M-<Space>", spawn "$HOME/.config/scripts/keyboardChanger.sh"),

  -- Window Navigation
  ("M-<Tab>"  , windows W.focusDown),
  ("M-j"      , windows W.focusDown),
  ("M-k"      , windows W.focusUp),
  -- ("M-<Space>", windows W.focusMaster),
 
  -- Window Swapin
  ("M-S-<Space>", windows W.swapMaster),
  ("M-S-j"      , windows W.swapDown),
  ("M-S-k"      , windows W.swapUp),
 
  -- Window Size Manip
  ("M-S-h", sendMessage Shrink),
  ("M-S-l", sendMessage Expand),
 
  -- Others
  ("M-C-n", refresh), -- Reset window size?
  ("M-S-t", withFocused $ windows . W.sink), -- Force to Tile
 
  ("M1-<Tab>", sendMessage NextLayout), -- Rotate through layouts

  ("M-m", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Full screen
 
  -- Toggle the status bar gap
  -- Use this binding with avoidStruts from Hooks.ManageDocks.
  -- See also the statusBar function from Hooks.DynamicLog.

  -- Hides bar (xmobar) ignores bar (polybar)
  ("M1-S-h", sendMessage ToggleStruts),
  ("M-C-p", spawn "$HOME/.config/polybar/launch.sh") -- Reload polybar

  --  Reset the layouts on the current workspace to default
  -- , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

  ]

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    -- [((m .|. modm, k), windows $ f i)
    --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    -- --
    -- -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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
-- Layouts

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall = renamed [Replace "tall"]
        -- $ smartBorders
        $ mySpacing 4
        $ ResizableTall 1 (3/100) (1/2) []

floats = renamed [Replace "floats"]
        -- $ smartBorders
        $ simplestFloat

myLayoutHook = avoidStruts
        $ T.toggleLayouts floats
        $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
    where 
      myDefaultLayout = withBorder myBorderWidth tall ||| 
                                                 Mirror tall -- Horizontal

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
--
-- myLogHook = return ()

-- Colors for polybar
color1, color2, color3, color4 :: String
color1 = "#7F7F7F"
color2 = "#c792ea"
color3 = "#900000"
color4 = "#2E9AFE"

myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput  = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ color4 ++ "} ") "%{F-}"
    , ppVisible = wrap ("%{F" ++ color1 ++ "} ") "%{F-}"
    , ppUrgent  = wrap ("%{F" ++ color3 ++ "} ") "%{F-}"
    , ppHidden  = wrap ("%{F" ++ color1 ++ "} ") "%{F-}"
    , ppTitle   = wrap ("%{F" ++ color2 ++ "}")"%{F-}"
    , ppSep     = "  |  "
    }

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
myStartupHook :: X ()
myStartupHook = do 
  spawnOnce "redshift -P -O 2800"
  spawnOnce "xwallpaper --zoom $HOME/Pictures/wallpapers/ascalon_aa_aster.png"
  spawnOnce "$HOME/.config/polybar/launch.sh &"

  -- For VM only
  -- spawnOnce "xrandr -s 1920x1080 && sleep 1"
  -- spawnOnce "setxkbmap us intl altGr dead keys"


-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
        D.signalBody = [D.toVariant $ UTF8.decodeString str]
    }
    D.emit dbus signal
    where
        objectPath = D.objectPath_ "/org/xmonad/Log"
        interfaceName = D.interfaceName_ "org.xmonad.Log"
        memberName = D.memberName_ "Update"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. [No] Need to modify this.
main :: IO ()
main = do 
  -- xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
  -- xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    -- The xmonad, ya know...what the window manager is named after.
    xmonad $ ewmhFullscreen . ewmh $ docks $ defaults { logHook = dynamicLogWithPP (myLogHook dbus) }

  -- xmonad $ ewmhFullscreen . ewmh $ docks $ def {
defaults = def {
    -- Simple stuff
    modMask            = modKey,
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    workspaces         = myWorkspaces,
    borderWidth        = myBorderWidth,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key (mouse) bindings
    -- keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayoutHook,
    -- manageHook         = myManageHook <+> (isFullscreen --> doFullFloat),
    manageHook = myManageHook <+> manageHook def,
    handleEventHook = myEventHook,
    startupHook = myStartupHook
    } `additionalKeysP` myKeys
