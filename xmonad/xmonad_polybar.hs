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
import System.IO (hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)

-- Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing

import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarColor, filterOutWsPP, xmobarPP, shorten, wrap, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doRectFloat, doFullFloat)
-- import XMonad.Hooks.StatusBar
import XMonad.Hooks.WindowSwallowing

-- Data 
import Data.Maybe (fromJust)


myTerminal :: String
myTerminal = "alacritty"

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

-- (I think) Useful if you want Xmobar to be clickable (needs clickable)
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

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
  ("M-p"       , spawn ("exe=`dmenu_path | dmenu` && eval \"exec $exe\"")),
  ("M-b"       , spawn ("brave --password-store=kwallet5")),
  ("M-S-b"     , spawn ("chromium")),
  ("M-f"       , spawn ("pcmanfm")),
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

  ("M-C-m", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Full screen
 
  -- Toggle the status bar gap
  -- Use this binding with avoidStruts from Hooks.ManageDocks.
  -- See also the statusBar function from Hooks.DynamicLog.
  ("M1-S-h", sendMessage ToggleStruts) -- Hides bar

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
        $ smartBorders
        $ mySpacing 4
        $ ResizableTall 1 (3/100) (1/2) []

floats = renamed [Replace "floats"]
        $ smartBorders
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
  spawnOnce "xrandr -s 1920x1080 && sleep 1"
  spawnOnce "xwallpaper --zoom $HOME/Pictures/evernix.png"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. [No] Need to modify this.

main :: IO ()
main = do 
  -- xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
  -- xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"

  handle <- spawnPipe "/home/delta/.config/polybar/launch.sh"

  xmonad $ ewmhFullscreen . ewmh $ docks $ def {
    -- Simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = modKey,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key (mouse) bindings
    -- keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayoutHook,
    manageHook         = myManageHook <+> (isFullscreen --> doFullFloat),
    handleEventHook    = myEventHook,
    startupHook        = myStartupHook,
    logHook = dynamicLogWithPP $ xmobarPP {
      -- ppOutput = \x -> hPutStrLn xmproc0 x
                    -- >> hPutStrLn xmproc1 x
     ppCurrent         = xmobarColor "#45D0FE" "" . wrap "[" "]",
      ppVisible         = xmobarColor "#81c19b" "" . wrap "<" ">",
      ppHidden          = xmobarColor "#bc83e3" "" . wrap "*" "" . clickable,
      ppHiddenNoWindows = xmobarColor "#b3afc2" "" . clickable,
      ppTitle           = xmobarColor "#b3afc2" "" . shorten 60,
      ppSep             = "<fc=#f2f4f5> <fn=1>|</fn> </fc>",
      ppUrgent          = xmobarColor "#e8646a" "" . wrap "!" "!",
      -- ppExtras          = [windowCount],
      ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    }
} `additionalKeysP` myKeys
